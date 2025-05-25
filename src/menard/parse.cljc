(ns menard.parse
  (:require
   [clojure.set :refer [union]]
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [menard.log :as log])
   [dag_unify.core :as u]
   [dag_unify.diagnostics :as diag]
   [dag_unify.serialization :refer [serialize]]
   [menard.exception :refer [exception]]
   [menard.lexiconfn :as l]
   [menard.parse.word :as word]
   [menard.serialization :as s]))

(def developer-mode? false)

;; <performance-optimized defaults>
(def enable-pmap? true)
(def take-this-many 300)
(def summary-filter? true)
(def log-these-rules-as-parents #{})

;; Put names of rules you want to debug in these sets:
;; To debug rules, you must also set developer-mode? to true (above).
(def log-these-rules-as-parents #{})
(def log-these-rules-as-head-children #{})
(def log-these-rules-as-comp-children #{})

;; examples:
;;
;;(def log-these-rules-as-parents #{"adj-p" "vp-inf"})
;;(def log-these-rules-as-head-children #{"vp-inf"})
;;(def log-these-rules-as-comp-children #{"vp-inf"})
(def over-compact? true)
;; </performance-optimized defaults>

;; <developer-mode overrides of performance-optimized defaults>
(when developer-mode?
  (def enable-pmap? false)
  (def summary-filter? false)
  (def over-compact? false)

  ;; modify these to log specific parse rules e.g.:
  ;;
  ;; (def log-these-rules-as-parents #{"s"})
  ;; (def log-these-rules-as-head-children #{"vp"})  
  
  )
;; </developer-mode overrides of performance-optimized defaults>

(def fail-counter 0)
(def succeed-counter 0)

;; a token can be max 7 words, e.g. "presidents of the united states of america".
(def max-token-length-in-words 7)

(defn pmap-if-available [fn args]
  #?(:clj
     ;; map is slower (no concurrency) but better for debugging since you can see the
     ;; log messages for a particular function call in order.
     (if enable-pmap?
       (pmap fn args)
       (map fn args)))
  #?(:cljs
     (map fn args)))

(defn mapcat-lazy-seq
  "Apply _f_ to each meber of coll and concatenate the results using lazy-cat.
  Prevents normal clojure behaviour of chunking from happening.
  Inspired thanks to https://gist.github.com/enforser/f43e42a803ca8c351daa4aba079955b4#file-lazy-side-effects-clj-L45"
  [f coll]
  (when (not-empty coll)
    (lazy-seq (lazy-cat (f (first coll))
                        (mapcat-lazy-seq f (rest coll))))))

(defn fail-path [dag1 dag2]
  (cond (or (not (map? dag1))
            (not (map? dag2)))
        []
        :else
        (let [keys (seq (set (concat (keys dag1) (keys dag2))))]
          (loop [kvs []
                 keys keys]
            (if (seq keys)
              (let [k (first keys)
                    v (dag_unify.core/unify (k dag1 :top)
                                            (k dag2 :top))]
                (cond
                  (= :fail v)
                  (do
                    (log/debug (str "fail-key: (1) " k " between: "
                                    (u/pprint (u/get-in dag1 [k] :top))
                                    " and "
                                    (u/pprint (u/get-in dag2 [k] :top))))
                    (cons k (fail-path (u/get-in dag1 [k] :top)
                                       (u/get-in dag2 [k] :top))))
                  (and (dag_unify.core/ref? v) (= :fail @v))
                  (do
                    (log/debug (str "fail-key: (2) " k " between: "                   
                                    (u/pprint (u/get-in dag1 [k] :top))
                                    " and "
                                    (u/pprint (u/get-in dag2 [k] :top))))
                    (cons k (fail-path (u/get-in dag1 [k] :top)
                                       (u/get-in dag2 [k] :top))))
                  :else
                  (recur [] (rest keys))))
              kvs)))))

(defn summary [dag]
  {:agr (u/get-in dag [:agr] :top)
   :cat (u/get-in dag [:cat] :top)
   :subcat (u/get-in dag [:subcat] :top)
   :phrasal? (u/get-in dag [:phrasal?] :top)})

(defn overh-compact
  "add given head as the head child of the phrase: parent. like overh but shorter."
  [parent head syntax-tree]
  {:pre [(map? parent)
         (map? head)]}
  (u/unify parent
           {:head head}))

(defn overh-full
  "add given head as the head child of the phrase: parent."
  [parent head syntax-tree]
  {:pre [(map? parent)
         (map? head)]}
  (let [log-this? (and (contains? log-these-rules-as-parents (u/get-in parent [:rule]))
                       (or
                        (contains? log-these-rules-as-head-children (u/get-in head [:rule]))
                        (false? (u/get-in head [:phrasal?]))))]
    (when log-this?
      (log/debug (str "overh attempt: " (syntax-tree parent) " <- " (syntax-tree head))))
    (let [pre-check? true
          result (cond pre-check?
                       (u/unify parent
                                {:head head})
                       :else
                       (do
                         (when log-this?
                           (log/info (str "failed precheck: parent: " (syntax-tree
                                                                       parent) "; head: " (syntax-tree head) "; "
                                          "parent [:head :cat]=" (u/get-in parent [:head :cat]) "; head [:cat]=" (u/get-in head [:cat]))))
                         :fail))]
      (if (not (= :fail result))
        ;; not :fail:
        (do
          (def succeed-counter (+ 1 succeed-counter))
          (when log-this?
            (log/info (str "overh success: " (syntax-tree parent) " -> " (syntax-tree result)))))

        ;; :fail:
        (do
          (when pre-check?
            (def fail-counter (+ 1 fail-counter))
            (when log-this?
              (let [fp (fail-path parent {:head head})]
                (log/info
                 (str "overh fail:    " (syntax-tree parent)
                      " <- " (syntax-tree head)
                      " fail-path: " (vec fp)
                      ". Parent wants: " (l/pprint (u/get-in parent fp))
                      ", but head has: (" (vec (rest fp)) ")" (l/pprint (u/get-in head (rest fp)))
                      (let [derivation (cond
                                         (seq (u/get-in head [:menard.lexiconfn/derivation]))
                                         (u/get-in head [:menard.lexiconfn/derivation])
                                         (keyword? (u/get-in head [:head-derivation]))
                                         nil
                                         (seq (u/get-in head [:head-derivation]))
                                         (u/get-in head [:head-derivation])
                                         :else nil)
                            derivation (l/encode-derivation derivation)]
                        (if (seq derivation)
                          (str ". Head derivation: " derivation)))
                      ".")))))))
      result)))

(defn overh [parent head syntax-tree]
  {:pre [(map? parent)
         (map? head)]}
  (if over-compact?
    (overh-compact parent head syntax-tree)
    (overh-full parent head syntax-tree)))

(defn overc-compact
  "add given child as the complement of the parent. like overc, but shorter."
  [parent comp syntax-tree]
  {:pre [(map? comp)]}
  (u/unify! (u/copy parent)
            {:comp (u/copy comp)}))

(defn overc-full
  "add given child as the complement of the parent"
  [parent comp syntax-tree]
  {:pre [(map? comp)]}
  (let [log-this? (and (contains? log-these-rules-as-parents (u/get-in parent [:rule]))
                       (or
                        (contains? log-these-rules-as-comp-children (u/get-in comp [:rule]))
                        (false? (u/get-in comp [:phrasal?]))))]
    (when log-this?
      (log/info (str "overc attempt: " (syntax-tree parent) " <- " (syntax-tree comp))))
    (let [pre-check? true
          result
          (cond pre-check?
                (u/unify! (u/copy parent)
                          {:comp (u/copy comp)})
                :else :fail)]
      (if (not (= :fail result))
        (do
          (def succeed-counter (+ 1 succeed-counter))
          (when log-this?
            (log/info (str "overc success: " (syntax-tree parent) " -> " (syntax-tree result)))))
        (do
          (when pre-check?
            (def fail-counter (+ 1 fail-counter))
            (when log-this?
              (let [fp (fail-path parent {:comp comp})]
                (log/info
                 (str "overc fail: " (syntax-tree parent)
                      " <- " (syntax-tree comp)
                      " fail path: " (vec fp)
                      ". parent wants: " (l/pprint (u/get-in parent fp))
                      ", but comp has: " (l/pprint (u/get-in comp (rest fp)))
                      ", head derivation: " (l/encode-derivation (or (u/get-in parent [:head :menard.lexiconfn/derivation])))
                      ", comp derivation: " (l/encode-derivation (or (u/get-in comp [:menard.lexiconfn/derivation])))                      
                      ".")))))))
      result)))

(defn overc [parent comp syntax-tree]
  (if over-compact?
    (overc-compact parent comp syntax-tree)
    (overc-full parent comp syntax-tree)))

(defn truncate [tree syntax-tree morph]
  (log/debug (str "truncating tree: " (syntax-tree tree)))
  (-> tree
      (assoc :syntax-tree (syntax-tree tree))
      (assoc :surface (morph tree))
      (dissoc :head)
      (dissoc :comp)
      (dissoc :1)
      (dissoc :2)))

(defn over [parents left-children right-children syntax-tree morph truncate?]
  (let [log-this? false]
    (->>
     parents

     (pmap-if-available
      (fn [parent]
        (let [[head-children comp-children] (if (= (:1 parent) (:head parent))
                                              [left-children right-children]
                                              [right-children left-children])
              head-summary (u/get-in parent [:head])
              comp-summary (u/get-in parent [:comp])]
          (->>
           head-children

           (filter (fn [head-child]
                     (or (false? summary-filter?)
                         (not (= :fail
                                 (u/unify head-summary
                                          (summary head-child)))))))

           (map (fn [head-child]
                  (let [parent-with-head
                        (overh parent head-child syntax-tree)
                        log-this? (and (contains? log-these-rules-as-parents (u/get-in parent [:rule]))
                                       (or
                                        (contains? log-these-rules-as-head-children (u/get-in head-child [:rule]))
                                        (false? (u/get-in head-child [:phrasal?]))))]
                    (if (= parent-with-head :fail)
                      []
                      (->> comp-children
                           (filter (fn [comp-child]
                                     (when log-this? (log/info (str "over: parent-with-head: " (syntax-tree parent-with-head) " looking at candidate comp-child: " (syntax-tree comp-child))))
                                     (let [check-unify (u/unify comp-summary (summary comp-child))]
                                       (when log-this?
                                         (log/info (str "   check-unify: " (l/pprint check-unify)))
                                         (when (= :fail check-unify)
                                           (let [fp (fail-path comp-summary (summary comp-child))]
                                             (log/info (str "    fail-path: " fp))
                                             (log/info (str "    parent wants: " (l/pprint (u/get-in comp-summary fp))))
                                             (log/info (str "    child has: " (l/pprint (u/get-in (summary comp-child) fp)))))))
                                       (not (= :fail check-unify)))))
                           (map (fn [comp-child]
                                  (when log-this? (log/info (str "over: summary check succeeded: calling overc: parent-with-head: " (syntax-tree parent-with-head) " <- comp: " (syntax-tree comp-child))))
                                  (overc parent-with-head comp-child syntax-tree)))
                           (remove #(= :fail %)))))))))))

     flatten

     (remove #(= :fail %))

     (map (fn [tree]
            (if truncate?
              (truncate tree syntax-tree morph)
              tree))))))

(defn span-pairs [i n]
  (cond (= i 0)
        (->> (range 0 (- n 1))
             (map (fn [x] [[0 (+ 1 x)][(+ 1 x) n]])))
        true
        (lazy-cat
         (span-pairs (- i 1) n)
         (->> (range i (+ i (- n 1)))
              (map (fn [x] [[i (+ 1 x)][(+ 1 x) (+ i n)]]))))))

(def split-on #"[ ]")

(defn parse-spans-of-length
  "Get all parses for length _span-length_, given :
   - _input_map_, a map from pairs [i,j] => _parses_,
     where i and j are the left and right coordinates within the input string, and _parses_
     are all the parses for the substring of tokens [t_i....t_j].
   - _input_length_, the length of the input string in tokens.
   - _grammar_, a list of grammar rules."
  [input-map input-length span-length grammar syntax-tree morph truncate?]
  (cond (> span-length input-length)
        ;; done
        input-map
        true
        (merge input-map
               (->> (span-pairs (- input-length span-length) span-length)
                    (pmap-if-available
                     (fn [[[left middle][middle right]]]
                       (let [all-results (over grammar
                                               (get input-map [left middle])
                                               (get input-map [middle right])
                                               syntax-tree
                                               morph
                                               truncate?)
                             taken-results (take take-this-many all-results)
                             taken-plus-one-results (take (+ 1 take-this-many) all-results)]
                         (when (> (count taken-plus-one-results) (count taken-results))
                           (log/warn (str "more than " take-this-many " parses for: '"
                                          (morph (first taken-results)) "' ; first: "
                                          (syntax-tree (first taken-results)))))
                         {[left right] taken-results})))
                    (reduce (fn [a b]
                              (merge-with
                               (fn [a b] (lazy-cat a b))
                               a b)))))))

(defn lookup-fn-with-trim [string lookup-fn]
  (let [trimmed (clojure.string/trim string)]
    (when (and (not (string/blank? trimmed))
               (= trimmed string))
      (lookup-fn string))))

(defn create-input-map [tokens lookup-fn]
  (into {}
        (map (fn [i]
               [[i (+ i 1)]
                (lookup-fn-with-trim (nth tokens i) lookup-fn)])
             (range 0 (count tokens)))))

(defn tokenize
  [input split-on analyze-fn]
  (word/groupings input split-on analyze-fn max-token-length-in-words))

(defn parse-start
  [input split-on analyze-fn]
  (log/debug (str "parse-start input: " input))
  (log/debug (str "parse-start analyze-fn: " analyze-fn))
  (->> (tokenize input split-on analyze-fn)
       (map (fn [tokenization]
              (log/debug (str "looking at tokenization: " (vec tokenization)))
              (create-input-map tokenization analyze-fn)))))

(defn parse-in-stages [input-map input-length i grammar syntax-tree morph truncate?]
  (log/debug (str "parse-in-stages with truncate? " truncate?))
  (if (or (get input-map [0 input-length])
          (> i input-length))
    input-map
    (-> input-map
        (parse-spans-of-length input-length i grammar syntax-tree morph truncate?)
        (parse-in-stages input-length (+ 1 i) grammar syntax-tree morph truncate?))))

;; TODO: move analyze to its own namespace (menard.analyze)
(declare analyze)

(defn parse-tokenization
  "Return all the possible parses given:
     1. the _tokenization_, a list of tokens,
     1. the _lookup-fn_, which gives a set of lexemes for the token,
     2. the grammar."
  [tokenization grammar lookup-fn syntax-tree morph truncate?]
  (log/debug (str "looking at tokenization: " (vec tokenization)))
  (let [token-count (count tokenization)
        all-parses (reduce (fn [input-map span-size]
                             (parse-spans-of-length input-map token-count span-size grammar syntax-tree morph truncate?))
                           (create-input-map tokenization lookup-fn)
                           (range 2 (+ 1 token-count)))
        result {:token-count (count tokenization)
                :complete-parses (->> (-> all-parses
                                          (get [0 (count tokenization)]))
                                      (filter map?)
                                      (map (fn [x]
                                             (merge x
                                                    {:complete? true}))))
                :all-parses all-parses}]
    (if (seq (:complete-parses result))
      (do
        (log/debug (str "found some complete results; first: " (syntax-tree (first (:complete-parses result)))))
        (log/debug (str " total results for this tokenization: " (count (:complete-parses result))))
        (:complete-parses result))

      ;; if there are no complete parses,
      ;; cobble together results by combining
      ;; partial parses with lexical lookups of tokens (if they exists).
      ;; e.g. we can parse "er zijn katten" so there is a complete parse
      ;; but "er zijn kat" can't be fully parsed, so we return:
      ;; [er zijn] [kat].
      (do
        (log/debug (str "partial result: " result))
        (->> result
             :all-parses
             vals
             flatten)))))

(defn grouping-covers-input? [grouping expression]
  (= (clojure.string/join " " grouping) expression))

(defn parse
  "Return a list of all possible parse trees given all possible tokenizations."
  [expression grammar lookup-fn syntax-tree morph split-on truncate?]
  (->>
   (word/groupings expression split-on lookup-fn max-token-length-in-words)
   (filter #(grouping-covers-input? % expression))
   (mapcat-lazy-seq #(parse-tokenization % grammar lookup-fn syntax-tree morph truncate?))))

(defn parse-all [expression grammar syntax-tree-fn split-on analyze-fn morph-fn truncate?]
  (let [;; remove trailing '.' if any:
        expression (string/replace expression #"[.]*$" "")]
        ;; ^ TODO: should handle '.' and other punctuation like '?' '!' and
        ;; use it as part of the meaning
        ;; i.e.
        ;; '.' -> declarative
        ;; '?' -> interrogative
        ;; '!' -> imperative
    (let [input-map (parse-start expression analyze-fn)]
      (-> input-map
          (parse-in-stages (count (keys input-map)) 2 grammar syntax-tree-fn truncate?)
          ((fn [m]
             {[0 (count (keys input-map))]
              (get m [0 (count (keys input-map))])}))))))

