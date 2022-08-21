(ns menard.parse
  (:require
   [clojure.set :refer [union]]
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [menard.log :as log])
   [dag_unify.core :as u]
   [dag_unify.diagnostics :as diag]
   [dag_unify.serialization :refer [serialize]]
   [menard.lexiconfn :as l]))

(def parse-only-one? false)

(def ^:dynamic lookup-fn
  (fn [_]
    []))
(def ^:dynamic grammar nil)
(def ^:dynamic syntax-tree (fn [x] (log/warn (str "'syntax-tree' was not bound."))))
(def ^:dynamic morph (fn [x] (log/warn (str "'morph' was not bound."))))
(def ^:dynamic truncate? false)
(declare truncate)
(def ^:dynamic split-on #"[ ']")
(def ^:dynamic take-this-many 30)
(def ^:dynamic debug-rule-for-comp nil)
(def ^:dynamic enable-pmap? true)

(defn pmap-if-available [fn args]
  #?(:clj
     ;; map is slower (no concurrency) but better for debugging since you can see the
     ;; log messages for a particular function call in order.
     (if enable-pmap?
       (pmap fn args)
       (map fn args)))
  #?(:cljs
     (map fn args)))

(def ^:dynamic log-these-rules #{})

(defn fail-path [dag1 dag2]
  (cond (or (not (map? dag1))
            (not (map? dag2)))
        []
        :else 
        (let [keys (vec (set (concat (keys dag1) (keys dag2))))]
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

(defn overh
  "add given head as the head child of the phrase: parent."
  [parent head]
  {:pre [(map? parent)
         (map? head)]
   :post [(vector? %)]}
  (when (contains? log-these-rules (u/get-in parent [:rule]))
    (log/info (str "overh attempting: " (syntax-tree parent) " <- " (syntax-tree head))))
 (let [pre-check? (not (= :fail
                           (u/get-in parent [:head :cat] :top)
                           (u/get-in head [:cat] :top)))
        result (cond pre-check?
                     (u/unify parent
                              {:head head})
                     :else
                     (do
                       (log/debug (str "failed precheck: parent: " (syntax-tree
                                                                    parent) "; head: " (syntax-tree head) "; "
                                       "parent [:head :cat]=" (u/get-in parent [:head :cat]) "; head [:cat]=" (u/get-in head [:cat])))
                       :fail))]
   (if (not (= :fail result))
     (do
       (when (contains? log-these-rules (u/get-in parent [:rule]))
         (log/info (str "overh success: " (syntax-tree parent) " -> " (syntax-tree result))))
       [result])
     (do
       (when (contains? log-these-rules (u/get-in parent [:rule]))
         (let [fp (fail-path parent {:head head})]
           (log/info
            (str "overh fail: " (syntax-tree parent)
                 " <- " (syntax-tree head)
                 " fail-path: " (vec fp)
                 ". parent has: " (u/pprint (u/get-in parent fp))
                 ", but head has: " (u/pprint (u/get-in head (rest fp)))
                 (if (:menard.lexiconfn/derivation head)
                   (str " head derivation: " (u/get-in head [:menard.lexiconfn/derivation])))
                 "."))))
        []))))

(defn overc
  "add given child as the complement of the parent"
  [parent comp]
  {:pre [(map? comp)
         (map? parent)]
   :post [(vector? %)]}
  (when (contains? log-these-rules (u/get-in parent [:rule]))
    (log/info (str "overc attempting: " (syntax-tree parent) " <- " (syntax-tree comp))))
  (let [pre-check? (not (= :fail (u/unify
                                  (u/get-in parent [:comp :cat] :top)
                                  (u/get-in comp [:cat] :top))))
        result
        (cond pre-check?
              (u/unify! (u/copy parent)
                        {:comp (u/copy comp)})
              :else :fail)]
    (if (not (= :fail result))
      (do
        (when (contains? log-these-rules (u/get-in parent [:rule]))
          (log/info (str "overc success: " (syntax-tree parent) " -> " (syntax-tree result))))
        [result])
      (do
        (when (contains? log-these-rules (u/get-in parent [:rule]))
          (let [fp (fail-path parent {:comp comp})]
            (log/info
             (str "overc fail: " (syntax-tree parent)
                  " <- " (syntax-tree comp)
                  " fail path: " (vec fp)
                  ". parent has: " (u/pprint (u/get-in parent fp))
                  ", but comp has: " (u/pprint (u/get-in comp (rest fp)))
                  "."))))
        []))))

(defn truncate [tree]
  (log/debug (str "truncating tree: " (syntax-tree tree)))
  (log/debug   (str "truncating:    " (syntax-tree tree)))
  (-> tree
      (assoc :syntax-tree (syntax-tree tree))
      (assoc :surface (morph tree))
      (dissoc :head)
      (dissoc :comp)
      (dissoc :1)
      (dissoc :2)))

(def ^:dynamic truncate-fn truncate)

(defn over [parents left-children right-children]
  (->>
   parents
   (pmap-if-available
    (fn [parent]
      (let [[head-children comp-children] (if (= (:1 parent) (:head parent))
                                            [left-children right-children]
                                            [right-children left-children])]
        (mapcat (fn [head-child]
                  (-> parent
                      (overh head-child)
                      ((fn [parents-with-head]
                         (mapcat (fn [comp-child]
                                   (mapcat (fn [parent-with-head]
                                             (overc parent-with-head comp-child))
                                           parents-with-head))
                                 comp-children)))))
                head-children))))
   (reduce
    (fn [a b]
      (lazy-cat a b)))

   (map (fn [tree]
          (if truncate?
            (truncate-fn tree)
            tree)))))

(defn summary
  "for diagnostic logging"
  [m]
  (into {}
        (->> (keys m)
             (map (fn [k]
                    {k (map syntax-tree (get m k))})))))

(defn span-pairs [i n]
  (cond (= i 0)
        (->> (range 0 (- n 1))
             (map (fn [x] [[0 (+ 1 x)][(+ 1 x) n]])))
        true
        (concat
         (span-pairs (- i 1) n)
         (->> (range i (+ i (- n 1)))
              (map (fn [x] [[i (+ 1 x)][(+ 1 x) (+ i n)]]))))))

(defn parse-spans-of-length
  "Get all parses for length _span-length_, given :
   - _input_map_, a map from pairs [i,j] => _parses_,
     where i and j are the left and right coordinates within the input string, and _parses_
     are all the parses for the substring of tokens [w_i....w_j].
   - _input_length_, the length of the input string in tokens.
   - _grammar_, a list of grammar rules."
  [input-map input-length span-length grammar]
  (cond (> span-length input-length)
        ;; done
        input-map
        true
        (do
          (log/debug (str "span-pairs: " (- input-length span-length) "," span-length))
          (merge input-map
                 (->> (span-pairs (- input-length span-length) span-length)
                      (pmap-if-available
                       (fn [[[left middle][middle right]]]
                         (let [all-results (over grammar
                                                 (get input-map [left middle])
                                                 (get input-map [middle right]))
                               taken-results (take take-this-many all-results)
                               taken-plus-one-results (take (+ 1 take-this-many) all-results)]
                           (when (> (count taken-plus-one-results) (count taken-results))
                             (log/warn (str "more than " take-this-many " parses for: '"
                                            (morph (first taken-results)) "' ; first: "
                                            (syntax-tree (first taken-results)))))
                           {[left right] taken-results})))
                      (reduce (fn [a b]
                                (merge-with concat a b))))))))

(defn lookup-fn-with-trim [string]
  (let [trimmed (clojure.string/trim string)]
    (when (and (not (string/blank? trimmed))
               (= trimmed string))
      (lookup-fn string))))

(defn create-input-map [tokens]
  (into {}
        (map (fn [i]
               [[i (+ i 1)]
                (lookup-fn-with-trim (nth tokens i))])
             (range 0 (count tokens)))))
  
;; TODO: should create all possible tokenizations.
;; (in other words, more than one tokenization is possible, e.g.
;;  if a token is made of separate words like "The White House".
(defn tokenize [input]
  (filter #(not (string/blank? %)) (string/split input split-on)))

(defn parse-start
  [input]
  (create-input-map (tokenize input)))

(defn parse-in-stages [input-map input-length i grammar surface]
  (if (or (get input-map [0 input-length])
          (> i input-length))
    input-map
    (-> input-map
        (parse-spans-of-length input-length i grammar)
        (parse-in-stages input-length (+ 1 i) grammar surface))))

(defn parse
  "Return a list of all possible parse trees for a string.
   Use a language-independent tokenizer (split on space and
  apostrophe) to turn the string into a sequence of tokens."
  ;; TODO: remove 'morph' as an input parameter; use a dynamic binding instead.
  [input]
  (log/debug (str "parsing input: '" input "'"))
  ;; TODO: should create all possible tokenizations.
  ;; (in other words, more than one tokenization is possible, e.g.
  ;;  if a token is made of separate words like "The White House".
  (let [tokenization (tokenize input)
        token-count (count tokenization)
        all-parses (reduce (fn [input-map span-size]
                             (parse-spans-of-length input-map token-count span-size grammar))
                           (create-input-map tokenization)
                           (range 2 (+ 1 token-count)))
        result {:token-count (count tokenization)
                :complete-parses
                (filter map? (get all-parses
                                  [0 (count tokenization)]))
                :all-parses all-parses}]
    (if (empty? (:complete-parses result))

      ;; if there are no complete parses,
      ;; cobble together results by combining
      ;; partial parses with lexical lookups of tokens (if they exists).
      ;; e.g. we can parse "er zijn katten" so there is a complete parse
      ;; but "er zijn kat" can't be fully parsed, so we return:
      ;; [er zijn] [kat].
      (let [analyses
            (zipmap
             tokenization
             (pmap-if-available
              (fn [token]
                (lookup-fn-with-trim token))
              tokenization))
            partial-parses (vals (:all-parses result))]
        (log/info (str "could not parse: \"" input "\". token:sense pairs: "
                       (string/join ";"
                                    (pmap-if-available (fn [token]
                                           (str token ":" (count (get analyses token)) ""))
                                         tokenization))
                       (str "; partial parses: " (count (mapcat (fn [parses-for-span]
                                                                  (pmap-if-available syntax-tree parses-for-span))
                                                                partial-parses)) ".")))
        (->> (flatten partial-parses)
             (map (fn [partial-parse]
                    (merge partial-parse {::partial? true})))))
      (do (log/debug (str "parsed input:    \"" input "\""))
          (:complete-parses result)))))

(defn strip-map [m]
  (select-keys m [:1 :2 :canonical :left-is-head? :rule :surface]))

(defn parse-all [expression load-model-fn syntax-tree-fn analyze-fn]
  (let [model (load-model-fn)

        ;; remove trailing '.' if any:
        expression (string/replace expression #"[.]*$" "")]
        ;; ^ TODO: should handle '.' and other punctuation like '?' '!' and
        ;; use it as part of the meaning
        ;; i.e.
        ;; '.' -> declarative
        ;; '?' -> interrogative
        ;; '!' -> imperative
    (binding [l/morphology (-> model :morphology)
              split-on #"[ ]"
              log-these-rules log-these-rules
              lookup-fn analyze-fn
              truncate? true
              truncate-fn
              (fn [tree]
                (let [left-is-head? (= (get tree :1) (get tree :head))]
                  (-> tree
                      (dissoc :head)
                      (dissoc :comp)
                      (assoc :left-is-head? left-is-head?)
                      (assoc :1 (strip-map (u/get-in tree [:1])))
                      (assoc :2 (strip-map (u/get-in tree [:2]))))))
              syntax-tree syntax-tree-fn]
      (let [input-map (parse-start expression)]
        (-> input-map
            (parse-in-stages (count (keys input-map)) 2 (-> model :grammar) expression)
            ((fn [m]
               {[0 (count (keys input-map))]
                (get m [0 (count (keys input-map))])})))))))
