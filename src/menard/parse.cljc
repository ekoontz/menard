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
   [menard.lexiconfn :as l]))

(def parse-only-one? false)
(def enable-pmap? true)
(def take-this-many 30)

;; modify this to log specific parse rules e.g.
;; (def log-these-rules #{"np:1" "np:2" "nbar"}).
(def log-these-rules #{})

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
  
(defn pre-check [child-spec child]
  (let [child-spec-summary (summary child-spec)
        child-summary (summary child)]
    (u/unify child-spec-summary child-summary)))

(defn overh-compact
  "add given head as the head child of the phrase: parent. like overh but shorter."
  [parent head syntax-tree]
  {:pre [(map? parent)
         (map? head)]}
  (u/unify parent
           {:head head}))

(defn overh
  "add given head as the head child of the phrase: parent."
  [parent head syntax-tree]
  {:pre [(map? parent)
         (map? head)]}
  (when (contains? log-these-rules (u/get-in parent [:rule]))
    (log/info (str "overh attempting: " (syntax-tree parent) " <- " (syntax-tree head))))
  (let [pre-check? true
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
      ;; not :fail: 
      (do
        (def succeed-counter (+ 1 succeed-counter))
        (when (contains? log-these-rules (u/get-in parent [:rule]))     
          (log/info (str "overh success: " (syntax-tree parent) " -> " (syntax-tree result)))))
      
      ;; :fail:
      (do
        (when pre-check?
          (def fail-counter (+ 1 fail-counter))
          (when (and true (contains? log-these-rules (u/get-in parent [:rule])))
            (let [fp (fail-path parent {:head head})]
              (log/info
             (str "overh fail: " (syntax-tree parent)
                  " <- " (syntax-tree head)
                  " fail-path: " (vec fp)
                  ". parent has: " (u/pprint (u/get-in parent fp))
                  ", but head has: " (u/pprint (u/get-in head (rest fp)))
                  (if (:menard.lexiconfn/derivation head)
                    (str " head derivation: " (u/get-in head [:menard.lexiconfn/derivation])))
                  ".")))))))
    result))

(defn overc-compact
  "add given child as the complement of the parent. like overc, but shorter."
  [parent comp syntax-tree]
  {:pre [(map? comp)]}
  (u/unify! (u/copy parent)
            {:comp (u/copy comp)}))

(defn overc
  "add given child as the complement of the parent"
  [parent comp syntax-tree]
  {:pre [(map? comp)]}
  (when (contains? log-these-rules (u/get-in parent [:rule]))
    (log/info (str "overc attempting: " (syntax-tree parent) " <- " (syntax-tree comp))))
  (let [pre-check? true
        result
        (cond pre-check?
              (u/unify! (u/copy parent)
                        {:comp (u/copy comp)})
              :else :fail)]
    (if (not (= :fail result))
      (do
        (def succeed-counter (+ 1 succeed-counter))
        (when (contains? log-these-rules (u/get-in parent [:rule]))
          (log/info (str "overc success: " (syntax-tree parent) " -> " (syntax-tree result)))))
      (do
        (when pre-check?
          (def fail-counter (+ 1 fail-counter))
          (when (contains? log-these-rules (u/get-in parent [:rule]))
            (let [fp (fail-path parent {:comp comp})]
              (log/info
               (str "overc fail: " (syntax-tree parent)
                    " <- " (syntax-tree comp)
                    " fail path: " (vec fp)
                    ". parent has: " (u/pprint (u/get-in parent fp))
                    ", but comp has: " (u/pprint (u/get-in comp (rest fp)))
                    ".")))))))
    result))

(defn truncate [tree syntax-tree morph]
  (log/debug (str "truncating tree: " (syntax-tree tree)))
  (log/debug   (str "truncating:    " (syntax-tree tree)))
  (-> tree
      (assoc :syntax-tree (syntax-tree tree))
      (assoc :surface (morph tree))
      (dissoc :head)
      (dissoc :comp)
      (dissoc :1)
      (dissoc :2)))

(defn over [parents left-children right-children syntax-tree morph truncate?]
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
                   (not (= :fail
                           (u/unify head-summary
                                    (summary head-child))))))
         
         (map (fn [head-child]
                (let [parent-with-head
                      (overh-compact parent head-child syntax-tree)]
                  (->> comp-children

                       (filter (fn [comp-child]
                                 (not (= :fail
                                         (u/unify comp-summary
                                                  (summary comp-child))))))
                       
                       (map (fn [comp-child]
                              (overc-compact parent-with-head comp-child syntax-tree)))
                       (remove #(= :fail %))))))))))

   flatten

   (remove #(= :fail %))
   
   (map (fn [tree]
          (if truncate?
            (truncate tree syntax-tree morph)
            tree)))))

(defn pad-left [input length]
  (if (< (count input) length)
    (let [remainder (- length (count input))]
      (str (clojure.string/join "" (map (fn [_] "0") (range 0 remainder))) input))
    input))

(declare word-glue)
(declare word-glue-wrapper)

(defn all-groupings [input-string split-on lookup-fn]
  (let [vector-of-words (clojure.string/split input-string split-on)]
    (->> (range (int (Math/pow 2
                               (- (count vector-of-words)
                                  1)))
                0 -1)
         
         (map (fn [i]
                (let [retval (word-glue-wrapper vector-of-words i lookup-fn)]
                  (log/debug (str "i: " i "; grouping retval: " (vec retval)))
                  retval)))
         (filter (fn [vector-of-words]
                   (not (empty? vector-of-words))))

         ;; taking only 1 means we'll miss multi-word tokenizations, but
         ;; for this branch, it's worth it for the speedup.
         (take 1))))

(defn word-glue-wrapper
    "This function 'glues' words together into tokens, or in other words, transforms a vector of words into a vector of tokens, where tokens are defined as a sequence of words.
   Examples of words are:
   - 'white'
   - 'house'
  Examples of tokens are:
   - 'white',
   - 'house'
   - 'white house'

  Inputs:
  - a number that will be interpreted as a bit vector [.....] of bits ('0' or '1') which define how to form the tokens.
  - words: a sequence of strings, which we are trying to group into larger sub-sequences of strings; each of which sub-sequence is called a token."
  [vector-of-words number lookup-fn]
  (log/debug (str "LF (wgw) " lookup-fn))
  (let [
        bit-vector
        (-> number
            Integer/toBinaryString
            (menard.parse/pad-left (- (count vector-of-words) 1))
            (clojure.string/split #""))

        debug (log/debug (str "word-glue-wrapper: bit-vector is: " bit-vector))
        debug (log/debug (str "word-glue-wrapper: vector-of-words is: " vector-of-words))

        grouped
        (word-glue

         bit-vector
         vector-of-words

         lookup-fn

         ;; init'ed stuff:
         []
         []
         0)]

    (->> grouped
         (map (fn [vector-of-words]
                (clojure.string/join " " vector-of-words))))))

(defn word-glue
  "This function 'glues' words together into tokens, or in other words, transforms a vector of words into a vector of tokens, where tokens are defined as a sequence of words.
   Examples of words are:
   - 'white'
   - 'house'
  Examples of tokens are:
   - 'white',
   - 'house'
   - 'white house'

  Inputs:
  - a bit vector [.....] of bits ('0' or '1') which define how to form the tokens.
  - words: the array of primitive words which we are trying to group into tokens.
  - token-in-progress: an empty array
  - next-word: nil"
  [bits words lookup-fn tokens token-in-progress i]
  ;;     0       1      0      1      0
  ;;     1       0      0      0      1
  (log/debug (str "iteration: " i))
  (if (nil? token-in-progress)
    tokens
    (let [next-word (first words)
          current-bit (if (empty? bits)
                        "0"
                        (first bits))
          joined-token-in-progress
          (clojure.string/join " "
                               token-in-progress)]
      (log/debug (str "input current-bit: " current-bit))
      (log/debug (str "input next-word: " next-word))
      (log/debug (str "input tokens: " tokens))
      (log/debug (str "input words: " words))
      (log/debug (str "input token-in-progress: " (vec token-in-progress)))
      (log/debug (str "input token-in-progress (joined): " joined-token-in-progress))
      (let [complete-token
            (cond

              (empty? words)
              token-in-progress

              (= "1" current-bit)
              ;; token is done:
              ;; complete-token is
              ;; the concatenation of
              ;; the token-in-progress with the
              ;; current word.
              (let [complete-token
                    (lazy-cat token-in-progress [next-word])]
                (log/debug (str "current-bit=1: new complete-token: "
                               (vec complete-token)))
                complete-token)

              :else
              ;; we're not at a word boundary,
              ;; so there is no complete-token yet:
              nil)
            joined-complete-token
            (clojure.string/join " " complete-token)

            tokens (cond (and
                          complete-token
                          (<= (count complete-token) max-token-length-in-words)
                          (seq (lookup-fn joined-complete-token)))
                         ;; We have a completed token,
                         ;; and this token is valid (
                         ;; (not too long and findable
                         ;; in the lexicon), so
                         ;; add it to the list of already-
                         ;; validated tokens:
                         (lazy-cat tokens [complete-token])

                         (empty? words)
                         ;; we're at end of the input,
                         ;; and complete-token
                         ;; (i.e. token-in-progress)
                         ;; was not valid, so give up
                         ;; on this tokenization
                         ;; hypothesis:
                         []

                         :else
                         ;; we're in middle of the input
                         ;; but not at the end of a token:
                         tokens)]
        (if (or (and
                 (empty? words)
                 (empty? tokens))
                (and
                 complete-token
                 (empty? (lookup-fn joined-complete-token))))

          ;; give up.
          []

          ;; otherwise, keep going:
          (let [token-in-progress
                (cond
                  complete-token
                  ;; just finished a token, so start a new one in-progress:
                  [next-word]

                  :else
                  ;; existing token-in-progress is not complete:
                  ;; continuing with it by
                  ;; gluing next word to it, if any:
                  (let [token-in-progress
                        (lazy-cat token-in-progress [next-word])]
                    (log/debug (str "current-bit is 0, so token-in-progress will be:" (vec token-in-progress) " with next-word: " next-word))
                    token-in-progress))]
            (log/debug (str "output tokens: " tokens))
            (log/debug (str "output token-in-progress: " (vec token-in-progress)))
            (log/debug (str ""))
            (log/debug (str ""))
            (log/debug (str ""))
            (cond
              (empty? words)
              (do
                (log/debug (str "words are empty; we're done: "
                               "going to return: "
                               (vec tokens)))
                tokens)

              :else
              (word-glue (rest bits)
                         (rest words)
                         lookup-fn tokens
                         (if (= "1" current-bit)
                           []
                           token-in-progress)
                         (+ i 1)))))))))

(declare span-pairs)

(defn words-to-tokens [words]
  (->> (span-pairs 0 (count words))
       (map (fn [pairs]
              (map (fn [[l r]]
                     (reduce (fn [a b] (str a " " b)) (subvec words l r)))
                   pairs)))))

(defn span-pairs [i n]
  (cond (= i 0)
        (->> (range 0 (- n 1))
             (map (fn [x] [[0 (+ 1 x)][(+ 1 x) n]])))
        true
        (lazy-cat
         (span-pairs (- i 1) n)
         (->> (range i (+ i (- n 1)))
              (map (fn [x] [[i (+ 1 x)][(+ 1 x) (+ i n)]]))))))

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
  ([input split-on analyze-fn]
   (all-groupings input split-on analyze-fn)))

(defn parse-start
  [input split-on analyze-fn]
  (log/debug (str "parse-start input: " input))
  (log/debug (str "parse-start analyze-fn: " analyze-fn))
  (->> (tokenize input split-on analyze-fn)
       (map (fn [tokenization]
              (log/debug (str "looking at tokenization: " (vec tokenization)))
              (create-input-map tokenization analyze-fn)))))

(defn parse-in-stages [input-map input-length i grammar syntax-tree morph truncate?]
  (if (or (get input-map [0 input-length])
          (> i input-length))
    input-map
    (-> input-map
        (parse-spans-of-length input-length i grammar syntax-tree morph truncate?)
        (parse-in-stages input-length (+ 1 i) grammar syntax-tree morph truncate?))))

(defn parse
  "Return a list of all possible parse trees given all possible tokenizations."
  [tokenizations grammar lookup-fn syntax-tree morph truncate?]
  (if (seq tokenizations)
    (lazy-cat
     ;; e.g. lookup-fn.
     (let [tokenization (first tokenizations)]
       (let [token-count (count tokenization)
             all-parses (reduce (fn [input-map span-size]
                                  (parse-spans-of-length input-map token-count span-size grammar syntax-tree morph truncate?))
                                (create-input-map tokenization lookup-fn)
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
                     (lookup-fn-with-trim token lookup-fn))
                   tokenization))
                 partial-parses (vals (:all-parses result))]
             (log/debug (str "could not parse: \"" (vec tokenization) "\" with "
                             (count tokenization) " tokens having "
                             "token:sense pairs: "
                             (string/join ";"
                                          (pmap-if-available (fn [token]
                                                               (str token ":" (count (get analyses token)) ""))
                                                             tokenization))))
             (->> (flatten partial-parses)
                  (map (fn [partial-parse]
                         (merge partial-parse {::partial? true})))))
           (do (log/debug (str "parsed input:    \"" (vec tokenization) "\""))
               (:complete-parses result)))))
     (parse (rest tokenizations) grammar lookup-fn syntax-tree morph truncate?))))

(defn strip-map [m]
  (select-keys m [:1 :2 :canonical :left-is-head? :rule :surface]))

(defn parse-all [expression load-model-fn syntax-tree-fn split-on analyze-fn morph-fn truncate?]
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
              log-these-rules log-these-rules]
      (let [input-map (parse-start expression analyze-fn)]
        (-> input-map
            (parse-in-stages (count (keys input-map)) 2 (-> model :grammar) syntax-tree-fn truncate?)
            ((fn [m]
               {[0 (count (keys input-map))]
                (get m [0 (count (keys input-map))])})))))))
