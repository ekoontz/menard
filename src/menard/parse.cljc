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
    (exception (str "lookup-fn was not bound."))))

(def ^:dynamic grammar nil)
(def ^:dynamic syntax-tree (fn [x] (log/warn (str "'syntax-tree' was not bound."))))
(def ^:dynamic morph (fn [x] (log/warn (str "'morph' was not bound."))))
(def ^:dynamic truncate? false)
(declare truncate)
(def ^:dynamic split-on #"[ ']+")
(def ^:dynamic take-this-many 30)
(def ^:dynamic debug-rule-for-comp nil)
(def ^:dynamic enable-pmap? true)
;; a token can be max 4 words, e.g.
;; "preston school of industry" is possible, but
;; but not:
;; "presidents of the united states of america"
(def ^:dynamic max-token-length-in-words 4)

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

(defn pad-left [input length]
  (if (< (count input) length)
    (let [remainder (- length (count input))]
      (str (clojure.string/join "" (map (fn [_] "0") (range 0 remainder))) input))
    input))

(declare word-glue)
(declare word-glue-wrapper)


(defn all-groupings [words lookup-fn]
  (let [vector-of-words (clojure.string/split words split-on)]
    (->> (range 0 (int (Math/pow 2
                                 (- (count vector-of-words)
                                    1))))
         (map (fn [i]
                (let [retval (word-glue-wrapper vector-of-words i lookup-fn)]
                  (log/info (str "i: " i "; grouping retval: " (vec retval)))
                  retval)))
         (filter (fn [vector-of-words]
                   (not (empty? vector-of-words)))))))

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

        debug (log/info (str "word-glue-wrapper: bit-vector is: " bit-vector))
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
    
    (log/info (str "grouped is: " grouped))
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
                    (concat token-in-progress [next-word])]
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
                         (do
                           (log/info (str "found in lexicon: "
                                          joined-complete-token))
                           (vec (concat tokens [complete-token])))
                         
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
                        (concat token-in-progress [next-word])]
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
              (vec (map (fn [[l r]]
                          (reduce (fn [a b] (str a " " b)) (subvec words l r)))
                        pairs))))))

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
     are all the parses for the substring of tokens [t_i....t_j].
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
  [(->> (-> input
            (string/split split-on))
        (filter #(not (string/blank? %))))])

(defn parse-start
  [input]
  ;; this 'doall' is necessary to force the lazy sequence
  ;; to use the bindings for the dynamic variable lookup-fn.
  (doall
   (->> (tokenize input)
        (map (fn [tokenization]
               (log/info (str "looking at tokenization: " (vec tokenization)))
               (create-input-map tokenization))))))

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
