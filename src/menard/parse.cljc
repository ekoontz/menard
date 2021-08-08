(ns menard.parse
  (:require
   [clojure.set :refer [union]]
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [dag_unify.core :as u]
   [dag_unify.diagnostics :as diag]
   [menard.reflexives :refer [reflexive-options]]))

(def parse-only-one? false)

(def ^:dynamic lookup-fn
  (fn [_]
    []))
(def ^:dynamic grammar nil)
(def ^:dynamic syntax-tree (fn [x] (log/warn (str "'syntax-tree' was not bound."))))
(def ^:dynamic truncate? false)
(def ^:dynamic split-on #"[ ']")
(def ^:dynamic take-this-many 30)
(def ^:dynamic debug-rule-for-comp nil)

(defn pmap-if-available [fn args]
  #?(:clj
     (pmap fn args))
  #?(:cljs
     (map fn args)))

(def log-these-rules #{"vp-modal-te" "vp-te"})

(defn overh
  "add given head as the head child of the phrase: parent."
  ;; TODO: get rid of all this type-checking in this (cond)
  ;; and use clojure.spec.
  [parent head]
  (cond
    (or (seq? head)
        (vector? head))
    (->>
     head
     (mapcat
      (fn [child]
        (overh parent child))))

    :else
    ;; TODO: 'true' here assumes that both parent and head are maps: make this assumption explicit,
    ;; and save :else for errors.
    (do
      (log/debug (str "overh: parent: " (syntax-tree parent)))
      (log/debug (str "overh: head:   " (syntax-tree head)))
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
            (log/debug (str "overh success: " (syntax-tree parent) " -> " (syntax-tree result)))
            [result])
          (do
            (when (contains? log-these-rules (u/get-in parent [:rule]))
              (log/debug
               (str "overh fail:   " (syntax-tree parent) " <- " (syntax-tree head)
                    " " (let [fp (diag/fail-path parent {:head head})]
                          {:path (:path fp)
                           :arg1 (u/pprint (:arg1 fp))
                           :arg2 (u/pprint (:arg2 fp))}))))))))))
  
(defn overc
  "add given child as the complement of the parent"
  [parent comp]
  (cond
    (or (seq? parent)
        (vector? parent))
    (let [parents parent]
      (mapcat (fn [parent]
                (overc parent comp))
              parents))
    
    (or (seq? comp)
        (vector? comp))
    (let [comp-children comp]
      (mapcat (fn [child]
                (overc parent child))
              comp-children))
    :else
    (let [pre-check? (= (u/get-in parent [:comp :cat])
                        (u/get-in comp [:cat] (u/get-in parent [:comp :cat])))
          result
          (cond pre-check?
                (u/unify! (u/copy parent)
                          {:comp (u/copy comp)})
                :else :fail)]
      (if (not (= :fail result))
        (do
          (log/debug (str "overc success: " (syntax-tree result) " -> " (syntax-tree result)))
          [result])
        (do
          (when (contains? log-these-rules (u/get-in parent [:rule])))
          (log/debug
           (str "overc fail: " (syntax-tree parent) " <- " (syntax-tree comp)))
          (log/debug (str " "
                         (let [fp (diag/fail-path (u/copy parent)
                                                  {:comp (u/copy comp)})]
                           
                           (str " at: " (vec (:path fp)) ", parent has: " (:arg1 fp) " but comp has: " (:arg2 fp)))))
          [])))))

(defn over [parents child1 child2]
  (->>
   parents
   (pmap-if-available
    (fn [parent]
      (let [[head comp] (if (= (:1 parent) (:head parent))
                          [child1 child2]
                          [child2 child1])]
        (-> parent
            (overh head)
            (overc comp)))))
   (reduce
    (fn [a b]
      (lazy-cat a b)))

   (map (fn [expression]
          (map (fn [option]
                 (u/unify option expression))
               (shuffle reflexive-options))))

   (flatten)
     
   (remove #(= % :fail))))

(defn span-pairs [i n]
  (cond (= i 0)
        (->> (range 0 (- n 1))
             (map (fn [x] [[0 (+ 1 x)][(+ 1 x) n]])))
        true
        (concat
         (span-pairs (- i 1) n)
         (->> (range i (+ i (- n 1)))
              (map (fn [x] [[i (+ 1 x)][(+ 1 x) (+ i n)]]))))))

(declare truncate)

(defn lookup-fn-with-trim [string]
  (let [trimmed (clojure.string/trim string)]
    (when (and (not (string/blank? trimmed))
               (= trimmed string))
      (lookup-fn string))))

(defn summary
  "for diagnostic logging"
  [m]
  (into {}
        (->> (keys m)
             (mapcat (fn [k]
                       (if (seq (get m k))
                         [{k (map syntax-tree (get m k))}]))))))

(defn parses
  "returns a list of elements
      where each element is: [[_i_ _j_] _parses_] and _parses_
   is a list of structures
      where each structure is a parse covering the interval in the input from _i_ to _j_."

  [input n total morph]
  (cond
    (= n 1) input
    (> n 1)
    (let [minus-1 (-> (parses input (- n 1) total morph))]
      (merge minus-1
             (reduce (fn [left-parses right-parses]
                       (merge-with (fn [a b]
                                     (lazy-cat a b))
                                   left-parses right-parses))
                     (->>
                      (span-pairs (- total n) n)
                      (pmap-if-available
                       (fn [[left right]]
                         (let [left-values (get minus-1 left)
                               right-values (get minus-1 right)
                               left-strings (filter string? left-values)
                               right-strings (filter string? right-values)
                               left-lexemes (reduce (fn [& [a b]] (lazy-cat a b))
                                                    (pmap-if-available
                                                     (fn [string]
                                                       (log/info (str "looking up a: " string))
                                                       (lookup-fn-with-trim string))
                                                     left-strings))
                               right-lexemes (reduce (fn [& [a b]] (lazy-cat a b))
                                                     (pmap-if-available
                                                      (fn [string]
                                                        (log/info (str "looking up b: " string))
                                                        (lookup-fn-with-trim string))
                                                      right-strings))
                               left-signs (lazy-cat left-lexemes (filter map? left-values))
                               right-signs (lazy-cat right-lexemes (filter map? right-values))
                               all-results (over grammar left-signs right-signs)
                               taken-results (take take-this-many all-results)
                               taken-plus-one-results (take (+ 1 take-this-many) all-results)]
                           ;; create a new key/value pair: [i,j] => parses,
                           ;; where each parse in parses matches the tokens from [i,j] in the input.
                           (if (and (seq left-signs)
                                    (seq right-signs))
                           {;; <key: [i,j]>
                            [(first left)
                             (second right)]
                            ;; </key>
                            
                            ;; <value: parses for strings indexed at [i,j]>
                            (lazy-cat
                             (do
                               (when (> (count taken-plus-one-results) (count taken-results))
                                 (log/warn (str "more than " take-this-many " parses for: '"
                                                (morph (first taken-results)) "' ; first: "
                                                (syntax-tree (first taken-results)))))
                               (->>
                                taken-results
                                (pmap-if-available (fn [tree]
                                                     (if truncate?
                                                       (truncate tree syntax-tree morph)
                                                       tree)))
                                
                                ;; if returning more than one parse,
                                ;; you must run (vec) on the return value of this (map).
                                ;; This is because (map f v) returns a lazy sequence out of
                                ;; calls to f. However f does not have the dynamic bindings
                                ;; that the caller has set up. So without the(vec), subsequent runs of f
                                ;; (after the first one) will be ran,
                                ;; with no binding for ^:dynamic variables like 'syntax-tree'.
                                ;; TODO: fix this by re-binding variables using (binding [syntax-tree syntax-tree]).
                                ((fn [trees]
                                   (if parse-only-one?
                                     trees
                                     (vec trees)))))))
                            ;; </value>
                            }))))))))))

(defn truncate [tree syntax-tree morph]
  (-> tree
      (assoc :syntax-tree (syntax-tree tree))
      (assoc :surface (morph tree))
      (dissoc :head)
      (dissoc :comp)
      (dissoc :1)
      (dissoc :2)))

(defn parse-tokens
  "Return a list of all possible parse trees for a list of tokens."
  [tokens morph]
  ;; TODO: remove 'morph' as an input parameter; use a dynamic binding instead.
  (let [token-count (count tokens)
        token-count-range (range 0 token-count)
        input-map (zipmap (pmap-if-available (fn [i] [i (+ i 1)])
                               token-count-range)
                          (pmap-if-available (fn [i] [(nth tokens i)])
                               token-count-range))]
      (log/debug (str "input map:" input-map))
      (let [all-parses
            (parses input-map token-count token-count morph)
            result
            {:token-count token-count
             :complete-parses
             (filter map? (get all-parses
                               [0 token-count]))
             :all-parses all-parses}]
        result)))

(defn tokenize [input]
  (filter #(not (string/blank? %)) (string/split input split-on)))
  
(defn parse
  "Return a list of all possible parse trees for a string.
   Use a language-independent tokenizer (split on space and
  apostrophe) to turn the string into a sequence of tokens."
  ;; TODO: remove 'morph' as an input parameter; use a dynamic binding instead.
  [input morph]
  (log/debug (str "parsing input: '" input "' with syntax-tree: " syntax-tree))
  ;; tokenize input (more than one tokenization is possible), and parse each tokenization.
  (let [tokenizations (tokenize input)
        result (parse-tokens tokenizations morph)]
    (if (empty? (:complete-parses result))

      ;; if there are no complete parses,
      ;; cobble together results by combining
      ;; partial parses with lexical lookups of tokens (if they exists).
      ;; e.g. we can parse "er zijn katten" so there is a complete parse
      ;; but "er zijn kat" can't be fully parsed, so we return:
      ;; [er zijn] [kat].
      (let [analyses
            (zipmap
             tokenizations
             (pmap-if-available
              (fn [token]
                (lookup-fn-with-trim token))
              tokenizations))
            partial-parses (->> (vals (:all-parses result))
                                (pmap-if-available (fn [x] (->> x (filter map?))))
                                (filter seq?))]
        (log/debug (str "could not parse: \"" input "\". token:sense pairs: "
                       (string/join ";"
                                    (pmap-if-available (fn [token]
                                           (str token ":" (count (get analyses token)) ""))
                                         tokenizations))
                       (str "; partial parses: " (count (mapcat (fn [parses-for-span]
                                                                  (pmap-if-available syntax-tree parses-for-span))
                                                                partial-parses)) ".")))
        (->> (flatten partial-parses)
             (map (fn [partial-parse]
                    (merge partial-parse {::partial? true})))))
      (do (log/debug (str "parsed input:    \"" input "\""))
          (:complete-parses result)))))

