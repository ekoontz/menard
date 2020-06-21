(ns menard.parse
  (:require
   [clojure.set :refer [union]]
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [dag_unify.core :as u]
   [dag_unify.diagnostics :as diag]))

(def parse-only-one? false)

(def ^:dynamic lookup-fn
  (fn [token]
    []))
(def ^:dynamic grammar nil)
(def ^:dynamic syntax-tree)
(def ^:dynamic truncate? true)
(def ^:dynamic split-on #"[ ']")
(def ^:dynamic take-this-many 10)
(def ^:dynamic debug-rule-for-comp nil)

(defn pmap-if-available [fn args]
  #?(:clj
     (pmap fn args))
  #?(:cljs
     (map fn args)))

(defn overh
  "add given head as the head child of the phrase: parent."
  [parent head]
  ;; TODO: get rid of all this type-checking and use
  ;; whatever people use for Clojure argument type-checking.
  (cond
    (or (seq? head)
        (vector? head))
    (mapcat
     (fn [child]
       (overh parent child))
     head)
    true
    ;; TODO: 'true' here assumes that both parent and head are maps: make this assumption explicit,
    ;; and save 'true' for errors.
    (let [debug (log/debug (str "overh: " (syntax-tree parent) "; head: " (syntax-tree head)))
          result (u/unify parent
                          {:head head})]
      (if (not (= :fail result))
        (do
          (log/debug (str "overh success: " (syntax-tree parent) " -> " (syntax-tree result)))
          [result])
        (log/debug
         (str "overh fail: " (syntax-tree parent) " <- " (syntax-tree head)
              " " (diag/fail-path parent {:head head})))))))

(defn overc [parent comp]
  "add given child as the complement of the parent"
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
    true
    (let [result
          (u/unify! (u/copy parent)
                    {:comp (u/copy comp)})]
      (if (not (= :fail result))
        (do
          (log/debug (str "overc success: " (syntax-tree result) " -> " (syntax-tree result)))
          [result])
        (do
          (when (and debug-rule-for-comp (= (:rule parent) debug-rule-for-comp))
            (log/info (str "overc fail: " (syntax-tree parent) " <- " (syntax-tree comp)))
            (log/info (str " "
                           (let [fp (diag/fail-path (u/copy parent)
                                                    {:comp (u/copy comp)})]
                             
                             (str " at: " (vec (:path fp)) ", parent has: " (:arg1 fp) " but comp has: " (:arg2 fp))))))
          [])))))

(defn over [parents child1 child2]
  (mapcat
   (fn [parent]
     (let [[head comp] (if (= (:1 parent) (:head parent))
                         [child1 child2]
                         [child2 child1])]
       (-> parent
           (overh head)
           (overc comp))))
   parents))

(defn square-cross-product [x]
  (mapcat (fn [each-x]
            (filter #(not (nil? %))
                    (map
                     (fn [each-y]
                       (if (= (second each-x) (first each-y))
                         [each-x each-y]))
                     x)))
          x))

(defn span-map [max]
  "return a map of size -> _spans_, where _size_ is an integer, and _spans_ are all
                            the [left,right] pairs whose combined size is equal to _size_., and _size_ ranges from 1 to _max_."
  ;; for example:
  ;; (span-map 5) =>
  ;; 
  ;; {1 ([0 1]         [1 2] ...) ;; all of these are of size 1
  ;;  2 ([[0 1] [1 2]] [[1 2] [2 3]] ... ) ;; each pair has a combined size of 2
  ;;  3 ([[0 2] [2 3]] [[1 2] [2 3]] ... ) ;; each pair has a combined size of 3
  ;;  4 ([[0 3] [3 4]] [[1 2] [2 4]] ..
  ;;  5 ([[0 4] [4 5]] [[1 3] [3 5]] ... ) } ;; each pair has a combined size of 5.
  (cond
    (< max 2)
    nil

    true
    (let [
          ;; TODO: rather than this function, make a static lookup table, at least for n < (e.g.) 5.
          ;; e.g. (spanpairs 5) =>
          ;; ([0 1] [0 2].. [0 5] [1 2] ..[1 4] [1 5] .... [4 5])
          spanpairs (fn [n]
                      (mapcat (fn [x]
                                (pmap-if-available
                                 (fn [y]
                                   [x y])
                                 (range (+ x 1) (+ n 1))))
                              (range 0 n)))
          spans (square-cross-product (spanpairs max))]
      (merge
       {1 (map
           (fn [i]
             [i (+ 1 i)])
           (range 0 max))}
       (reduce (fn [resultant-map this-submap]
                 (merge-with union ;; TODO: this could get expensive - consider alternatives.
                             resultant-map this-submap))
               (map
                (fn [span-pair]
                  (let [left-span (first span-pair)
                        left-boundary (first left-span)
                        right-span (second span-pair)
                        right-boundary (second right-span)]
                    {(- right-boundary left-boundary)
                     (list span-pair)}))
                spans))))))

(declare truncate)

(defn parses [input n span-map morph]
  (cond
    (= n 1) input
    (> n 1)
    (let [minus-1 (parses input (- n 1) span-map morph)]
      (merge minus-1
             (reduce (fn [x y]
                       (merge-with concat x y))
                     (pmap-if-available
                      (fn [span-pair]
                        
                        ;; create a new key/value pair: [i,j] => parses,
                        ;; where each parse in parses matches the tokens from [i,j] in the input.
                        {

                         ;; <key: [i,j]>
                         [(first (first span-pair))
                          (second (second span-pair))]
                         ;; </key>

                         ;; <value: parses for strings indexed at [i,j]>
                         (let [left (get minus-1 (first span-pair))
                               right (get minus-1 (second span-pair))
                               left-strings (filter string? left)
                               right-strings (filter string? right)
                               left-lexemes (mapcat (fn [string]
                                                      (lookup-fn string))
                                                    left-strings)
                               right-lexemes (mapcat (fn [string]
                                                       (lookup-fn string))
                                                     right-strings)
                               left-signs (concat left-lexemes (filter map? left))
                               right-signs (concat right-lexemes (filter map? right))
                               all-results (over grammar left-signs right-signs)
                               taken-results (take take-this-many (shuffle all-results))
                               taken-plus-one-results (take (+ 1 take-this-many) all-results)]
                           (concat
                            (if (and (not (empty? left-signs))
                                     (not (empty? right-signs)))
                              (do
                                (log/debug (str (string/join ", " (set (map syntax-tree left-signs))) " || "
                                                (string/join ", " (set (map syntax-tree right-signs)))))
                                (if (> (count taken-plus-one-results) (count taken-results))
                                  (log/warn (str "more than " take-this-many " parses; first: " (syntax-tree (first taken-results)))))
                                (->>
                                 taken-results
                                 (map (fn [tree]
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
                                      (vec trees))))))
                              [(string/join " " [(first left-strings) (first right-strings)])])))})
                      ;; </value>
                      
                      (get span-map n)))))))

(defn truncate [tree syntax-tree morph]
  (-> tree
      (assoc :syntax-tree (syntax-tree tree))
      (assoc :surface (morph tree))
      (dissoc :head)
      (dissoc :comp)
      (dissoc :1)
      (dissoc :2)))

(defn parse-tokens [tokens morph]
  "Return a list of all possible parse trees for a list of tokens."
  ;; TODO: remove 'morph' as an input parameter; use a dynamic binding instead.
  (let [debug (log/debug (str "parsing input: (seq or vector) with syntax-tree: " syntax-tree))
        token-count (count tokens)
        token-count-range (range 0 token-count)
        input-map (zipmap (map (fn [i] [i (+ i 1)])
                               token-count-range)
                          (map (fn [i] [(nth tokens i)])
                               token-count-range))]
      (log/debug (str "input map:" input-map))
      (let [all-parses
            (parses input-map token-count
                    (span-map token-count) morph)
            result
            {:token-count token-count
             :complete-parses
             (filter map? (get all-parses
                               [0 token-count]))
             :all-parses all-parses}]
        result)))

(defn parse
  "Return a list of all possible parse trees for a string.
   Use a language-independent tokenizer (split on space and
  apostrophe) to turn the string into a sequence of tokens."
  ;; TODO: remove 'morph' as an input parameter; use a dynamic binding instead.
  [input morph]
  (log/debug (str "parsing input: '" input "' with syntax-tree: " syntax-tree))
  ;; tokenize input (more than one tokenization is possible), and parse each tokenization.
  (let [tokenizations (filter #(not (empty? %)) (string/split input split-on))
        result (parse-tokens tokenizations morph)]
    (if (empty? (:complete-parses result))
      (let [analyses
            (zipmap
             tokenizations
             (map (fn [token]
                    (lookup-fn token))
                  tokenizations))
            partial-parses (->> (vals (:all-parses result))
                                (map (fn [x] (->> x (filter map?))))
                                (filter #(not (empty? %))))]
        (log/warn (str "could not parse: \"" input "\". token:sense pairs: "
                       (string/join ";"
                                    (map (fn [token]
                                           (str token ":" (count (get analyses token)) ""))
                                         tokenizations))
                       (str "; partial parses: " (count (mapcat (fn [parses-for-span]
                                                                  (map syntax-tree parses-for-span))
                                                                partial-parses)) ".")))
        (flatten partial-parses))
      (do (log/debug (str "parsed input:    \"" input "\""))
          (:complete-parses result)))))

