(ns babylon.parse
  (:require
   [clojure.set :refer [union]]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [dag_unify.core :as u]))

(def ^:dynamic lookup-fn)
(def ^:dynamic grammar nil)
(def ^:dynamic truncate? true)

;; for now, using a language-independent tokenizer.
(def tokenizer #"[ ']")
(def map-fn #?(:clj pmap) #?(:cljs map))

(declare overc)
(declare overh)
(declare overhc)

;; TODO: distinguish between when:
;; 1) called with only a child1 (no child2),
;; 2) called with both a child1 and a child2, but child2's supplied value is nil:
;;    should be treated the same as empty list.
(defn over [parents child1 & [child2]]
  (cond (map? parents)
        (over [parents] child1 child2)
        
        true
        (mapcat
         (fn [parent]
           (let [[head comp] (if (= (:1 parent) (:head parent))
                               [child1 child2]
                               [child2 child1])]
             (overhc parent head comp)))
         parents)))

(defn overhc [parent head comp]
  (-> parent
      (overh head)
      (overc comp)))

(defn overh
  "add given head as the head child of the phrase: parent."
  [parent head]
  ;; TODO: get rid of all this type-checking and use
  ;; whatever people use for Clojure argument type-checking.
  (cond
    (or (seq? head)
        (vector? head))
    (mapcat (fn [child]
              (overh parent child))
            head)
    true
    ;; TODO: 'true' here assumes that both parent and head are maps: make this assumption explicit,
    ;; and save 'true' for errors.
    (let [result (u/unify! (u/copy parent)
                           {:head (u/copy head)})]
      (if (not (= :fail result))
        (do
          (log/debug (str "overh success: " (get-in parent [:rule]) "-> cat=" (get-in head [:synsem :cat])))
          (log/debug (str "overh success: " (get-in parent [:rule]) "-> pred=" (get-in head [:synsem :sem :pred]))) 
          [result])
        (log/debug (str "overh: fail-path for rule: " (:rule parent) ":"
                        (u/fail-path (u/copy parent) {:head (u/copy head)})))))))

(defn overc [parent comp]
  "add given child as the complement of the parent"
  (cond
    (or (seq? parent)
        (vector? parent))
    (let [parents (lazy-seq parent)]
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
    (let [result (u/unify! (u/copy parent)
                           {:comp (u/copy comp)})
          is-fail? (= :fail result)]
      (if (not is-fail?)
        (do
          (log/debug (str "overc success: " (get-in parent [:rule]) " -> " (get-in comp [:rule]
                                                                                   (get-in comp [:synsem :sem :pred]
                                                                                           "(no pred for comp)"))))
          [result])
        (log/debug (str "overc: fail-path for rule: " (:rule parent) ":"
                        (u/fail-path (u/copy parent) {:comp (u/copy comp)})))))))

(defn square-cross-product [x]
  (mapcat (fn [each-x]
            (filter #(not (nil? %))
                    (map-fn (fn [each-y]
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
                                (map-fn (fn [y]
                                          [x y])
                                        (range (+ x 1) (+ n 1))))
                              (range 0 n)))
          spans (square-cross-product (spanpairs max))]
      (merge
       {1 (map-fn
           (fn [i]
             [i (+ 1 i)])
           (range 0 max))}
       (reduce (fn [resultant-map this-submap]
                 (merge-with union ;; TODO: this could get expensive - consider alternatives.
                             resultant-map this-submap))
               (map-fn (fn [span-pair]
                         (let [left-span (first span-pair)
                               left-boundary (first left-span)
                               right-span (second span-pair)
                               right-boundary (second right-span)]
                           {(- right-boundary left-boundary)
                            (list span-pair)}))
                       spans))))))

(defn parses [input n span-map syntax-tree morph]
  (cond
    (= n 1) input
    (> n 1)
    (let [minus-1 (parses input (- n 1) span-map syntax-tree morph)]
      (log/debug (str "parses: input=" input))
      (log/debug (str "parses: syntax-tree: " syntax-tree))
      (merge minus-1
             (reduce (fn [x y]
                       (merge-with concat x y))
                     (map-fn (fn [span-pair]
                               
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
                                      left-signs (lazy-cat left-lexemes (filter map? left))
                                      right-signs (lazy-cat right-lexemes (filter map? right))]
                                  (lazy-cat
                                   (if (and (not (empty? left-signs))
                                            (not (empty? right-signs)))
                                     (->>
                                      (over grammar left-signs right-signs)
                                      (map (fn [tree]
                                             (cond truncate?
                                                   (-> tree (dissoc :head) (dissoc :comp)
                                                       (dissoc :1) (dissoc :2)
                                                       (assoc :surface (morph tree))
                                                       (assoc :syntax-tree (syntax-tree tree)))
                                                   true tree)))))
                                   [(string/join " " [(first left-strings) (first right-strings)])]))})
                             ;; </value>
                             
                             (get span-map n)))))))

(declare parse)

(defn parse-from-file [reader]
  "parse linefeed-separated file line-by-line."
  (map parse
       (line-seq reader)))

(defn parse
  "Return a list of all possible parse trees for a string or a sequence of tokens.
   If the input is a string, then use a language-independent tokenizer
  to turn the string into a sequence of tokens.  In the latter case,
  the tokens are assumed to be produced by splitting a string in some
  language-dependent way."
  ([input syntax-tree morph]
   (cond (string? input)
         (do
           (log/debug (str "parsing input: '" input "' with syntax-tree: " syntax-tree))
           ;; tokenize input (more than one tokenization is possible), and parse each tokenization.
           (let [tokenizations (filter #(not (empty? %)) (string/split input tokenizer))
                 result (parse tokenizations syntax-tree morph)]
             (if (empty? result)
               (let [analyses
                     (zipmap
                      tokenizations
                      (map (fn [token]
                             (lookup-fn token))
                           tokenizations))]
                 (log/warn (str "could not parse: \"" input "\". Tokenizations attempted: "
                                (string/join ";"
                                             (map (fn [token]
                                                    (str token ":" (count (get analyses token)) ""))
                                                  tokenizations)))))
               (log/debug (str "parsed input:    \"" input "\"")))
             result))
           
         (or (seq? input) (vector? input))
         ;; assume input is a list of tokens.
         (let [tokens input
               token-count (count tokens)
               token-count-range (range 0 token-count)
               input-map (zipmap (map (fn [i] [i (+ i 1)])
                                      token-count-range)
                                 (map (fn [i] [(nth tokens i)])
                                      token-count-range))]
           (log/debug (str "input map:" input-map))
           (let [all-parses
                 (parses input-map token-count
                         (span-map token-count) syntax-tree morph)
                 result
                 {:token-count token-count
                  :complete-parses
                  (filter map? (get all-parses
                                    [0 token-count]))
                  :all-parses all-parses}]
             (:complete-parses result)))
         true
         (throw (Exception. (str "Don't know how to parse input of type: " (type input)))))))
