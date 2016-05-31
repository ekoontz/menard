(ns babel.parse
 (:refer-clojure :exclude [get-in resolve find])
 (:require
  [babel.over :as over]
  [clojure.set :refer [union]]
  [clojure.string :as string]
  #?(:clj [clojure.tools.logging :as log])
  #?(:cljs [babel.logjs :as log])
  [dag_unify.core :refer (get-in strip-refs)]))

;; for now, using a language-independent tokenizer.
(def tokenizer #"[ ']")
(declare over)

(def map-fn #?(:clj pmap) #?(:cljs map))

(defn over [grammar left right morph]
  "opportunity for additional logging before calling the real (over)"
  (log/trace (str "parse/over: grammar size: " (count grammar)))
  (log/debug (str "parse/over: "
                  "left: " (vec (set (map morph left)))
                  "; right: " (vec (set (map morph right)))))
  (over/over grammar left right))

(defn square-cross-product [x]
  (mapcat (fn [each-x]
            (filter #(not (nil? %))
                    (map-fn (fn [each-y]
                              (if (= (second each-x) (first each-y))
                                [each-x each-y]))
                            x)))
          x))

(defn span-map [n]
  "take a 'square span array' and return a map of size -> _spans_,
   where _size_ is an integer, and _spans_ are all the [left,right] pairs whose combined
   size is equal to _size_."
  ;; for example:
  ;; (span-map 5) =>
  ;; 
  ;; {1 ([0 1]         [1 2] ...) ;; all of these are of size 1
  ;;  2 ([[0 1] [1 2]] [[1 2] [2 3]] ... ) ;; each pair has a combined size of 2
  ;;  3 ([[0 2] [2 3]] [[1 2] [2 3]] ... ) ;; each pair has a combined size of 3
  ;;  4 ([[0 3] [3 4]] [[1 2] [2 4]] ..
  ;;  5 ([[0 4] [4 5]] [[1 3] [3 5]] ... ) } ;; each pair has a combined size of 5.
  (cond
    (< n 2)
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
          spans (square-cross-product (spanpairs n))]
      (merge
       {1 (map-fn
           (fn [i]
             [i (+ 1 i)])
           (range 0 n))}
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

(defn parses [input n model span-map]
  (cond
    (= n 1) input
    (> n 1)
    (let [minus-1 (parses input (- n 1) model span-map)]
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
                                      left-lexemes (mapcat (:lookup model)
                                                           left-strings)
                                      right-lexemes (mapcat (:lookup model)
                                                            right-strings)
                                      left-signs (lazy-cat left-lexemes (filter map? left))
                                      right-signs (lazy-cat right-lexemes (filter map? right))
                                      debug (if (not (empty? left-lexemes))
                                              (log/debug (str "#lexemes found for left:'" (string/join "','" left-strings) "':"
                                                              (count left-lexemes))))
                                      debug (if (not (empty? right-lexemes))
                                              (log/debug (str "#lexemes found for right:'" (string/join "','" right-strings) "':"
                                                              (count right-lexemes))))
                                      ]
                                  (lazy-cat
                                   (if (and (not (empty? left-signs))
                                            (not (empty? right-signs)))
                                     (over (:grammar model) left-signs right-signs (:morph model)))
                                   
                                 ;; TODO: explain why we can use (first) here for the left- and right-strings.
                                   ;; Throw an exception if (> 1 (count left-strings)) or (> 1 (count right-strings))
                                   [(string/join " " [(first left-strings) (first right-strings)])]))
                                ;; </value>
                                })
                             (get span-map n)))))))

(declare parse)

(defn parse-from-file [reader]
  "parse linefeed-separated file line-by-line."
  (map parse
       (line-seq reader)))

(defn parse
  "return a list of all possible parse trees for a string or a sequence of tokens.
   If the input is a string, then use a language-independent tokenizer to turn the string into a sequence of tokens.
   In the latter case, the tokens are assumed to be produced by splitting a string in 
   some language-dependent way."
  ([input model & [original-input]]
   (let [original-input (if original-input original-input input)]
     (cond (= (type input) java.io.BufferedReader)
           (parse-from-file input model)
           (string? input)
           (do
             (log/info (str "parsing input: '" input "'"))
             (parse (filter #(not (empty? %)) (string/split input tokenizer)) model original-input))
         
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
                   (parses input-map token-count model
                           (span-map token-count))
                   result
                   {:token-count token-count
                    :complete-parses
                    (filter map? (get all-parses
                                      [0 token-count]))
                    :all-parses all-parses}]
               (if (empty? (:complete-parses result))
                 (log/warn (str "could not parse: '" original-input "'"))
                 (log/info (str "successfully parsed input: '" original-input "'")))
               (:complete-parses result)))

           true
           (throw (Exception. "Don't know how to parse input of type: " (type input)))))))

  
