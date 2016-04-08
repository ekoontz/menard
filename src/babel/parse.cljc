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

(defn over [grammar left right]
  "opportunity for additional logging before calling the real (over)"
  (log/trace (str "parse/over: grammar size: " (count grammar)))
  (over/over grammar left right))

(defn cross-product [x y]
  (mapcat (fn [each-x]
            (filter #(not (nil? %))
                    (pmap (fn [each-y]
                            (if (= (second each-x) (first each-y))
                              [each-x each-y]))
                          y)))
          x))

(defn spanpairs [n]
  (mapcat (fn [x]
            (pmap (fn [y]
                    [x y])
                  (range (+ x 1) (+ n 1))))
          (range 0 n)))

(defn square [x]
  (let [pairs (spanpairs x)]
    (cross-product pairs pairs)))

(defn span-map [n]
  "take a 'square span array' and reorganizes it into a map of size -> _spans_,
   where _size_ is an integer, and _spans_ are all the [left,right] pairs whose combined
   size is equal to _size_."
  (let [spans
        (square n)]
    (merge
     {1 (pmap
         (fn [i]
           [i (+ 1 i)])
         (range 0 n))}
     (reduce (fn [resultant-map this-submap]
               (merge-with union ;; TODO: this could get expensive - consider alternatives.
                           resultant-map this-submap))
             (pmap (fn [span-pair]
                     (let [left-span (first span-pair)
                           left-boundary (first left-span)
                           right-span (second span-pair)
                           right-boundary (second right-span)]
                       {(- right-boundary left-boundary)
                        (list span-pair)}))
                   spans)))))

(defn parses [input n model span-map]
  (cond
    (= n 1) input
    (> n 1)
    (let [minus-1 (parses input (- n 1) model span-map)]
      (merge minus-1
             (reduce (fn [x y]
                       (merge-with concat x y))
                     (pmap (fn [span-pair]

                             ;; create a new key/value pair: [i,j] => parses,
                             ;; where each parse in parses matches the tokens from [i,j] in the input.
                             {

                              ;; <key>
                              [(first (first span-pair))
                               (second (second span-pair))]
                              ;; </key>

                              ;; <value>
                              (let [left (get minus-1 (first span-pair))
                                    right (get minus-1 (second span-pair))
                                    left-strings (filter string? left)
                                    right-strings (filter string? right)
                                    left-lexemes (mapcat (:lookup model)
                                                         left-strings)
                                    right-lexemes (mapcat (:lookup model)
                                                          right-strings)
                                    left-signs (lazy-cat left-lexemes (filter map? left))
                                    right-signs (lazy-cat right-lexemes (filter map? right))]
                                (lazy-cat
                                 (if (and (not (empty? left-signs))
                                          (not (empty? right-signs)))
                                   (over (:grammar model) left-signs right-signs))
                                 
                                 ;; TODO: explain why we can use (first) here for the left- and right-strings.
                                 ;; Throw an exception if (> 1 (count left-strings)) or (> 1 (count right-strings))
                                 [(string/join " " [(first left-strings) (first right-strings)])]))
                              ;; </value>
                              })

                           (get span-map n)))))))

(defn parse [input model]
  "return a list of all possible parse trees for a string or a list of lists of maps
   (a result of looking up in a dictionary a list of tokens from the input string)"
  (log/info (str "parsing input: '" input "'"))
  (let [tokens (string/split input tokenizer)
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
        (log/warn (str "could not parse: " input))
        (log/info (str "successfully parsed input: '" input "'")))
      (:complete-parses result))))

