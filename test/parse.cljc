(ns menard.test.parse
  (:require [menard.parse :as parse]
            [menard.parse.word :as word]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def split-on #"[ ]")
(def max-word-length-in-tokens 3)

;; <mock data>
(def expression-1 "The White House Press Corps Dinner")
(defn lookup-fn-1 [input-string]
  (cond
    (or (= input-string "The")
        (= input-string "White")
        (= input-string "House")
        (= input-string "Press")
        (= input-string "Corps")        
        (= input-string "Dinner")
        (= input-string "White House")
        (= input-string "Press Corps"))
    [{:top :top}]
    :else []))

(def expression-2 "the hiding place")
(defn lookup-fn-2 [input-string]
  (cond
    (or (= input-string "the")
        (= input-string "hiding")
        (= input-string "place")
        (= input-string "hiding place"))
    [{:top :top}]
    :else []))

;; in this case, suppose that although "boot" and "rain boot" are words in the lexicon,
;; "rain" by itself is not:
(def expression-3 "the rain boot")
(defn lookup-fn-3 [input-string]
  (cond
    (or (= input-string "the")
        (= input-string "boot")
        (= input-string "rain boot"))
    [{:top :top}]
    :else []))

;; </mock-data>

(deftest word-graph-test-1
  ;; TODO: add more tests for the graph
  (is (map? (word/graph expression-1 split-on lookup-fn-1 max-word-length-in-tokens))))

(deftest word-groupings-test-1
  (let [found-groupings (word/groupings expression-1 split-on lookup-fn-1 max-word-length-in-tokens)]
    (is (= (count found-groupings) 4))))

(deftest word-groupings-test-2
  (let [found-groupings (word/groupings expression-2 split-on lookup-fn-2 max-word-length-in-tokens)]
    (is (= (count found-groupings) 2))))

(deftest word-groupings-test-3
  (let [found-groupings (word/groupings expression-3 split-on lookup-fn-3 max-word-length-in-tokens)]
    (is (= (count found-groupings) 3))))

