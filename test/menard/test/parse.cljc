(ns menard.test.parse
  (:require [menard.parse :as parse]
            [menard.parse.word :as word]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; <mock data>
(defn lookup-fn [input-string]
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
(def expression "The White House Press Corps Dinner")
(def split-on #"[ ]")
(def max-word-length-in-tokens 3)
;; </mock-data>

;; <tests>
(deftest word-graph-test
  ;; TODO: add more tests for the graph
  (is (map? (word/graph expression split-on lookup-fn max-word-length-in-tokens))))

(deftest word-groupings-test
  (is (seq (word/groupings expression split-on lookup-fn max-word-length-in-tokens))))

;; </tests>
