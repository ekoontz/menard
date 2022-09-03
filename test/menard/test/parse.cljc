(ns menard.test.parse
  (:require [menard.parse :as parse]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn lookup-fn [input-string]
  (cond
    (or (= input-string "The")
        (= input-string "White House")
        (= input-string "Press Corps")
        (= input-string "Dinner"))
    [{:top :top}]

    :else []))

(deftest word-glue
  (is (contains?
       (set
        (menard.parse/all-groupings "The White House Press Corps Dinner" lookup-fn))
       '("The" "White House" "Press Corps" "Dinner"))))






    
