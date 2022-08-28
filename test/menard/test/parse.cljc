(ns menard.test.parse
  (:require [menard.parse :as parse]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest word-glue
  (is (contains?
       (set
        (menard.parse/all-groupings "The White House Press Corps Dinner"))
       '("The" "White House" "Press Corps" "Dinner"))))



    
