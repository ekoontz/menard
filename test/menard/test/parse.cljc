(ns menard.test.parse
  (:require [menard.parse :as parse]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest word-glue
  (let [bits  [     "1"       "0"         "1"         "0"         "1"        ]
        words ["The"   "White"    "House"     "Press"     "Corps"    "Dinner"]
        tokens []
        current-token []
        next-word nil]
    (is (= 0 0))
    (is (= (parse/word-glue bits words tokens current-token next-word 0)
           [["The"] ["White" "House"] ["Press" "Corps"] ["Dinner"]]))))

