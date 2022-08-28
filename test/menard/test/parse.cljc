(ns menard.test.parse
  (:require [menard.parse :as parse]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest word-glue
  (let [bits1  [     "1"       "0"         "1"         "0"         "1"        ]


        words  ["The"   "White"    "House"     "Press"     "Corps"    "Dinner"]

        
        bits2  [     "1"       "0"         "0"         "0"         "1"        ]

        tokens []
        current-token []]
    (is (= (parse/word-glue bits1 words tokens current-token 0)
           [["The"] ["White" "House"] ["Press" "Corps"] ["Dinner"]]))
    (is (= (parse/word-glue bits2 words tokens current-token 0)
           [["The"] ["White" "House" "Press" "Corps"] ["Dinner"]]))


    (is (= (parse/word-glue-wrapper "The White House Press Corps" 21)
           ["The" "White House" "Press Corps" "Dinner"]))))
