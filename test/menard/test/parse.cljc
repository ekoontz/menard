(ns menard.test.parse
  (:require [menard.parse :as parse]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

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

(def split-on #"[ ]")

(deftest word-glue
  (is (contains?
       (set
        (menard.parse/all-groupings "The White House Press Corps Dinner" split-on
                                    lookup-fn))
       '("The" "White House" "Press Corps" "Dinner"))))

(defn foo []
  (let [unify u/unify!
        the (keyword "the")
        white (keyword "white")
        house (keyword "house")
        press (keyword "press")
        corps (keyword "corps")
        dinner (keyword "dinner")        
        white-house (keyword "white house")
        press-corps (keyword "press corps")]
    (->
     ;; 1. add individual words:
     {the
      {white
       {house
        {press
         {corps
          {dinner :top}}}}}}

     ;; 2. add compound words:
     (unify {the
             {white-house :top}})
     (unify {the
             {white
              {house
               {press-corps :top}}}})

     ;; 3. make reentrances to "press corps":
     (unify (let [press-corps (atom :top)]
              {the {white {house press-corps}
                    white-house press-corps}}))

     ;; 4. make reentrances to "dinner":
     (unify (let [dinner (atom :top)]
              {the {white {house {press {corps dinner}}}
                    white-house {press-corps dinner}}}))
     )))



                  


        





           






