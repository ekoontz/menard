(ns babel.test.la
  (:require
   [babel.latin.morphology :refer [analyze conjugate]]
   [babel.latin :refer [lexicon]]
   [clojure.test :refer [deftest is]]))

(deftest analyze-ere
  (is (= :verb
         (-> (analyze "abeo" lexicon)
             first
             (get-in [:synsem :cat])))))

(deftest conjugate-ere
  (is (= "abamus"
         (conjugate "abēre"
                    {:synsem {:subcat {:1 {:agr {:number :plur
                                                 :person :1st}}}}}))))



