(ns babel.test.la
  (:require
   [babel.latin.morphology :refer [analyze conjugate]]
   [babel.latin :refer [lexicon]]
   [clojure.test :refer [deftest is]]))

;; https://en.wikipedia.org/wiki/Latin_conjugation#Present_indicative
(deftest analyze-ere
  (is (= :verb
         (-> (analyze "abeo" lexicon)
             first
             (get-in [:synsem :cat])))))

(deftest conjugate-ere
  (is (= "abemus"
         (conjugate "abēre"
                    {:synsem {:sem {:subj {:pred :noi}}}}))))




