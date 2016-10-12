(ns babel.test.la
  (:require
   [babel.latin.morphology :refer [analyze conjugate]]
   [babel.latin :refer [model]]
   [clojure.test :refer [deftest is]]))

(def lexicon (:lexicon model))
(def generate (:generate model))

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

(deftest generate-1
  (is (= "circumetis"
         (generate
          {:synsem {:sem {:pred :go-around
                          :subj {:pred :voi}}}}))))




