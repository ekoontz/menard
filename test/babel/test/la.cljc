(ns babel.test.la
  (:require
   [babel.latin.morphology :refer [analyze conjugate]]
   [babel.latin :refer [model]]
   [clojure.test :refer [deftest is]]))

(def lexicon (:lexicon model))
(def generate (:generate-fn model))
(def fo (:fo model))

;; https://en.wikipedia.org/wiki/Latin_conjugation#Present_indicative
(deftest analyze-ere
  (is (= :verb
         (-> (analyze "ardeo" lexicon)
             first
             (get-in [:synsem :cat])))))

(deftest conjugate-ere
  (is (= "ardemus"
         (conjugate "ardēre"
                    {:synsem {:sem {:subj {:pred :noi}}}}))))

(deftest generate-1
  (is (= "ardetis"
         (fo (generate
              {:root "ardēre"
               :synsem {:sem {:subj {:pred :voi}}}})))))






