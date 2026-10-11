(ns menard.test.italiano.morphology
  (:require [menard.italiano :as it]
            [menard.morphology :refer [morph-leaf]]
            [clojure.test :refer [deftest is testing]]))

(def morphology (:morphology @it/model))

(defn inflect [canonical infl agr]
  (morph-leaf {:canonical canonical
               :cat :verb
               :infl infl
               :agr agr}
              morphology))

(deftest present-indicative
  (testing "first conjugation (-are) verbs"
    (is (= "parlo" (inflect "parlare" :present {:number :sing :person :1st})))
    (is (= "parlano" (inflect "parlare" :present {:number :plur :person :3rd}))))
  (testing "second conjugation (-ere) verbs"
    (is (= "credo" (inflect "credere" :present {:number :sing :person :1st})))
    (is (= "credono" (inflect "credere" :present {:number :plur :person :3rd})))))

(deftest nouns-do-not-generate-inflected-forms
  ;; babel italiano had no :g (generation) pairs for noun patterns, so the
  ;; converted rules fall back to the canonical form, as in babel.
  (is (= "cane" (morph-leaf {:canonical "cane"
                             :cat :noun}
                            morphology))))

(deftest non-matching-agreement-falls-back-to-a-string
  (testing "an agreement with no matching rule still yields a surface form"
    (is (string? (inflect "parlare" :present {:number :plur :person :1st})))))
