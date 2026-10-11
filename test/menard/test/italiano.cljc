(ns menard.test.italiano
  (:require [menard.italiano :as it]
            [clojure.test :refer [deftest is testing]]))

(def model @it/model)

(deftest model-loads
  (testing "the italiano/basic model loads with all three resource groups"
    (is (map? model))
    (is (seq (:morphology model)))
    (is (seq (:grammar model)))
    (is (seq (:lexicon model)))))

(deftest morphology-rules-are-well-formed
  (testing "every morphology rule has a 2-element [regex replacement] :g and :p"
    (is (every? (fn [rule]
                  (and (= 2 (count (:g rule)))
                       #?(:clj (instance? java.util.regex.Pattern (first (:g rule)))
                          :cljs true)
                       (string? (second (:g rule)))
                       (= 2 (count (:p rule)))
                       #?(:clj (instance? java.util.regex.Pattern (first (:p rule)))
                          :cljs true)
                       (string? (second (:p rule)))))
                (:morphology model)))))

(deftest multi-pair-rules-were-split
  (testing "each babel [regex replacement] pair became its own rule"
    ;; present tense had 10 babel rules carrying several pairs each;
    ;; after splitting there are many more menard rules for that tense.
    (is (< 10 (count (filter #(= :present (:infl (:u %))) (:morphology model)))))))

(deftest grammar-tense-options-expanded
  (testing "the tense-specific rules were merged onto :options and expanded"
    (let [grammar (:grammar model)]
      (is (= 7 (count (filter #(= "s" (:rule %)) grammar))))
      (is (= 7 (count (filter #(= "vp" (:rule %)) grammar)))))))

(deftest lexicon-categories
  (testing "the ported lexicon has entries in the expected categories"
    (let [cats (frequencies (map :cat (mapcat val (:lexicon model))))]
      (is (pos? (get cats :verb 0)))
      (is (pos? (get cats :noun 0)))
      (is (pos? (get cats :adjective 0)))
      (is (pos? (get cats :det 0))))))

(deftest lexicon-has-known-verbs
  (testing "well-known italian verbs are present in the lexicon"
    (doseq [verb ["parlare" "credere" "avere" "essere"]]
      (is (contains? (:lexicon model) verb)))))
