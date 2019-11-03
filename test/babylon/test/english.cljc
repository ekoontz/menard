(ns babylon.test.english
  (:require [babylon.english :as en :refer [analyze expressions generate morph parse syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]))

(deftest parse-cat
  (is (not (empty? (analyze "cat"))))
  (is (not (empty? (parse "small cat"))))
  (is (not (empty? (parse "the small cat"))))
  (is (not (empty? (parse "the small cat sleeps"))))

  (let [string "I see"
        expected-trees
        ["[s(:present-simple) .I +see]"
         "[s-comp .I +see]"
         "[s-comp-2 .I +see]"
         "[s-slash(:present-simple) .I +see]"]]
    (let [parses (set (map syntax-tree (parse string)))]
     (doall (map (fn [expected-tree]
                   (is (contains? parses expected-tree)))
                 expected-trees))))

  (let [string "cat I see"
        expected-trees
        ["[nbar2 +cat .[s-slash(:present-simple) .I +see]]"]]
    (let [parses (set (map syntax-tree (parse string)))]
     (doall (map (fn [expected-tree]
                   (is (contains? parses expected-tree)))
                 expected-trees))))

  (let [string "the small cat I see"
        expected-trees
        ["[np .the +[nbar .small +[nbar2 +cat .[s-slash(:present-simple) .I +see]]]]"]]
    (let [parses (set (map syntax-tree (parse string)))]
     (doall (map (fn [expected-tree]
                   (is (contains? parses expected-tree)))
                 expected-trees)))))

;; [s-interog did [s-comp .she *sleep]]
(deftest s-comp
  (let [parses (parse "she sleep")]
    (is (= 1 (count parses)))
    (is (= "s-comp" (u/get-in (first parses) [:rule])))))

(deftest s-interog
  (is (not (empty? (parse "did she sleep")))))

(deftest prepositional-phrases
  (is (not (empty? (parse "she puts the cat he sees on the table")))))

(deftest ditransitive-phrase
  (let [expression
        (generate
         {:sem {:tense :present
                :aspect :simple
                :pred :put-on
                :subj {:pred :girl}
                :iobj {:pred :table}
                :obj {:pred :cat}}
          :cat :verb
          :rule "s"})]
    (is (not (empty? expression)))
    (is (not (empty? (morph expression))))))


(deftest all-expressions-work
  (let [expressions
        (->>
         (range 0 (count expressions))
         (pmap (fn [index]
                 (generate (nth expressions index)))))])
  (is (empty? (filter empty? expressions))))

       
