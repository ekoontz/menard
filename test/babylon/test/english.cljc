(ns babylon.test.english
  (:require [babylon.english :as en :refer [analyze generate morph parse]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]))

(deftest parse-cat
  (is (not (empty? (analyze "cat"))))
  (is (not (empty? (parse "small cat"))))
  (is (not (empty? (parse "the small cat"))))
  (is (not (empty? (parse "the small cat sleeps"))))
  (let [I-see (parse "I see")]
    (is (not (empty? I-see)))
    (is (= 4 (count I-see)))
    (is (contains? (set (->> I-see (map :rule))) "s-slash"))
    (is (= (-> (->> I-see
                    (filter #(= "s-slash" (:rule %)))
                    (map #(u/get-in % [:sem])))
               first (u/get-in [:sem]) (get :obj))
           (-> (->> I-see
                    (filter #(= "s-slash" (:rule %)))
                    (map #(u/get-in % [:sem])))
               first (u/get-in [:subcat :1]) (get :sem)))))

  (is (not (empty? (parse "cat I see"))))
  (let [cat-I-see (parse "cat I see")]
    (is (= 2 (count cat-I-see)))
    (is (= "nbar2" (-> cat-I-see first :rule)))
    (is (= :see (-> cat-I-see first (u/get-in [:mod :first :pred]))))
    (is (empty? (-> cat-I-see first (u/get-in [:mod :rest])))))

  (is (not (empty? (parse "small cat I see"))))
  (let [parses (parse "small cat I see")]
    (is (= 2 (count parses)))
    (is (= "nbar" (-> parses first :rule)))
    (is (= :small (-> parses first (u/get-in [:mod :first :pred]))))
    (is (= :see (-> parses first (u/get-in [:mod :rest :first :pred]))))
    (is (empty? (-> parses first (u/get-in [:mod :rest :rest])))))

  (let [parses (parse "the small cat I see")]
    (is (= 2 (count parses)))
    (is (= "np" (-> parses first :rule)))
    (is (= :small (-> parses first (u/get-in [:mod :first :pred]))))
    (is (= :see (-> parses first (u/get-in [:mod :rest :first :pred]))))
    (is (empty? (-> parses first (u/get-in [:mod :rest :rest]))))))

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
