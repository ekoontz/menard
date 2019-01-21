(ns babylon.test.english
  (:require [babylon.english :as en :refer [analyze parse]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]))

(deftest parse-cat
  (is (not (empty? (analyze "cat"))))
  (is (not (empty? (parse "small cat"))))
  (is (not (empty? (parse "the small cat"))))
  (is (not (empty? (parse "the small cat sleeps"))))
  (let [I-see (parse "I see")]
    (is (not (empty? I-see)))
    (is (= 2 (count I-see)))
    (is (contains? (set (->> I-see (map :rule))) "s-slash"))
    (is (= (-> (->> I-see (filter #(= "s-slash" (:rule %))) (map #(u/get-in % [:sem])))
               first (u/get-in [:sem]) (get :obj))
           (-> (->> I-see (filter #(= "s-slash" (:rule %))) (map #(u/get-in % [:sem])))
               first (u/get-in [:subcat :1]) (get :sem))))))

  (is (not (empty? (parse "cat I see"))))
;  (is (not (empty? (parse "small cat I see"))))
;  (is (not (empty? (parse "the small cat I see"))))
;  (is (not (empty? (parse "the small cat I see sleeps")))))

