(ns babylon.test.english
  (:require [babylon.english :as en :refer [analyze parse]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]))

(deftest parse-cat
  (is (not (empty? (analyze "cat"))))
  (is (not (empty? (parse "small cat"))))
  (is (not (empty? (parse "the small cat"))))
  (is (not (empty? (parse "the small cat sleeps")))))
