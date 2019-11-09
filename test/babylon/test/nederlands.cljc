(ns babylon.test.nederlands
  (:require [babylon.nederlands :as nl :refer [analyze expressions generate morph parse syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]))

(deftest all-expressions-work
  (let [expressions
        (->>
         (range 0 (count expressions))
         (pmap (fn [index]
                 (generate (nth expressions index)))))]
    (is (empty? (filter empty? expressions)))
    (is (empty? (filter empty? (map (fn [expression]
                                      (-> expression
                                          morph
                                          parse))
                                    expressions))))))
