(ns babylon.test.nederlands
  (:require [babylon.nederlands :as nl :refer [analyze expressions generate morph parse syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            [clojure.tools.logging :as log]))

(deftest adjective-agreement
  (is (= "het oude huis"
         (morph (generate {:rule "np"
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :def}
                           :mod {:first {:pred :old}}
                           :head {:rule "nbar"}}))))
  (is (= "een oud huis"
         (morph (generate {:rule "np"
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :indef}
                           :mod {:first {:pred :old}}
                           :head {:rule "nbar"}}))))
  (is (= "de oude huizen"
         (morph (generate {:rule "np"
                           :root "huis"
                           :agr {:number :plur}
                           :mod {:first {:pred :old}}
                           :head {:rule "nbar"}})))))

(deftest all-expressions-work
  (let [generate-per-expression 5
        expression-sets
        (->>
         (range 0 (count expressions))
         (pmap (fn [index]
                 (take generate-per-expression (repeatedly #(generate (nth expressions index)))))))]
    (is (= (count expressions)
           (count expression-sets)))
    (is (empty?
         (->> expression-sets
              (map (fn [expression-set]
                     (->> expression-set
                          (map (fn [expression]
                                 (log/info (str (morph expression)))
                                 (parse (morph expression)))))))
              (map count)
              (remove #(= % generate-per-expression)))))))
