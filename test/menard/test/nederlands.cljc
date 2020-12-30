(ns menard.test.nederlands
  (:require [menard.nederlands :as nl :refer [analyze expressions generate load-model morph parse syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest adjective-agreement
  (is (= "het oude huis"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :the
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}
                           :training-wheels {:comp {:cat :det}
                                             :sem {:mod {:first {:number? false}}}}}))))
  (is (= "een oud huis"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :some
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}))))
  (is (= "de oude huizen"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :plur}
                           :sem {:quant :the
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}
                           :training-wheels {:comp {:cat :det}
                                             :sem {:mod {:first {:number? false}}}}})))))

(deftest all-expressions-work
  (let [generate-per-expression 5

        ;; exclude non-production expressions:
        expressions (->> expressions
                         (filter #(= true (u/get-in % [:prod?] true))))

        expression-sets
        (->>
         (range 0 (count expressions))
         (pmap (fn [index]
                 (take generate-per-expression
                       (repeatedly #(generate (nth expressions index)))))))]
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

(deftest morphology
  (is (= "zeeën"
         (menard.morphology/morph-leaf
          {:cat :noun
           :null? false
           :agr {:number :plur}
           :canonical "zee"}
          (-> menard.nederlands/model deref :morphology))))
  (is (= "honden"
         (menard.morphology/morph-leaf
          {:cat :noun
           :null? false
           :agr {:number :plur}
           :canonical "hond"}
          (-> menard.nederlands/model deref :morphology))))
  (is (= "opdrachten"
         (menard.morphology/morph-leaf
          {:cat :noun
           :null? false
           :agr {:number :plur}
           :canonical "opdracht"}
          (-> menard.nederlands/model deref :morphology)))))







