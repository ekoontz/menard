(ns menard.test.italiano.direct-objects
  (:require [menard.italiano :as it :refer [analyze morph parse syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is testing]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn object-preds
  "the :sem :obj :pred of each parse of _expression_."
  [expression]
  (set (map #(u/get-in % [:sem :obj :pred]) (parse expression))))

(deftest parse-direct-object-pronouns
  (testing "a clitic direct object pronoun is the object of the verb"
    (doseq [[expression pred] [["lo vedo" :he]
                               ["la vedo" :she]
                               ["li vedo" :they]
                               ["le vedo" :they]
                               ["mi vede" :i]
                               ["ti vedo" :you]
                               ["ci vede" :we]
                               ["vi vedo" :you]]]
      (is (seq (parse expression)) expression)
      (is (contains? (object-preds expression) pred)
          (str expression " should have a direct object with :pred " pred))))
  (testing "with an explicit subject"
    (doseq [[expression pred] [["io lo vedo" :he]
                               ["io li vedo" :they]
                               ["il cane lo vede" :he]]]
      (is (seq (parse expression)) expression)
      (is (contains? (object-preds expression) pred)
          (str expression " should have a direct object with :pred " pred))))
  (testing "a nominative pronoun is not used as a clitic direct object"
    (is (empty? (parse "io vedo lui"))))
  (testing "a full noun phrase direct object lands in :sem :obj too"
    (is (contains? (set (map #(u/get-in % [:sem :obj :pred]) (parse "il cane vede il gatto")))
                   :cat))))
