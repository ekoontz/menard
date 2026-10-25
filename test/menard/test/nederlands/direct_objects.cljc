(ns menard.test.nederlands.direct-objects
  (:require [menard.nederlands :as nl :refer [morph parse]]
            [menard.nederlands.complete :as complete]
            [menard.nederlands.tenses]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is testing]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn object-preds
  "the :sem :obj :pred of each parse of _expression_."
  [expression]
  (set (map #(u/get-in % [:sem :obj :pred]) (parse expression))))

(deftest parse-direct-object-pronouns
  (testing "a pronoun in the direct object position is the object of the verb"
    (doseq [[expression pred] [["ik zie hem" :he]
                               ["ik zie haar" :she]
                               ["ik zie het" :it]
                               ["ik zie ze" :they]
                               ["ik zie hen" :they]
                               ["ik zie hun" :they]
                               ["ik zie je" :you]
                               ["ik zie jou" :you]
                               ["ik zie u" :you]
                               ["ik zie jullie" :you]
                               ["wij zien hen" :they]
                               ["zij ziet hem" :he]]]
      (is (seq (parse expression)) expression)
      (is (contains? (object-preds expression) pred)
          (str expression " should have a direct object with :pred " pred))))
  (testing "a nominative pronoun is not used as a direct object"
    (doseq [expression ["ik zie hij" "ik zie zij" "ik zie wij"]]
      (is (empty? (parse expression)) expression))))

(deftest generate-direct-object-pronouns
  (testing "a direct object pronoun is generated in object position"
    (let [spec {:max-depth 2
                :reflexive? false
                :cat :verb
                :subcat []
                :sem {:mood :declarative
                      :polarity :plus
                      :pred :see
                      :obj {:obj :none
                            :pred :he}}
                :training-wheels {:head {:rule "vp"
                                         :head {:phrasal? false}}}}
          surfaces (repeatedly 5 #(some-> (nl/generate spec complete/model) morph))]
      (is (every? (fn [surface]
                    (and surface (re-find #" hem$" surface)))
                  surfaces)
          (str "generated: " (vec surfaces))))))
