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

(defn object-humans
  "the :sem :obj :ref :human? of each parse of _expression_."
  [expression]
  (set (map #(u/get-in % [:sem :obj :ref :human?]) (parse expression))))

(deftest play-takes-an-inanimate-object
  (testing "play takes a direct object, but not a human one"
    (doseq [expression ["ik speel hem" "ik speel haar" "ik speel hen"]]
      (is (not (contains? (object-humans expression) true)) expression)))
  (testing "the inanimate reading of the pronoun is kept"
    (is (contains? (object-humans "ik speel het") false)))
  (testing "the human object pronouns still work with other verbs"
    (is (contains? (object-humans "ik zie hem") true))))

(deftest eten-takes-an-inanimate-object
  (testing "eat takes a direct object, but not a human one"
    (doseq [expression ["ik eet hem" "ik eet haar" "ik eet hen"]]
      (is (not (contains? (object-humans expression) true)) expression)))
  (testing "the inanimate reading of the pronoun is kept"
    (is (contains? (object-humans "ik eet het") false)))
  (testing "the object-free reading is kept"
    (is (seq (parse "ik eet")))))

(deftest maken-takes-an-inanimate-object
  (testing "make takes a direct object, but not a human one"
    (doseq [expression ["ik maakte hem" "ik maakte haar" "ik maakte hen"]]
      (is (not (contains? (object-humans expression) true)) expression)))
  (testing "the inanimate reading of the pronoun is kept"
    (is (contains? (object-humans "ik maakte het") false)))
  (testing "an inanimate noun phrase is still an object"
    (is (contains? (set (map #(u/get-in % [:sem :obj :pred]) (parse "ik maakte een boek")))
                   :book))))
