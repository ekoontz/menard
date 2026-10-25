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

(deftest play-takes-an-inanimate-object
  (testing "play takes a clitic direct object, but not a human one"
    (doseq [expression ["lo gioco" "li gioco" "lo suono" "le suono"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human])) (parse expression))
          expression)))
  (testing "the inanimate reading of the clitic is kept"
    (is (some #(false? (u/get-in % [:sem :obj :human])) (parse "lo gioco"))))
  (testing "the human clitics still work with other verbs"
    (is (some #(true? (u/get-in % [:sem :obj :human])) (parse "lo vedo"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "gioco")))
    (is (seq (parse "suono")))))

(deftest mangiare-takes-an-inanimate-object
  (testing "the human clitic is rejected, the inanimate one is not"
    (doseq [expression ["lo mangio" "la mangio" "li mangio"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human])) (parse expression))
          expression))
    (is (some #(false? (u/get-in % [:sem :obj :human])) (parse "lo mangio"))))
  (testing "an edible noun phrase is still an object"
    (is (some #(= :cipolla (u/get-in % [:sem :obj :pred])) (parse "mangio la cipolla"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "mangio")))))

(deftest ballare-is-intransitive
  (doseq [expression ["lo ballo" "li ballo" "la ballo"]]
    (is (empty? (parse expression)) expression)))
