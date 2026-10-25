(ns menard.test.español.direct-objects
  (:require [menard.español.curated-verbs :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is testing]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; if you made changes to these, you can uncomment them to reload them
;; so that in turn the below model will be reloaded with these changes:
(load "../../../../src/menard/subcat")
(load "../../../../src/menard/español/tenses")

;; reload the model every time to help with debugging the model:
(load "../../../../src/menard/español/curated_verbs")

(def model es/model)

(defn object-preds
  "the :sem :obj :pred of each parse of _expression_."
  [expression]
  (set (map #(u/get-in % [:sem :obj :pred]) (parse expression))))

(deftest generation
  (is (= "yo los veo"
         (->
          {:root "ver"
           :subcat []
           :reflexive? false
           :head {:comp {:reflexive? false}}
           :sem {:subj {:pred :i}
                 :tense :present
                 :aspect :simple
                 :obj {:pred :they
                       :gender :masc}}
           :rule "s"}
          es/generate
          es/morph))))


(deftest parse-direct-object-pronouns
  (testing "a clitic direct object pronoun is the object of the verb"
    (doseq [[expression pred] [["yo lo veo" :he]
                               ["yo la veo" :she]
                               ["yo los veo" :they]
                               ["yo las veo" :they]
                               ["él me ve" :i]
                               ["yo te veo" :you]
                               ["ella nos ve" :we]
                               ["yo os veo" :you]]]
      (is (seq (parse expression)) expression)
      (is (contains? (set (map #(u/get-in % [:sem :obj :pred]) (parse expression))) pred)
          (str expression " should have a direct object with :pred " pred)))))


(deftest play-takes-an-inanimate-object
  (testing "play takes a clitic direct object, but not a human one"
    (is (contains? (object-preds "yo lo toco") :it))
    (is (contains? (object-preds "yo lo juego") :it))
    (is (not (contains? (object-preds "yo lo toco") :he)))
    (is (not (contains? (object-preds "yo lo juego") :he))))
  (testing "the human clitics still work with other verbs"
    (is (contains? (object-preds "yo lo veo") :he)))
  (testing "the intransitive reading is kept"
    (is (seq (parse "yo juego")))
    (is (seq (parse "yo toco")))))

(deftest eat-takes-an-inanimate-object
  (testing "eat takes a clitic direct object, but not a human one"
    (is (contains? (object-preds "yo lo como") :it))
    (is (not (contains? (object-preds "yo lo como") :he))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "yo como")))))

(deftest bailar-is-intransitive
  (doseq [expression ["yo lo bailo" "yo la bailo"]]
    (is (empty? (parse expression)) expression)))
