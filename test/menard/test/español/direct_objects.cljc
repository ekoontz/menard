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

(deftest eat-and-think-take-an-inanimate-object
  (testing "eat and think take a clitic direct object, but not a human one"
    (doseq [expression ["yo lo como" "yo lo pienso"]]
      (is (contains? (object-preds expression) :it) expression)
      (is (not (contains? (object-preds expression) :he)) expression)))
  (testing "the intransitive reading is kept"
    (doseq [expression ["yo como" "yo pienso"]]
      (is (seq (parse expression)) expression))))

(deftest aprender-and-practicar-take-an-inanimate-object
  (testing "learn and practice take a clitic direct object, but not a human one"
    (doseq [expression ["yo lo aprendo" "yo la aprendo"
                        "yo lo practico" "yo la practico"]]
      (is (contains? (object-preds expression) :it) expression)
      (is (not (contains? (object-preds expression) :he)) expression)))
  (testing "the intransitive reading is kept"
    (doseq [expression ["yo aprendo" "yo practico"]]
      (is (seq (parse expression)) expression))))

(deftest hacer-takes-an-inanimate-object
  (testing "do and make take a clitic direct object, but not a human one"
    (doseq [expression ["yo lo hago" "yo la hago"]]
      (is (contains? (object-preds expression) :it) expression)
      (is (not (contains? (object-preds expression) :he)) expression)))
  (testing "the intransitive reading is kept"
    (is (seq (parse "yo hago")))))

(deftest abrir-takes-an-inanimate-object
  (testing "open takes a clitic direct object, but not a human one"
    (doseq [expression ["yo lo abro" "yo la abro"]]
      (is (contains? (object-preds expression) :it) expression)
      (is (not (contains? (object-preds expression) :he)) expression)))
  (testing "the intransitive reading is kept"
    (is (seq (parse "yo abro")))))

(deftest tener-takes-an-inanimate-object
  (testing "have takes a clitic direct object, but not a human one"
    (doseq [expression ["yo lo tengo" "yo la tengo"]]
      (is (contains? (object-preds expression) :it) expression)
      (is (not (contains? (object-preds expression) :he)) expression)))
  (testing "the intransitive reading is kept"
    (is (seq (parse "yo tengo")))))

(deftest beber-takes-an-inanimate-object
  (testing "drink takes a clitic direct object, but not a human one"
    (doseq [expression ["yo lo bebo" "yo la bebo"]]
      (is (contains? (object-preds expression) :it) expression)
      (is (not (contains? (object-preds expression) :he)) expression)))
  (testing "the intransitive reading is kept"
    (is (seq (parse "yo bebo"))))
  (testing "tomar's \"take\" sense is left unconstrained"
    (is (contains? (object-preds "yo lo tomo") :he))))

(deftest leer-takes-an-inanimate-object
  (testing "read takes a clitic direct object, but not a human one"
    (doseq [expression ["yo lo leo" "yo la leo"]]
      (is (contains? (object-preds expression) :it) expression)
      (is (not (contains? (object-preds expression) :he)) expression)))
  (testing "the intransitive reading is kept"
    (is (seq (parse "yo leo")))))

(deftest poner-takes-an-inanimate-object
  (testing "put takes a clitic direct object, but not a human one"
    (doseq [expression ["yo lo pongo" "yo la pongo"]]
      (is (contains? (object-preds expression) :it) expression)
      (is (not (contains? (object-preds expression) :he)) expression)))
  (testing "the intransitive reading is kept"
    (is (seq (parse "yo pongo")))))

(deftest bailar-is-intransitive
  (doseq [expression ["yo lo bailo" "yo la bailo"]]
    (is (empty? (parse expression)) expression)))

(deftest asistir-is-intransitive
  (doseq [expression ["yo lo asisto" "yo la asisto"]]
    (is (empty? (parse expression)) expression))
  (is (seq (parse "yo asisto"))))

(deftest comentar-is-transitive-only
  (testing "comentar needs a direct object, so the bare verb does not parse"
    (is (empty? (parse "yo comento")))
    (is (contains? (object-preds "yo lo comento") :it))
    (is (contains? (object-preds "yo lo comento") :he))))

(deftest entrar-takes-an-inanimate-object
  (testing "enter takes a clitic direct object, but not a human one"
    (doseq [expression ["yo lo entro" "yo la entro"]]
      (is (contains? (object-preds expression) :it) expression)
      (is (not (contains? (object-preds expression) :he)) expression)))
  (testing "the intransitive reading is kept"
    (is (seq (parse "yo entro")))))

(deftest trabajar-is-intransitive
  (testing "trabajar takes no direct object"
    (doseq [expression ["yo lo trabajo" "yo la trabajo"]]
      (is (empty? (parse expression)) expression)))
  (testing "the intransitive reading is kept"
    (is (seq (parse "yo trabajo")))))
