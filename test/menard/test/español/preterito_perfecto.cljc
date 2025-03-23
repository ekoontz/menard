(ns menard.test.español.preterito-perfecto
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; to reload after fixing something, do:
;; uncomment these:
(load "../../../../src/menard/subcat")
(load "../../../../src/menard/español")

(def spec
  {:cat :verb
   :subcat []
   :reflexive? false
   :sem {:pred :eat
         :subj {:pred :i}
         :tense :past
         :aspect :perfect}})

;; https://es.wiktionary.org/wiki/comer#Conjugaci%C3%B3n

(deftest analyze-test
  (let [analysis (analyze "comido")]
    (is (seq analysis))
    (is (= 1 (count analysis)))
    (is (map? (-> analysis first)))
    (is (= :participio (-> analysis first (u/get-in [:infl]))))))

(deftest parse-test-explicit-subject
  (let [non-reflexive (-> "yo he comido" parse first)]
    (is (= (-> non-reflexive syntax-tree)
           "[s-aux(:preterito-perfecto) .yo +[vp-aux-non-reflexive(:preterito-perfecto) +he(:explicit-subj-non-reflexive) .comido]]"))
    (is (= (-> non-reflexive (u/get-in [:sem :aspect])) :perfect))
    (is (= (-> non-reflexive (u/get-in [:sem :tense])) :past)))
  (let [reflexive (-> "yo me he lastimado" parse first)]
    (is (= (-> reflexive syntax-tree)
           "[s-aux(:preterito-perfecto){+} .yo +[vp-aux-reflexive-3(:preterito-perfecto){+} .me +[vp-aux-reflexive-2(:preterito-perfecto){+} +he(:explicit-subj-reflexive) .lastimado(:explicit-subj)]]]"))))

(deftest parse-test-null-subject

  ;; Does not yet work: requires verbs to have a null-subject lexical rule
  ;; that produces all 4 of the following subcat frames:
  ;; 1. <> (intransitive and implicit subject)
  ;; 2. <noun(subj)> (intransitive and explicit subject)
  ;; 3. <noun(obj)> (transitive and implicit subject)
  ;; 4. <noun(subj), noun(obj)> (transitive and explicit subject)
  ;; Adding this lexical rule will likely simplify many of the rules in grammar.edn.
  ;;  {:rule "s-aux-nonreflexive-subj-implicit" ;; "he comido"
  ;;  (let [non-reflexive (-> "he comido" parse first)]
  ;;    (is (= (-> non-reflexive syntax-tree)
  ;;           "[s-aux(:preterito-perfecto) +he .comido]")))

  (let [reflexive (-> "me he lastimado" parse first)]
    (is (= (-> reflexive syntax-tree)
           "[s-aux(:preterito-perfecto){+} .me +[vp-aux-reflexive-1(:preterito-perfecto){+} +he(:implicit-subj-reflexive) .lastimado(:implicit-subj)]]"))
    (is (= (-> reflexive (u/get-in [:sem :aspect])) :perfect))
    (is (= (-> reflexive (u/get-in [:sem :tense])) :past))))

(deftest generate-test
  (is (= (-> spec generate syntax-tree)
         "[s-aux(:preterito-perfecto) .yo +[vp-aux-non-reflexive(:preterito-perfecto) +he(:explicit-subj-non-reflexive) .comido]]")))
