(ns menard.test.español.implicit-subject
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; These are for convenience so you don't
;; have to reload these language-model files every time you make a change:
;; you can just reload this file (menard.test.español.present.clj)
;; file and it will reload the changed language-model files.
(load "../../../../src/menard/subcat")
(load "../../../../src/menard/español")

;; https://es.wiktionary.org/wiki/comer#Conjugaci%C3%B3n

(deftest analyze-test
  (let [analysis (analyze "comido")]
    (is (seq analysis))
    (contains? (->> analysis (map #(u/get-in % [:subcat])) set) []) ;; implicit subject
    (contains? (->> analysis (map #(u/get-in % [:subcat :1 :case])) set) :nom) ;; explicit subject
    (is (= #{:participio} (->> analysis (map #(u/get-in % [:infl])) set)))))

(deftest parse-test-explicit-subject
  (let [non-reflexive (-> "yo he comido" parse first)]
    (is (= (-> non-reflexive syntax-tree)
           "[s-aux(:preterito-perfecto) .yo +[vp-aux-non-reflexive(:preterito-perfecto) +he(:explicit-subj-non-reflexive-intransitive) .comido]]"))
    (is (= (-> non-reflexive (u/get-in [:sem :aspect])) :perfect))
    (is (= (-> non-reflexive (u/get-in [:sem :tense])) :past)))
  (let [reflexive (-> "yo me he lastimado" parse first)]
    (is (= (-> reflexive syntax-tree)
           "[s-aux(:preterito-perfecto){+} .yo +[vp-aux-reflexive-3(:preterito-perfecto){+} .me +[vp-aux-reflexive-2(:preterito-perfecto){+} +he(:explicit-subj-reflexive) .lastimado]]]"))))

(deftest parse-test-implicit-subject
  (let [parses (->> "he comido" parse)]
    (not (empty? parses))))

(deftest generate-implicit-subject
  (let [spec {:root "dormir",
              :sem {:pred :sleep
                    :subj {:pred :i}
                    :tense :present
                    :aspect :simple},
              :subcat [],
              :cat :verb}]
    (is (= (-> spec generate morph) "duermo"))))

(deftest generate-implicit-subject
  (let [spec {:root "ver",
              :rule "s"
              :comp {:surface "lo"}
              :sem {:pred :see
                    :subj {:pred :i}
                    :obj {:pred :it}
                    :tense :present
                    :aspect :simple},
              :subcat [],
              :cat :verb}]
    (is (= (-> spec generate morph) "lo veo"))))
