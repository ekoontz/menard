(ns menard.test.español.reflexives
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; https://es.wiktionary.org/wiki/quedarse#Conjugaci%C3%B3n

;; to reload after fixing something, do:
;; uncomment these:
(load "../../../../src/menard/subcat")
(load "../../../../src/menard/español")

(deftest analyze-test
  (let [analysis (analyze "quedarse")]
    (is (seq analysis)))
  (let [analysis (analyze "queda")]
    (is (seq analysis))))

(deftest parse-test
  (let [present (-> "Juan se queda" parse first)]
    (is (= (-> present syntax-tree)
           "[s(:present-simple){+} .Juan +[vp-pronoun(:present-simple){+} .se(2) +queda(:explicit-subj)]]")))
  (let [preterito (-> "yo me he lastimado" parse first)]
    "[s-aux(:preterito-perfecto){+} .yo +[vp-aux-reflexive-2(:preterito-perfecto){+} .me +[vp-aux-reflexive-1(:preterito-perfecto){+} +he(:explicit-subj) .lastimado(:explicit-subj)]]]"))




