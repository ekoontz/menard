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

(def developer-mode? false)
(when developer-mode? 
  ;; to reload after fixing something, do:
  ;; uncomment these:
  (load "../../../../src/menard/subcat")
  (load "../../../../src/menard/español"))

(deftest analyze-test
  (let [analysis (analyze "quedarse")]
    (is (seq analysis)))
  (let [analysis (analyze "queda")]
    (is (seq analysis))))

(deftest parse-test
  (let [present (-> "Juan se queda" parse first)]
    (is (= (-> present syntax-tree)
           "[s-head-last(:present-simple){+} .Juan +[vp-pronoun-c(:present-simple){+} .se(2) +queda]]")))
  (let [preterito (-> "yo me he lastimado" parse first)]
    (is (= (-> preterito syntax-tree)
           "[s-head-last(:preterito-perfecto){+} .yo +[vp-pronoun-c(:preterito-perfecto){+} .me +[vp-aux-k(:preterito-perfecto){+} +he(:explicit-subj-reflexive) .lastimado]]]"))))
