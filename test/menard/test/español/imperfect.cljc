(ns menard.test.español.imperfect
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def developer-mode? false)
(when developer-mode? 
  ;; These are for convenience so you don't
  ;; have to reload these language-model files every time you make a change:
  ;; you can just reload this file (menard.test.español.preterito-perfecto.clj)
  ;; file and it will reload the changed language-model files.
  (load "../../../../src/menard/subcat")
  (load "../../../../src/menard/español"))

;; https://es.wiktionary.org/wiki/comer#Conjugaci%C3%B3n

(deftest analyze-test
  (let [analysis (analyze "veía")]
    (is (seq analysis))
    (is (= #{true} (->> analysis (map map?) set)))
    (is (= #{:imperfect} (->> analysis (map #(u/get-in % [:infl])) set)))))
