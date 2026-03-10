(ns menard.test.español.present
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
;; you can just reload this file (menard.test.español.preterito-perfecto.clj)
;; file and it will reload the changed language-model files.
(load "../../../../src/menard/subcat")
(load "../../../../src/menard/español")

;; https://es.wiktionary.org/wiki/comer#Conjugaci%C3%B3n

(deftest exception-test
  (let [spec {:sem
              {:obj :none,
               :subj
               {:existential? false,
                :mod [],
                :ref {:human? true, :number :sing},
                :pred :i},
               :mod [],
               :pred :obtain-c,
               :aspect :simple,
               :tense :present},
              :subcat [],
              :cat :verb}]
    (is (= 
         (->> spec es/generate-all (map es/morph) set)
         #{"yo consigo"}))))

