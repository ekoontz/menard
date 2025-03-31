(ns menard.test.español.noun-phrases
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; These are for convenience so you don't
;; have to reload the language-model files every time you make a change:
;; you can just reload this (i.e. menard.test.español.nouns)
;; file and it will reload the changed language-model files.
(load "../../../../src/menard/español")

(deftest analyze-test
  (let [analysis (-> "gato" analyze first)]
    (is (= :noun (u/get-in analysis [:cat])))))

(deftest parse-tests
  (is (= "[np .el +aqua]" (->> "el aqua" parse (map syntax-tree) first)))
  (is (= "[np .el +gato]" (->> "el gato" parse (map syntax-tree) first)))
  (is (= "[np .la +mesa]" (->> "la mesa" parse (map syntax-tree) first)))
  (is (empty? (->> "la gato" parse)))
  (is (empty? (->> "la aqua" parse)))
  (is (empty? (->> "el mesa" parse))))
  
