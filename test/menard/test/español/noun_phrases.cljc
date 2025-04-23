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
  (is (= (->> "el aqua" parse (map syntax-tree) first)
         "[np .el +aqua]"))
  (is (= (->> "el gato" parse (map syntax-tree) first)
         "[np .el +gato]"))
  (is (= (->> "los gatos" parse (map syntax-tree) first)
         "[np .los +gatos]"))
  (is (= (->> "el gato negro" parse (map syntax-tree) first)
         "[np .el +[nbar +gato .negro]]"))
  (is (= (->> "los gatos negros" parse (map syntax-tree) first)
         "[np .los +[nbar +gatos .negros]]"))
  (is (= (->> "la mesa" parse (map syntax-tree) first)
         "[np .la +mesa]"))
  (is (= (->> "la mesa blanca" parse (map syntax-tree) first)
         "[np .la +[nbar +mesa .blanca]]"))
  (is (= (->> "el primero gato" parse (map syntax-tree) first)
         "[np .el +[nbar .primero +gato]]"))
  (is (= (->> "la mesa blanca" parse (map #(u/get-in % [:sem])) (map l/pprint) vec)
         [{:pred :table
           :context :top
           :quant :the
           :ref {:number :sing}
           :mod {:first {:pred :white}
                 :rest []}}]))
  (is (= (->> "el primero gato" parse (map #(u/get-in % [:sem])) (map l/pprint) vec)
         [{:pred :cat
           :context :top
           :quant :the
           :ref {:number :sing}
           :mod {:first {:pred :first}
                 :rest []}}]))

  ;; negative tests: things that shouldn't parse:
  (is (empty? (->> "la gato" parse)))
  (is (empty? (->> "la aqua" parse)))
  (is (empty? (->> "el mesa" parse)))
  (is (empty? (->> "el gato negra" parse)))
  (is (empty? (->> "la mesa blanco" parse)))
  (is (empty? (->> "la blanca mesa" parse)))
  (is (empty? (->> "la mesa primera" parse))))

(deftest generate-test
  (is (= (-> {:cat :noun :phrasal? true
              :sem {:pred :cat
                    :quant :the
                    :ref {:number :sing}
                    :mod {:first {:pred :black}}}
              :subcat []}
             generate
             morph)
         "el gato negro"))
  (is (= (-> {:cat :noun :phrasal? true
              :sem {:pred :cat
                    :quant :the
                    :ref {:number :plur}                    
                    :mod {:first {:pred :black}}}
              :subcat []}
             generate
             morph)
         "los gatos negros")))
