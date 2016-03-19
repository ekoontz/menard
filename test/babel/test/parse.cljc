(ns ^{:doc "Parsing Testing Code"}
    babel.test.parse
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.forest :as forest]
            [babel.francais.grammar :refer [small medium]]
            [babel.francais.lexicon :refer [lexicon]]
            [babel.francais.morphology :refer [analyze fo replace-patterns]]
            [babel.francais.workbook :as workbook]
            [babel.over :as over]
            [babel.parse :refer [parse tokenizer]]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [fail-path fail? get-in strip-refs unifyc]]))

(deftest split
  (is (= 2 (count (string/split "je suis" tokenizer)))))
