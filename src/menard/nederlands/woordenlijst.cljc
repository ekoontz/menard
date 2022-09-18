(ns menard.nederlands.woordenlijst
  (:require [dag_unify.core :as u]
            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]))

(def model
  (ref (create "nederlands/models/woordenlijst"
               "woordenlijst"
               compile-lexicon)))

(defn analyze [token]
  (let [use-null-tokens? false]
    (menard.nederlands/analyze token use-null-tokens? model)))

(defn parse [expression]
  (menard.nederlands/parse expression model))

(defn generate [spec]
  (menard.nederlands/generate expression model))

