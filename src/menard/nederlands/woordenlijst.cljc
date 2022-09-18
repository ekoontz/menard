(ns menard.nederlands.woordenlijst
  (:require [dag_unify.core :as u]
            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]))

(def model
  (ref (create "nederlands/models/woordenlijst"
               "woordenlijst"
               compile-lexicon)))
