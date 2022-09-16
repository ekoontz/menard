(ns menard.nederlands.woordenlijst
  (:require [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]))

(def create-model? true)

(if create-model?
  (def model
    (ref (create "nederlands/models/woordenlijst"
                 "woordenlijst"
                 compile-lexicon))))

