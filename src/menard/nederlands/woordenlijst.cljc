(ns menard.nederlands.woordenlijst
  (:require [menard.nederlands :refer [compile-lexicon]]
            [menard.model :refer [create]]))

(def create-model? true)

(if create-model?
  (def model
    (ref (create "nederlands/models/woordenlijst"
                 compile-lexicon))))

