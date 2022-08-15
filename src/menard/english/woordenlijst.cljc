(ns menard.english.woordenlijst
  (:require [menard.english.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]))

(def create-model? true)

(if create-model?
  (def model
    (ref (create "english/models/woordenlijst"
                 compile-lexicon))))
