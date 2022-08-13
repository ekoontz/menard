(ns menard.nederlands.woordenlijst
  (:require [menard.nederlands :as nl]
            [menard.model :as model]))

(def create-woordenlijst-model? true)

(if create-woordenlijst-model?
  (def woordenlijst-model
    (ref (model/create "woordenlijst"
                       nl/load-morphology
                       nl/load-lexicon-with-morphology
                       nl/load-lexicon
                       nl/load-grammar
                       nl/create-lexical-index
                       nl/fill-lexicon-indexes))))



