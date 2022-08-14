(ns menard.nederlands.woordenlijst
  (:require [menard.nederlands :as nl]
            [menard.model :refer [create]]))

(def create-woordenlijst-model? true)

(if create-woordenlijst-model?
  (def model
    (ref (create "nederlands/models/woordenlijst"
                 nl/load-lexicon-with-morphology
                 nl/load-lexicon))))
