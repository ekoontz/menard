(ns menard.nederlands.woordenlijst
  (:require [menard.nederlands :as nl]
            [menard.model :refer [create]]))

(def create-model? true)

(if create-model?
  (def model
    (ref (create "nederlands/models/woordenlijst"
                 nl/load-lexicon-with-morphology))))
