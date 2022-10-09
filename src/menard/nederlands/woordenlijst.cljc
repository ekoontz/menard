(ns menard.nederlands.woordenlijst
  (:require [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]))

(def model
  (ref (create "nederlands/models/woordenlijst"
               "woordenlijst"
               compile-lexicon)))

(menard.model/install-the-usual-suspects)
