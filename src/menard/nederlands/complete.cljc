(ns menard.nederlands.complete
  (:require [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]))

(def model
  (ref (create "nederlands/models/complete"
               "complete"
               compile-lexicon)))

(menard.model/install-the-usual-suspects)
