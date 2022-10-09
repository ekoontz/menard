(ns menard.nederlands.complete
  (:require [clojure.tools.logging :as log]
            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]
            [menard.parse :as parse]))

(def model
  (ref (create "nederlands/models/complete"
               "complete"
               compile-lexicon)))

(menard.model/install-the-usual-suspects model)
