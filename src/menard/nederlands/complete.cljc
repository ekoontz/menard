(ns menard.nederlands.complete
  (:require [dag_unify.core :as u]
            [clojure.tools.logging :as log]
            [menard.generate :as generate]
            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]
            [menard.nesting]
            [menard.parse :as parse]
            [menard.serialization :as serialization]
            [menard.subcat]))

(def model
  (ref (create "nederlands/models/complete"
               "complete"
               compile-lexicon)))

(menard.model/install-the-usual-suspects)
