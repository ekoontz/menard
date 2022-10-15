(ns menard.nederlands.complete
  (:require [dag_unify.core :as u]
            [clojure.tools.logging :as log]
            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]
            [menard.nesting]
            [menard.parse :as parse]
            [menard.subcat]))

(def model
  (ref (create "nederlands/models/complete"
               "complete"
               compile-lexicon false)))

(defn parse [expression]
  (parse/parse expression model))
