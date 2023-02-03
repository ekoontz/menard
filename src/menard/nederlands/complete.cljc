(ns menard.nederlands.complete
  (:require [dag_unify.core :as u]
            [clojure.tools.logging :as log]
            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]
            [menard.nesting]
            [menard.parse :as parse]
            [menard.serialization :as s]
            [menard.subcat]))

(def model {})

(defn parse [expression]
  (parse/parse expression model))

(defn load-model []
  (def model (ref (create "nederlands/models/complete"
                          "complete"
                          compile-lexicon true))))

(defn reload-model-keep-existing-lexicon []
  (def model
    (ref (create "nederlands/models/complete"
                 "complete"
                 compile-lexicon true {:existing-model @model
                                       :use-existing-lexicon? true}))))

(defn reload-model-refresh-lexicon-with-derivations []
  (def model
    (ref (create "nederlands/models/complete"
                 "complete"
                 compile-lexicon true {:use-existing-lexicon? false
                                       :include-derivation? true}))))

(defn reload-model-refresh-lexicon-without-derivations []
  (def model
    (ref (create "nederlands/models/complete"
                 "complete"
                 compile-lexicon true {:use-existing-lexicon? false
                                       :include-derivation? false}))))

(defn reload-model-refresh-lexicon []
  (def model
    (ref (create "nederlands/models/complete"
                 "complete"
                 compile-lexicon true {:use-existing-lexicon? false}))))

(defn reload []
  (reload-model-keep-existing-lexicon))

(if false
  (reload-model-refresh-lexicon-with-derivations)
  (reload-model-refresh-lexicon-without-derivations))


