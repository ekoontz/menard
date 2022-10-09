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

;; TODO: these functions should be a macro in menard.model.
;; e.g. (menard.model/install-the-usual-suspects)

(defn analyze [expression]
  (parse/analyze expression model))

(defn generate [expression]
  (generate/generate expression model))

(defn morph [expression]
  (serialization/morph expression model))

(defn parse [expression]
  (parse/parse expression model))

(defn syntax-tree [expression]
  (serialization/syntax-tree expression model))

