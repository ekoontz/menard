(ns menard.nederlands.complete
  (:require [dag_unify.core :as u]
            [clojure.tools.logging :as log]
            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]
            [menard.nesting]
            [menard.subcat]))

(def model
  (ref (create "nederlands/models/complete"
               "complete"
               compile-lexicon)))

(defn analyze [token]
  (let [use-null-tokens? false]
    (menard.nederlands/analyze token use-null-tokens? model)))
  
(defn parse [expression]
  (menard.nederlands/parse expression model))

(defn generate [spec]
  (menard.nederlands/generate expression model))




