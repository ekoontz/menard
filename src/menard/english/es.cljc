(ns menard.english.es
  (:require [dag_unify.core :as u]
            [clojure.tools.logging :as log]
            [menard.english :as en]
            [menard.english.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]))

(def model
  (delay (create "english/models/es"
                 "complete"
                 compile-lexicon false {:include-derivation? false})))


(defn analyze [surface]
  (en/analyze surface @model))

(defn generate [spec]
  (en/generate spec @model))

(defn morph [expression]
  (en/morph expression @model false))

(defn parse [surface]
  (en/parse surface @model))

(defn syntax-tree [tree]
  (en/syntax-tree tree @model))


