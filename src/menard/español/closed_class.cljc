(ns menard.español.closed-class
  (:require [dag_unify.core :as u]
            [clojure.tools.logging :as log]
            [menard.español :as es]
            [menard.model :refer [create]]
            [menard.serialization :as s]))

(def create-model? true)

(defn compile-lexicon-fn [lexicon _ _]
  lexicon)

(defn model-fn []
  (create "español/models/closed-class"
          "closed-class"
          compile-lexicon-fn false {:include-derivation? false}))

#?(:clj
   (if create-model?
     (def model
       (ref (model-fn)))))

(defn reload []
  (dosync (ref-set model (model-fn)))
  (type @model))

(defn generate [spec]
  (es/generate spec @model))

(defn parse [expression]
  (es/parse expression model))

(defn morph [tree]
  (s/morph tree (-> model deref :morphology)))

(defn syntax-tree [tree]
  (s/syntax-tree tree (-> model deref :morphology)))
