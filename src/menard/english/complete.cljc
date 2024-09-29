(ns menard.english.complete
  (:require [dag_unify.core :as u]
            [clojure.tools.logging :as log]
            [menard.english.compile :refer [compile-lexicon]]
            [menard.english.tenses :refer [tenses]]
            [menard.model :refer [create]]))

(def create-model? true)

(defn model-fn []
  (create "english/models/complete"
          "complete"
          compile-lexicon false {:include-derivation? false}))

#?(:clj
   (if create-model?
     (def model
       (ref (model-fn)))))

(defn reload []
  (dosync (ref-set model (model-fn)))
  (type @model))
