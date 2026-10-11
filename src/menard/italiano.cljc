(ns menard.italiano
  "Italiano language module (JVM only): loads menard.italiano.tenses, whose symbols are
   referenced from resources/italiano/grammar.edn :options, and exposes the basic model.
   Mirrors menard.español's structure."
  #?(:clj (:require [menard.italiano.tenses]
                    [menard.model :refer [create]])))

#?(:clj
   (defn model-fn []
     (create "italiano/models/basic"
             "basic"
             ;; compile-lexicon-fn
             (fn [lexicon _ _] lexicon)
             ;; filter-out-nils?
             true
             {:include-derivation? false})))

#?(:clj (def model (ref (model-fn))))

#?(:clj
   (defn reload []
     (dosync (ref-set model (model-fn)))
     (type @model)))
