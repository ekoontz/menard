(ns menard.español 
  (:require [menard.español.compile :refer [compile-lexicon]]
            [menard.generate :as g]
            [menard.model :refer [create load-model]]
            [menard.morphology :refer [morph-leaf]]
            [menard.serialization :as s]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def model
  (ref (create "español/models/basic"
               "basic"
               compile-lexicon
               true)))

#?(:clj
   (defn es-syntax-tree [tree]
     (s/syntax-tree tree (-> model deref :morphology))))

(defn generate [spec]
  (let [model (deref model)
        retval
        (binding [g/max-depth (:max-depth spec g/max-depth)
                  g/max-fails (:max-fails spec g/max-fails)
                  g/allow-backtracking? true]
          (-> spec
              ((fn [x] (unify x (:training-wheels x :top))))
              (dissoc :training-wheels)
              (g/generate (-> model :grammar)
                          (-> model :lexicon-index-fn)
                          es-syntax-tree)))]
    (log/info (str "menard.espa~nol/generate: generated: " (-> retval es-syntax-tree)))
    retval))

(defn morph [tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (s/morph (u/get-in tree [:syntax-tree]) (:morphology @model))

     :else
     (s/morph tree (:morphology @model))))
