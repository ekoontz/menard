(ns menard.español 
  (:require [menard.español.compile :refer [compile-lexicon]]
            [menard.generate :as g]
            [menard.model :refer [create load-model]]
            [menard.morphology :refer [morph-leaf]]
            [menard.serialization :as s]
            [dag_unify.core :as u :refer [unify]]
            #?(:clj [clojure.java.io :as io :refer [resource]])
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
    (log/debug (str "menard.espa~nol/generate: generated: " (-> retval es-syntax-tree)))
    retval))

(defn morph [tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (s/morph (u/get-in tree [:syntax-tree]) (:morphology @model))

     :else
     (s/morph tree (:morphology @model))))

(defn convert []
  (->> (-> "resources/español/lexicon.edn"
           slurp
           read-string)
       (filter (fn [[k v]]
                 (or true (= :verb (u/get-in v [:synsem :cat])))))
       (map (fn [[k v]]
              (cond (map? v)
                    [k (-> {:cat :verb
                            :sem (u/get-in v [:synsem :sem])}
                           (merge (if (u/get-in v [:synsem :agr])
                                    {:agr (u/get-in v [:synsem :agr])}
                                    {}))
                           (merge (if (u/get-in v [:espanol])
                                    {:agr (u/get-in v [:espanol])}
                                    {})))]
                    :else
                    [k v])))
       (into {})))
