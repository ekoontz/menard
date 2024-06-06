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

(defn convert-exceptions [exceptions]
  (concat (when (u/get-in exceptions [:present :1sing])
            [{:infl :present
              :agr {:person :1st :number :sing}
              :surface (u/get-in exceptions [:present :1sing])}])
          (when (u/get-in exceptions [:present :2sing])
            [{:infl :present
              :agr {:person :2nd :number :sing}
              :surface (u/get-in exceptions [:present :2sing])}])
          (when (u/get-in exceptions [:present :2sing])
            [{:infl :present
              :agr {:person :3rd :number :sing}
              :surface (u/get-in exceptions [:present :3sing])}])
          
          (when (u/get-in exceptions [:preterito :1sing])
            [{:infl :preterito
              :agr {:person :1st :number :sing}
              :surface (u/get-in exceptions [:preterito :1sing])}])
          (when (u/get-in exceptions [:preterito :2sing])
            [{:infl :preterito
              :agr {:person :2nd :number :sing}
              :surface (u/get-in exceptions [:preterito :2sing])}])
          (when (u/get-in exceptions [:preterito :2sing])
            [{:infl :preterito
              :agr {:person :3rd :number :sing}
              :surface (u/get-in exceptions [:preterito :3sing])}])))

(defn convert []
  (->> (-> "resources/español/lexicon.edn"
           slurp
                 read-string)
       (map (fn [[k v]]
              [k (if (vector? v)
                   v [v])]))
       (map (fn [[k vs]]
              [k (->> vs
                      (map (fn [v]
                             (-> {}
                                 (merge (when (u/get-in v [:synsem :sem])
                                          {:sem (u/get-in v [:synsem :sem])}))
                                 (merge (when (u/get-in v [:synsem :agr])
                                          {:agr (u/get-in v [:synsem :agr])}
                                          {}))
                                 (merge (if (u/get-in v [:espanol])
                                          {:exceptions (vec (convert-exceptions (u/get-in v [:espanol])))}
                                          {})))))
                      vec)]))
       (into {})))


