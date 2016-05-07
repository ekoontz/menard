(ns babel.italiano.vocabcoach
  (:refer-clojure :exclude [get-in merge])
  (:require
   #?(:clj [clojure.tools.logging :as log])
   [babel.cache :refer [create-index]]
   #?(:cljs [babel.logjs :as log]) 
   [babel.engine :as engine]
   [babel.italiano :refer [analyze parse]]
   [babel.italiano.grammar :refer [medium]]
   [babel.italiano.lexicon :as main-lexicon]
   [babel.italiano.morphology :as morph :refer [fo]]
   [babel.lexiconfn :refer [filter-keys filter-vals]]
   [babel.over :refer [over]]
   [babel.ug :refer [head-principle]]
   [dag_unify.core :refer [fail? get-in merge strip-refs unifyc unify]]))

(def lexicon
  (-> main-lexicon/lexicon
      (filter-keys
       #(or
         (= % "a")
         (= % "a prossima")
         (= % "casa")
         (= % "essere")
         (= % "io")
         (= % "mezzogiorno")
         (= % "sono")))))

(def model
  (merge (into {}
               (map (fn [k]
                      [k (get medium k)])
                    (filter #(and (not (= % :lexicon))
                                  (not (= % :index)))
                            (keys medium))))
         {:lexicon lexicon
          :index (create-index (:grammar medium)
                               (flatten (vals lexicon))
                               head-principle)}))
(defn generate
  ([]
   (let [result (engine/generate :top model)]
     (if result
       (conj {:surface (fo result)}
             result))))
  ([spec]
   (let [result (engine/generate spec model)]
     (if result
       (conj {:surface (fo result)}
             result)))))

