(ns babel.italiano.morphology.determiners
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :as u :refer (copy fail? get-in ref? strip-refs unifyc)]))

(def patterns
  (-> "babel/italiano/morphology/determiners.edn"
      clojure.java.io/resource
      slurp
      read-string))

;; TODO: rules should apply iteratively until no changes are no made
;; similar to babel.encyclopedia/sem-impl
(defn apply-determiner-rules [input]
  (let [results
        (remove nil?
                (map (fn [[from to]]
                       (if (re-find from input)
                         (clojure.string/replace 
                          input
                    from to)))
                     (map #(get % :g)
                          (filter (fn [pattern-map]
                                    (some #(= :g %) (keys pattern-map)))
                                  patterns))))]
    (if (not (empty? results))
      [true (first results)]
      [false input])))
