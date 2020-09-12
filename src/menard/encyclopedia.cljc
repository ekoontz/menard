(ns menard.encyclopedia
  ^{:doc "real-world knowledge, expressed
as a map of implications"}
  (:refer-clojure)
  (:require
   [menard.exception :refer [exception]]
   #?(:cljs [menard.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :refer [unify unify!]]
   [dag_unify.diagnostics :refer [ismorphic? strip-refs]]))

;; TODO: use clojure.core/isa? and clojure.core/derive where possible
;; in here, e.g.: (derive ::human ::animal)


#?(:clj
   (def nouns
     (->
      "menard/encyclopedia/nouns.edn"
      (clojure.java.io/resource)
      slurp
      read-string)))
  
;; {:aux false}: needed to prevent matching aux verbs because
;; they lack a {:pred} value.
#?(:clj
   (def verbs
     (->
      "menard/encyclopedia/verbs.edn"
      (clojure.java.io/resource)
      slurp
      read-string)))

(defn null-sem-impl [input]
  "null sem-impl: simply return input."
  (log/trace (str "null-sem-impl:" (strip-refs input)))
  input)

(defn get-encyc [input k]
  (get nouns {k (get-in input [k])} {}))

(defn impl-list [input]
  (map (fn [kv]
         (let [k (first (first kv))]
           (get nouns {k (get-in input [k] :top)} {})))
       (keys nouns)))

(defn sem-impl [input]
  "expand input feature structures with world and cultural knowledge, e.g., 
     if {:human true} => {:edible false}"
  (let [merged
        (unify input
               (reduce unify (impl-list input)))]
    (log/debug (str "sem-impl so far: " merged))
    (if (not (= (isomorphic? (strip-refs merged)
                             (strip-refs input))))
      ;; we've added some new information: more implications
      ;; may be possible from that, so call (sem-impl) again.
      (sem-impl merged original-input)
      
      ;; else, no more implications: done.
      (do
        (log/debug (str "sem-impl:" (strip-refs original-input) " -> " (strip-refs merged)))
        merged))))



