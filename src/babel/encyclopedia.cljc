(ns babel.encyclopedia
  ^{:doc "real-world knowledge, expressed
as a map of implications"}
  (:refer-clojure)
  (:require
   [babel.exception :refer [exception]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :as u :refer [strip-refs unify unify!]]))

;; TODO: use clojure.core/isa? and clojure.core/derive where possible
;; in here, e.g.: (derive ::human ::animal)

(def nouns
  (->
    "babel/encyclopedia/nouns.edn"
    (clojure.java.io/resource)
    slurp
    read-string))
  
;; {:aux false}: needed to prevent matching aux verbs because
;; they lack a {:pred} value.
(def verbs
  (map (fn [sem]
         (unify! {:synsem {:aux false
                           :sem {:shared-with-obj false} ;; prevent matching reflexive verbs.
                           :cat :verb}}
                 {:synsem {:sem sem}}))

       (->
        "babel/encyclopedia/verbs.edn"
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
  (cond (< 1 (count (keys (u/get-in input [:prop]))))
        (mapcat (fn [k]
                  (impl-list {:prop {k (u/get-in input [:prop k])}}))
                (keys (u/get-in input [:prop])))
        true
        (map (fn [kv]
               (let [k (first (first kv))]
                 (get nouns {k (get-in input [k] :top)} {})))
             (keys nouns))))

(defn sem-impl [input & [original-input]]
  "expand input feature structures with semantic (really cultural) implicatures, e.g., if human, then not buyable or edible"
  (let [original-input (if original-input original-input
                           (do
                             (log/debug (str "original call of sem-impl: (" (get-in input [:pred]) ")" (strip-refs input)))
                             input))]
    (cond
      (keyword? input) input
      (empty? input) {}
      true
      (let [merged
            (unify input
                   (reduce unify (impl-list input)))]
        (log/debug (str "sem-impl so far: " merged))
        (if (not (= (::dag_unify.core/serialized merged)
                    (::dag_unify.core/serialized input)))
          ;; rather than equality-check to see if merged has changed.
          ;; we've added some new information: more implications
          ;; may be possible from that, so call (sem-impl) again.
          (sem-impl merged original-input)

          ;; else, no more implications: done.
          (do
            (log/debug (str "sem-impl:" (strip-refs original-input) " -> " (strip-refs merged)))
            merged))))))
