(ns babel.encyclopedia
  ^{:doc "real-world knowledge, expressed
as a map of implications"}
  (:refer-clojure)
  (:require
   [babel.exception :refer [exception]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :refer [strip-refs unify]]))

;; TODO: use clojure.core/isa? and clojure.core/derive where possible
;; in here, e.g.: (derive ::human ::animal)

(def encyc
  {
   {:activity true}  {:animate false
                      :artifact false
                      :consumable false
                      :part-of-human-body false}
   
   {:animate false}  {:human false
                      :pet false}
   
   {:animate true}   {:activity false
                      :artifact false
                      :living true
                      :mass false
                      :furniture false
                      :physical-object true
                      :part-of-human-body false
                      :drinkable false
                      :speakable false
                      :place false}

   {:artifact true}  {:animate false
                      :activity false
                      :living false
                      :physical-object true}
   
   {:buyable true}   {:human false
                      :part-of-human-body false}

   {:city true}      {:artifact true
                      :legible false
                      :place true}

   {:clothing true}  {:animate false
                      :edible false
                      :place false
                      :physical-object true}
   {:consumable
    true}            {:activity false
                      :buyable true
                      :furniture false
                      :legible false
                      :human false
                      :part-of-human-body false
                      :pet false
                      :physical-object true
                      :place false
                      :speakable false}
   {:consumable
    false}           {:drinkable false
                      :edible false}

   {:drinkable true} {:consumable true
                      :edible false
                      :mass true}
   
   {:edible true}    {:consumable true
                      :drinkable false}

   {:event true}     {:buyable false
                      :physical-object false
                      :speakable false}
   
   {:furniture true} {:artifact true
                      :animate false
                      :buyable true
                      :consumable false
                      :legible false
                      :edible false
                      :place false
                      :speakable false}
   
   {:human true}     {:animate true
                      :buyable false
                      :consumable false
                      :legible false
                      :pet false
                      :part-of-human-body false
                      :physical-object true
                      :place false
                      :spec {:of {:human true}}}
   {:intelligent
    true}            {:animate true}

   {:living false}   {:animate false
                      :human false}
   
   {:living true}    {:artifact false
                      :place false}

   {:part-of-human-body
    true}            {:consumable false
                      :human false
                      :physical-object true}

   {:pet true}       {:animate true
                      :buyable true
                      :edible false
                      :human false
                      :spec {:of {:human true}}}

   {:physical-object
    false}           {:consumable false
                      :living false}
   {:physical-object
    true}            {:event false
                      :speakable false}

   {:place true}     {:activity false
                      :consumable false
                      :living false
                      :physical-object true
                      :speakable false
                      :spec {:of {:animate true}}}
   ;; some places could place additional
   ;; restrictions on owners (e.g. {:of {:human true}}).

   ;; <preds>
   {:pred :backpack} {:artifact true
                      :clothing true}
   {:pred :bag}      {:artifact true
                      :consumable false
                      :event false
                      :legible false
                      :place false
                      :spec {:of {:human true}}}
   
   {:pred :bicycle}  {:artifact true
                      :consumable false
                      :legible false
                      :place false
                      :spec {:of {:human true}}}
   
   {:pred :bread}    {:artifact true
                      :edible true}

   {:pred :car}      {:artifact true
                      :consumable false
                      :legible false
                      :place false}

   {:pred :coffee}   {:artifact true
                      :drinkable true}

   {:pred :espresso} {:artifact true
                      :drinkable true}

   {:pred :house}    {:artifact true
                      :consumable false
                      :place true
                      :legible false}

   {:pred :key}      {:animate false
                      :consumable false
                      :event false
                      :place false}

   {:pred :name}     {:physical-object false
                      :event false}

   {:pred :pizza}    {:artifact true
                      :edible true}

   {:pred :shoe}     {:artifact true
                      :clothing true}

   {:pred :student}  {:human true}

   ;; The following inference rule is not
   ;; yet used, but intended as a way to integrate
   ;; verbs into the encyclopedia as we do with nouns.
   ;; Note the use of {:cat :verb} to distinguish this
   ;; from ..
   {:pred :study   
    :cat :verb}  {:activity true
                  :subj {:human true}
                  :obj {:legible true}}

   ;; .. the next rule which applies to nouns only.
   {:pred :study   
    :cat :noun}  {:place true}

   {:pred :vino}     {:artifact true
                      :drinkable true}
   ;; </preds>

   {:stupid true}    {:animate true}

   {:time true}      {:activity false
                      :living false
                      :place false}
   }
  )

(defn null-sem-impl [input]
  "null sem-impl: simply return input."
  (log/trace (str "null-sem-impl:" (strip-refs input)))
  input)

(defn get-encyc [input k]
  (get encyc {k (get-in input [k])} {}))

(defn impl-list [input]
  (map (fn [kv]
         (let [k (first (first kv))]
           (get encyc {k (get-in input [k] :top)} {})))
       (keys encyc)))

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
        (if (not (= merged input)) ;; TODO: make this check more efficient: count how many rules were hit
          ;; rather than equality-check to see if merged has changed.
          (sem-impl merged original-input) ;; we've added some new information: more implications possible from that.

          ;; else, no more implications: done.
          (do
            (log/debug (str "sem-impl:" (strip-refs original-input) " -> " (strip-refs merged)))
            merged))))))




