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
                      :artifact true
                      :consumable false
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
   {:pred :backpack} {:clothing true}

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
                      :city false
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

   {:pred :salad}    {:artifact true
                      :edible true}

   {:pred :shoe}     {:clothing true}

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

;; {:aux false}: needed to prevent matching aux verbs because
;; they lack a {:pred} value.
(def verb-pred-defaults
  (map #(unify {:synsem {:aux false
                         :cat :verb}}
               {:synsem {:sem %}})
         
       [{:pred :abbracciare
         :active false
         :discrete false
         :obj {:animate true}
         :subj {:human true}}

        {:subj {:human true}
         :pred :admire}

        {:activity true
         :obj {:human true}
         :pred :aiutare}
        
        {:activity false
         :discrete false
         :pred :amare
         :subj {:human true}}

        {:pred :bring
         :subj {:human true}
         :obj {:buyable true}}

        {:pred :carry
         :subj {:human true}
         :obj {:physical-object true}}
        
        {:pred :cenare
         :subj {:human true}}

        {:pred :cercare
         :subj {:animate true}}
        
        {:pred :change-clothes
         :subj {:human true}}

        {:pred :chat
         :subj {:human true}}

        {:pred :come
         :subj {:animate true}}

        {:pred :comprare
         :subj {:human true}
         :obj {:buyable true}}
        
        {:pred :deludere
         :obj {:human true}}
        
        {:pred :drink
         :subj {:animate true}
         :obj {:drinkable true}}
        
        {:pred :earn
         :subj {:human true}
         :obj {:human false}}

        {:pred :exit
         :subj {:animate true}}
        
        {:pred :fall-asleep
         :subj {:animate true}}

        {:pred :get-angry
         :subj {:animate true}}

        {:pred :get-bored
         :subj {:animate true}}

        {:pred :get-dressed
         :subj {:human true}}

        {:pred :get-off
         :subj {:animate true}}

        {:pred :get-on
         :subj {:animate true}}

        {:pred :get-ready
         :subj {:human true}}

        {:pred :get-up
         :subj {:animate true}}

        {:pred :giocare
         :subj {:human true}
         :obj {:game true}}

        {:pred :go
         :subj {:animate true}}

        {:pred :go-downstairs
         :subj {:animate true}}

        {:pred :go-out
         :subj {:animate true}}

        {:pred :go-upstairs
         :subj {:animate true}}

        {:pred :have-fun
         :reflexive true
         :subj {:human true}}

        {:pred :hug
         :subj {:human true}
         :obj {:animate true}}

        {:pred :imagine
         :subj {:human true}}

        {:pred :leave
         :subj {:animate true}}

        {:pred :leave-behind
         :subj {:animate true}}

        {:pred :live
         :subj {:animate true}}
        
        {:pred :mangiare
         :subj {:animate true}
         :obj {:edible true}}
        
        {:pred :read
         :subj {:human true}
         :obj {:legible true}}

        {:pred :scold
         :subj {:human true}}
        
        {:pred :sleep
         :subj {:animate true}}

        {:pred :speak
         :subj {:human true}
         :obj {:speakable true}}

        {:pred :steal
         :subj {:animate true}
         :obj {:physical-object true
               :human false
               :place false}}
        
        {:pred :study
         :subj {:human true}
         :obj {:legible true}}

        {:pred :suonare
         :subj {:human true}
         :obj {:human false}} ;; TODO: split suonare into two preds: 1. play + song; 2. play + instrument.
        
        {:pred :talk
         :subj {:human true}
         :obj {:speakable true}}

        {:pred :think
         :subj {:human true}}

        {:pred :try
         :subj {:animate true}}
        
        {:pred :vendere
         :subj {:human true}
         :obj {:human false}}

        {:pred :wake-up
         :reflexive true
         :subj {:animate true}}
        
        {:pred :want
         :subj {:animate true}}

        {:pred :wash
         :subj {:human true}}

        {:pred :win
         :subj {:human true}
         :obj {:human false}}

        {:pred :wonder
         :subj {:human true}}

        {:pred :work-human
         :subj {:human true}}

        {:pred :work-nonhuman
         :subj {:human false}}

        {:pred :yell
         :subj {:human true}}]))

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
          ;; we've added some new information: more implications
          ;; may be possible from that, so call (sem-impl) again.
          (sem-impl merged original-input)

          ;; else, no more implications: done.
          (do
            (log/debug (str "sem-impl:" (strip-refs original-input) " -> " (strip-refs merged)))
            merged))))))
