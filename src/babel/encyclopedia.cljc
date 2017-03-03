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
                      :machine false
                      :mass false
                      :furniture false
                      :physical-object true
                      :part-of-human-body false
                      :drinkable false
                      :speakable false
                      :place false}

   {:artifact false}  {:clothing false
                       :machine false}

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
                      :machine false
                      :place false
                      :physical-object true}
   {:consumable
    false}           {:drinkable false
                      :edible false}
   {:consumable
    true}            {:activity false
                      :buyable true
                      :clothing false
                      :furniture false
                      :legible false
                      :human false
                      :machine false
                      :part-of-human-body false
                      :pet false
                      :physical-object true
                      :place false
                      :speakable false}

   {:drinkable true} {:animate false
                      :consumable true
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
                      :clothing false
                      :consumable false
                      :legible false
                      :edible false
                      :machine false
                      :place false
                      :physical-object true
                      :speakable false}
   
   {:human true}     {:animate true
                      :artifact false
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

   {:machine true}   {:artifact true
                      :consumable false}
   
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
    false}           {:clothing false
                      :consumable false
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
                      :clothing false
                      :consumable false
                      :event false
                      :legible false
                      :machine false
                      :place false
                      :spec {:of {:human true}}}

   {:pred :beach}    {:artifact false
                      :place true}
   
   {:pred :bicycle}  {:artifact true
                      :clothing false
                      :consumable false
                      :legible false
                      :machine true
                      :place false
                      :spec {:of {:human true}}}

   {:pred :bird}     {:animate true
                      :human false}

   {:pred :book}     {:artifact true
                      :clothing false
                      :legible true
                      :machine false}
   
   {:pred :bread}    {:artifact true
                      :edible true}
   
   {:pred :car}      {:clothing false
                      :consumable false
                      :legible false
                      :machine true
                      :place false}

   {:pred :cat}      {:pet true}
   
   {:pred :chair}    {:furniture true}
   
   {:pred :city}     {:clothing false
                      :machine false}
   
   {:pred :coffee}   {:artifact true
                      :drinkable true}

   {:pred :dentist}  {:human true}

   {:pred :dog}      {:pet true}
   
   {:pred :espresso} {:artifact true
                      :drinkable true}

   {:pred :game}     {:activity true
                      :event true
                      :games true
                      :machine false}
   
   {:pred :house}    {:artifact true
                      :clothing false
                      :city false
                      :consumable false
                      :machine false
                      :place true
                      :legible false}

   {:pred :key}      {:artifact true
                      :clothing false
                      :consumable false
                      :event false
                      :machine false
                      :place false}

   {:pred :meeting}  {:clothing false
                      :consumable false
                      :living false
                      :machine false}
   
   {:pred :music}    {:clothing false
                      :consumable false}
   
   {:pred :name}     {:physical-object false
                      :event false}
   
   {:pred :note}     {:animate false
                      :clothing false
                      :drinkable false
                      :edible false
                      :legible true}

   {:pred :party}    {:clothing false
                      :machine false}

   {:pred :pizza}    {:artifact true
                      :edible true}

   {:pred :radio}    {:clothing false
                      :machine true}

   {:pred :salad}    {:artifact true
                      :edible true}

   {:pred :sigh}     {:artifact false
                      :physical-object false
                      :event true}
   
   {:pred :shoe}     {:clothing true}

   {:pred :student}  {:human true}

   {:pred :study   
    :cat :noun}      {:place true}

   {:pred :table}    {:furniture true}

   {:pred :vino}     {:artifact true
                      :drinkable true}

   {:pred :water}    {:artifact false
                      :drinkable true}
   
   {:pred :word}     {:consumable false
                      :legible true
                      :living false
                      :machine false
                      :physical-object false
                      :speakable true}

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
  (map (fn [sem]
         (unify {:synsem {:aux false
                          :sem {:shared-with-obj false} ;; prevent matching reflexive verbs.
                          :cat :verb}}
                {:synsem {:sem sem}}))
                  
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

        {:pred :answer
         :subj {:human true}
         :obj {:human true}}

        {:pred :answer
         :subj {:human true}}

        {:pred :avoid
         :subj {:animate true}}
        
        {:pred :boil
         :subj {:human true}
         :obj {:liquid true
               :consumable true}}

        {:pred :bring
         :subj {:human true}
         :obj {:buyable true}}

        {:pred :burn
         :subj {:human true}
         :obj {:human false
               :physical-object true}}
        
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
         :subj {:human true}
         :obj {:human true}}

        {:pred :come
         :subj {:animate true}}

        {:pred :comprare
         :subj {:human true}
         :obj {:buyable true}}

        {:pred :correspond
         :subj {:human true}
         :obj {:human true}}
        
        {:pred :dance
         :subj {:human true}}
        
        {:pred :deludere
         :obj {:human true}}
        
        {:pred :drink
         :subj {:animate true}
         :obj {:drinkable true}}
        
        {:pred :earn
         :subj {:human true}
         :obj {:human false}}

        {:pred :eat
         :subj {:animate true}}

        {:pred :eat
         :obj {:edible true}}

        {:pred :enter
         :subj {:animate true}
         :obj {:place true}}

        {:pred :exit
         :subj {:animate true}}
        
        {:pred :faint
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

        {:pred :have-dinner
         :subj {:human true}}

        {:pred :have-fun
         :reflexive true
         :subj {:human true}}

        {:pred :hug
         :subj {:human true}
         :obj {:animate true}}

        {:pred :imagine
         :subj {:human true}}

        {:pred :learn
         :subj {:human true}
         :obj {:legible true}} ;; not quite right, but a rough start.

        {:pred :leave
         :subj {:animate true}}

        {:pred :leave-behind
         :subj {:animate true}}

        {:pred :live
         :subj {:animate true}}

        {:pred :put-on
         :subj {:human true}
         :obj {:clothing true}}
        
        {:pred :read
         :subj {:human true}}

        {:pred :read
         :obj {:legible true}}

        {:pred :say
         :subj {:human true}
         :obj {:speakable true}}

        {:pred :scold
         :subj {:human true}}

        {:pred :sigh
         :subj {:human true}}
        
        {:pred :sleep
         :subj {:animate true}}

        {:pred :speak
         :subj {:human true}
         :obj {:speakable true}}

        {:pred :stampare
         :obj {:legible true}}

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

        {:pred :turn-on
         :subj {:human true}
         :obj {:machine true}}

        {:pred :turn-up
         :subj {:human true}
         :obj {:machine true}}

        {:pred :turn-down
         :subj {:human true}
         :obj {:machine true}}

        {:pred :understand
         :subj {:human true}}

        {:pred :understand-deeply
         :subj {:human true}}
        
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

        {:pred :wash-oneself
         :subj {:living true}}

        {:pred :wear
         :subj {:clothing true}}

        {:pred :wear-i
         :subj {:clothing true}}

        {:pred :wear-l
         :subj {:clothing true}}

        {:pred :wear-p
         :subj {:clothing true}}

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
