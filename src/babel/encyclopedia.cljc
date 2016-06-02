(ns babel.encyclopedia
  ^{:doc "real-world knowledge, expressed
as a map of implications"}
  (:refer-clojure)
  (:require
   [babel.exception :refer [exception]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :refer [strip-refs]]))

(def encyc
  {
   {:activity true} {:animate false
                     :artifact false
                     :consumable false
                     :part-of-human-body false}

   {:animate true} {:activity false
                    :artifact false
                    :mass false
                    :furniture false
                    :physical-object true
                    :part-of-human-body false
                    :drinkable false
                    :speakable false
                    :place false}

   {:artifact true} {:animate false
                     :activity false
                     :physical-object true}

   {:buyable true} {:human false
                    :part-of-human-body false}

   {:city true} {:artifact true
                 :legible false
                 :place true}

   {:clothing true} {:animate false
                     :place false
                     :physical-object true}

   {:consumable true} {:activity false
                       :buyable true
                       :furniture false
                       :legible false
                       :human false
                       :part-of-human-body false
                       :pet false
                       :physical-object true
                       :speakable false}

   {:consumable false} {:drinkable false
                        :edible false}

   {:drinkable true}   {:mass true}

   {:edible true}      {:consumable true}
                      
   {:furniture true}   {:artifact true
                        :animate false
                        :buyable true
                        :consumable false
                        :legible false
                        :edible false
                        :place false
                        :speakable false}
   
   {:human true} {:animate true
                  :buyable false
                  :consumable false
                  :legible false
                  :pet false
                  :part-of-human-body true
                  :physical-object true
                  :place false}

   {:living false} {:animate false
                    :human false}
   
   {:living true} {:artifact false}

   {:part-of-human-body true} {:consumable false
                               :human false
                               :physical-object true}
   
   {:pet true} {:animate true
                :buyable true
                :edible false
                :human false}

   {:place true} {:activity false
                  :consumable false
                  :living false
                  :physical-object true}

   {:time true} {:activity false
                 :living false
                 :place false}
   
   }
  )

(def animal {:animate true
             :artifact false
             :living true})

(defn null-sem-impl [input]
  "null sem-impl: simply return input."
  (log/trace (str "null-sem-impl:" (strip-refs input)))
  input)

(defn get-encyc [input k]
  (get encyc {k (get-in input [k])} {}))

(defn impl-list [input]
  [(get-encyc input :activity)
   (get-encyc input :animate)
   (get-encyc input :artifact)
   (get-encyc input :buyable)
   (get-encyc input :city)
   (get-encyc input :clothing)
   (get-encyc input :consumable)
   (get-encyc input :drinkable)
   (get-encyc input :edible)
   (get-encyc input :furniture)
   (get-encyc input :human)
   (get-encyc input
              :part-of-human-body)
   (get-encyc input :pet)
   (get-encyc input :place)
   (get-encyc input :time)])

(defn sem-impl [input & [original-input]]
  "expand input feature structures with semantic (really cultural) implicatures, e.g., if human, then not buyable or edible"
  (let [original-input (if original-input original-input
                           (do
                             (log/debug (str "original call of sem-impl: (" (get-in input [:pred]) ")" (strip-refs input)))
                             input))]
    (cond
      (= input :top) input
      true
      (let [activity   (get-encyc input :activity)
            animate    (get-encyc input :animate)
            artifact   (get-encyc input :artifact)
            buyable    (get-encyc input :buyable)
            city       (get-encyc input :city)
            clothing   (get-encyc input :clothing)
            consumable (get-encyc input :consumable)
            drinkable  (get-encyc input :drinkable)
            edible     (get-encyc input :edible)
            furniture  (get-encyc input :furniture)
            human      (get-encyc input :human)
            part-of-human-body (get-encyc input
                                          :part-of-human-body)
            pet        (get-encyc input :pet)
            place      (get-encyc input :place)
            time       (get-encyc input :time)
            ]
        (let [merged
              (cond (= input :fail) :fail

                    true
                    (merge input
                           (reduce merge [activity animate artifact buyable city clothing consumable drinkable
                                          edible furniture human
                                          pet place time])))]
          (log/trace (str "sem-impl so far: " merged))
          (if (not (= merged input)) ;; TODO: make this check more efficient: count how many rules were hit
            ;; rather than equality-check to see if merged has changed.
            (sem-impl merged original-input) ;; we've added some new information: more implications possible from that.

            ;; else, no more implications: done.
            (do
              (log/debug (str "sem-impl:" (strip-refs original-input) " -> " (strip-refs merged)))
              merged)))))))


