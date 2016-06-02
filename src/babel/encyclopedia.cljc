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
   {:pred :house} {:artifact true
                   :buyable true
                   :place true}

   {:animate true} {:living true}

   {:human true} {:animate true}

   {:living true} {:artifact false}

   {:pet true} {:animate true
                :buyable true
                :edible false
                :human false}
   }
  )

(def animal {:animate true
             :artifact false
             :living true})

(defn null-sem-impl [input]
  "null sem-impl: simply return input."
  (log/trace (str "null-sem-impl:" (strip-refs input)))
  input)
  
(defn sem-impl [input & [original-input]]
  "expand input feature structures with semantic (really cultural) implicatures, e.g., if human, then not buyable or edible"
  (let [original-input (if original-input original-input
                           (do
                             (log/debug (str "original call of sem-impl: (" (get-in input [:pred]) ")" (strip-refs input)))
                             input))]
    (cond
      (= input :top) input
      true
      (let [activity (if (= (get-in input '(:activity))
                            true)
                       {:animate false
                        :artifact false
                        :consumable false
                        :part-of-human-body false})
            animate (if (= (get-in input '(:animate))
                           true)
                      {:activity false
                       :artifact false
                       :mass false
                       :furniture false
                       :physical-object true
                       :part-of-human-body false
                       :drinkable false
                       :speakable false
                       :place false}{})
            artifact (if (= (get-in input '(:artifact))
                            true)
                       {:animate false
                        :activity false
                        :physical-object true}{})

            buyable (if (= (get-in input '(:buyable))
                           true)
                      {:human false
                       :part-of-human-body false})

            city (if (= (get-in input '(:city))
                        true)
                   {:place true
                    :human false
                    :animate false
                    :legible false})

            clothing (if (= (get-in input '(:clothing))
                            true)
                       {:animate false
                        :place false
                        :physical-object true}{})

            consumable (if (= (get-in input '(:consumable)) true)
                         {:activity false
                          :buyable true
                          :furniture false
                          :legible false
                          :pet false
                          :physical-object true
                          :speakable false})
            
            consumable-false (if (= (get-in input '(:consumable)) false)
                               {:drinkable false
                                :edible false} {})

            drinkable
            ;; drinkables are always mass nouns.
            (if (= (get-in input '(:drinkable)) true)
              {:mass true})

            drinkable-xor-edible-1
            ;; things are either drinkable or edible, but not both (except for weird foods
            ;; like pudding or soup). (part 1: edible)
            (if (and (= (get-in input '(:edible)) true)
                     (= (get-in input '(:drinkable) :notfound) :notfound))
              {:drinkable false}{})

            drinkable-xor-edible-2
            ;; things are either drinkable or edible, but not both (except for weird foods
            ;; like pudding or soup). (part 2: drinkable)
            (if (and (= (get-in input '(:drinkable)) true)
                     (= (get-in input '(:edible) :notfound) :notfound))
              {:edible false})

            ;; qualities of foods and drinks.
            edible (if (or (= (get-in input '(:edible)) true)
                           (= (get-in input '(:drinkable)) true))
                     {:consumable true
                      :human false
                      :pet false
                      :place false
                      :speakable false
                      :legible false
                      :furniture false
                      :part-of-human-body false}{})

            furniture (if (= (get-in input '(:furniture))
                             true)
                        {:artifact true
                         :animate false
                         :buyable true
                         :drinkable false
                         :legible false
                         :edible false
                         :place false
                         :speakable false})

            human (if (= (get-in input '(:human))
                         true)
                    {:activity false
                     :buyable false
                     :physical-object true
                     :edible false
                     :animate true
                     :part-of-human-body false
                     :drinkable false
                     :speakable false
                     :city false
                     :place false})
            inanimate (if (= (get-in input '(:animate))
                             false)
                        {:human false})

            ;; legible(x) => artifact(x),drinkable(x,false),edible(x,false),human(x,false)
            legible
            (if (= (get-in input '(:legible)) true)
              {:artifact true
               :drinkable false
               :human false
               :furniture false
               :part-of-human-body false
               :edible false})

            material-false
            (if (= (get-in input '(:material)) :false)
              {:edible false
               :animate false
               :drinkable false
               :buyable false ; money can't buy me love..
               :visible false})

            non-places (if (or
                            (= (get-in input '(:legible)) true)
                            (= (get-in input '(:part-of-human-body)) true)
                            (= (get-in input '(:pred)) :fiore)
                            (= (get-in input '(:pred)) :scala))
                         {:place false})

            ;; artifact(x,false) => legible(x,false)
            not-legible-if-not-artifact
            (if (= (get-in input '(:artifact)) false)
              {:legible false})

            part-of-human-body
            (if (= (get-in input '(:part-of-human-body)) true)
              {:speakable false
               :buyable false
               :animate false
               :edible false
               :drinkable false
               :legible false
               :artifact false})

            ;; we don't eat pets (unless things get so desperate that they aren't pets anymore)
            pets (if (= (get-in input '(:pet))
                        true)
                   {:edible false
                    :buyable true
                    :physical-object true
                    })

            place (if (= (get-in input '(:place))
                         true)
                    {:activity false
                     :animate false
                     :speakable false
                     :physical-object true
                     :drinkable false
                     :edible false
                     :legible false}{})

            place-false (if (= (get-in input '(:place))
                               false)
                          {:city false})
         
            time (if (= (get-in input [:time])
                        true)
                   {:activity false
                    :animate false
                    :city false
                    :speakable false
                    :physical-object true
                    :drinkable false
                    :edible false
                    :legible false
                    :place false})
            ]
        (let [merged
              (cond (= input :fail) :fail

                    true
                    (merge input animate artifact buyable city clothing consumable consumable-false drinkable
                           drinkable-xor-edible-1 drinkable-xor-edible-2
                           edible furniture human inanimate
                           legible material-false non-places
                           not-legible-if-not-artifact part-of-human-body pets place
                           place-false time
                           ))]
          (log/trace (str "sem-impl so far: " merged))
          (if (not (= merged input)) ;; TODO: make this check more efficient: count how many rules were hit
            ;; rather than equality-check to see if merged has changed.
            (sem-impl merged original-input) ;; we've added some new information: more implications possible from that.

            ;; else, no more implications: done.
            (do
              (log/debug (str "sem-impl:" (strip-refs original-input) " -> " (strip-refs merged)))
              merged)))))))


