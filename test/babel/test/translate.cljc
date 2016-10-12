(ns babel.test.translate
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.test.it :as it]
   [babel.test.en :as en]
   [clojure.test :refer [deftest is]]
   [dag_unify.core :refer [get-in strip-refs]]))

;; In Italian, certain verbs, called "essere" verbs, when conjugated
;; in certain tenses, agree in gender and number with their subject:
;;
;; For example, compare:
;;  Italian: "loro sono andati"
;;  English: They (masculine plural) went.
;; but:
;;  Italian: "loro sono andate"
;;  English: They (feminine plural) went.
;;
;; In English we indicate feminine and masculine gender with ♀ and ♂,
;; respectively.

;; Test that gender agreement is correctly translated.
(deftest past-and-gender-agreement-feminine
  (let [italian "loro sono andate"

        italian-structure
        (-> italian
            babel.italiano/parse
            first
            :parses
            first)
         
        semantics
        (-> italian-structure
            (get-in [:synsem :sem])
            strip-refs)
        
        english-structure
        (->  {:synsem {:sem semantics}}
             (babel.english/generate :model en/small))

        english (babel.english.morphology/fo english-structure)]

    (= "they (♀) went" english)))

(deftest past-and-gender-agreement-masculine
  (let [italian "loro sono andati"

        italian-structure
        (-> italian
            babel.italiano/parse
            first
            :parses
            first)
         
        semantics
        (-> italian-structure
            (get-in [:synsem :sem])
            strip-refs)
        
        english-structure
        (->  {:synsem {:sem semantics}}
             (babel.english/generate :model en/small))

        english (babel.english.morphology/fo english-structure)]

    (= "they (♂) went" english)))

(deftest latin
  (let [latin "ardebam"
        latin-structure
        (-> latin
            babel.latin/parse
            first
            :parses
            first)
         
        semantics
        (-> latin-structure
            (get-in [:synsem :sem])
            strip-refs)
        
        english-structure
        (->  {:synsem {:sem semantics}}
             (babel.english/generate :model en/small))

        english (babel.english.morphology/fo english-structure)]

    (= "I was burning" english)))
