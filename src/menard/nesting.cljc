(ns menard.nesting
  (:require
   [dag_unify.core :as u :refer [unify]]
   [menard.ug :as ug]))

;; These are rules for sharing
;; contents of various word- or phrase-
;; level maps, when nesting applies
;; (i.e. when the :mod of the head's :sem is nested
;;  within the parent's :sem).
;;
;; A grammatical rule can inherit from
;; at most one (i.e. could be 0) of:
;; - cons-and-nest
;; - cons-only
;; - nest-only
;; If it inherits from either cons-and-nest
;; or nest-only, then it should also inherit
;; from exactly one of the following (in this
;; file below in 'part-2'):
;; - adjective
;; - noun
;; - verb
(comment
  :def "cons-and-nest"
  :sem {:mod {:first [[1] :top]
              :rest [[2] :top]}}
  :mods-nested? true
  :comp {:mods-nested? true
         :sem [1]}
  :head {:mods-nested? false
         :mod [2]}
  "used for the top-most adjunct in a phrase,
   e.g. in '[.vier [.kleine +honden]]', 
   the top-most adjunct is 'vier'."
  )
(def cons-and-nest
  (let [tmpone (atom :top)
        one (atom {:ref tmpone})
        two (atom :top)]
    {:sem {:ref tmpone
           :mod {:first one
                 :rest two}}
     :mod nil
     :mods-nested? true
     :comp {:mods-nested? true
            :sem one}
     :head {:mods-nested? false
            :mod two}}))

(def cons-and-nest-two
  (let [tmpone (atom :top)
        one (atom :top)
        two (atom :top)]
    {:mods-nested? true
     :sem {:mod {:first one
                 :rest two}}
     :comp {:mods-nested? true
            :sem one}
     :head {:mods-nested? false
            :sem {:mod two}}}))

(comment
  :mods-nested? false
  :mod {:first [[1] :top]
        :rest [[2] :top]}
  :comp {:mods-nested? true
         :sem [1]}
  :head {:mod [2]
         :mods-nested? false}
  "add a comment here..")
(def cons-only
  (-> ug/head-sem
      (unify ug/head-rule)
      (unify
       (let [one (atom :top)
             two (atom {:ref one})
             three (atom :top)]
         {:sem {:ref one}
          :mods-nested? false
          :mod {:first two
                :rest three}
          :comp {:mods-nested? true
                 :sem two}
          :head {:mod three
                 :mods-nested? false}}))))

(def nest-only
  (let [two (atom :top)
        three (atom :top)]
    {:mods-nested? true
     :sem {:mod two}
     :mod nil
     :comp {:mods-nested? true}
     :head {:sem three
            :mods-nested? false
            :mod two}}))

;; part 2: part-of-speech-specific
;; nesting rules.
;; any grammar rule that is either
;; cons-and-nest or nest-only
;; needs to also include one of the
;; following, depending on the :cat
;; of the :head (and :cat of the parent)
;; 
;; This is because with cons-and-nest or nest-only
;; rules, the :sem of the head is distinct from
;; the :sem of the parent, but all of the
;; contents within the :sem *are* shared
;; between head and parent.
;; The parent's :sem therefore has all of the
;; contents of the :head's sem, plus the :mod
;; of the complement, so that the adjunct complement
;; can modify the semantics of the heads.
(def adjective
  (let [one (atom :top)
        two (atom :top)]
    {:sem {:pred one
           :number? two}
     :head {:sem {:pred one
                  :number? two}}}))

(def noun
  (let [one (atom :top)
        two (atom :top)
        three (atom :top)
        four (atom :top)]
    {:sem {:pred one
           :quant two
           :ref three
           :context four}
     :head {:sem {:pred one
                  :quant two
                  :ref three
                  :context four}}}))

(def verb
  (let [one (atom :top)
        two (atom :top)
        three (atom :top)
        four (atom :top)]
    {:sem {:pred one
           :subj two
           :obj three}
     :head {:sem {:pred one
                  :subj two
                  :obj three}}}))

