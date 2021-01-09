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
;; - cons-and-nest-1
;; - cons-and-nest-2
;; - cons-only
;; - nest-only
;;
;; This exclusivity is enforced by the ::only-one-allowed-of below.

;; If it inherits from cons-and-nest-1,
;; cons-and-nest-2 or nest-only, then it should
;; also inherit
;; from exactly one of the following (in this
;; file below in 'part-2'):
;; - adjective
;; - noun
;; - verb

(comment "
          comp-modifies:
              sem|mod|first = comp|sem
")
(def comp-modifies
  (let [mod (atom :top)]
    {:mods-nested? true
     :sem {:mod {:first mod}}
     :comp {:mods-nested? true
            :sem mod}
     :head {:mods-nested? false}}))

(comment "
          The head's :mod is at the same level as its :sem.

          rest-of-mods-unnested:
              sem|mod|rest = head|mod
")
(def rest-of-mods-unnested
  (let [head-mod (atom :top)]
    (unify
     comp-modifies
     {::only-one-allowed-of :rest-of-mods-unnested
      :sem {:mod {:rest head-mod}}
      :head {:mod head-mod}})))


(comment "
          The head's :mod is nested within its :sem.

          rest-of-mods-nested:
              sem|mod|rest = head|sem|mod
")
(def rest-of-mods-nested
  (let [head-mod (atom :top)]
    (unify
     comp-modifies
     {::only-one-allowed-of :rest-of-mods-nested
      :sem {:mod {:rest head-mod}}
      :head {:sem {:mod head-mod}}})))


(comment "
          cons-only:
              mod|first = comp|sem
              mod|rest  = head|mod
")
(def cons-only
  (-> ug/head-sem
      (unify ug/head-rule)
      (unify
       (let [one (atom :top)
             two (atom :top)]
         {::only-one-allowed-of :cons-only
          :mods-nested? false
          :mod {:first one
                :rest two}
          :comp {:mods-nested? true
                 :sem one}
          :head {:mod two
                 :mods-nested? false}}))))

(comment "
          nest-only:
              sem|mod = head|mod
")
(def nest-only
  (let [one (atom :top)]
    {::only-one-allowed-of :nest-only
     :mods-nested? true
     :sem {:mod one}
     :mod nil
     :comp {:mods-nested? true}
     :head {:mods-nested? false
            :mod one}}))

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

