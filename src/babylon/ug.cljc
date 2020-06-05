(ns babylon.ug
  (:require
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.dissoc :refer [dissoc-in]]))

;; This file consists of language independent, or 'universal'
;; grammar rules.

(def comp-is-root
  (let [root (atom :top)]
    {:root root
     :comp {:root root
            :canonical root}}))

;; TODO: remove: use nest/ instead.
(def cons-mod-nominal
  (let [ref (atom :top)
        quant (atom :top)
        head-mod (atom :top)
        sem (atom {:ref ref
                   :quant quant})
        comp-sem (atom {:arg ref
                        :quant quant})]
    {:mod {:first comp-sem
           :rest head-mod}
     ;; TODO: don't need this sem=sem; remove;
     ;; the head-sem def handles that.
     :sem sem
     :head {:sem sem
            :mod head-mod}
     :comp {:sem comp-sem}}))

;; TODO: remove: use nest/ instead.
;; e.g. "kleine honden" in: "kleine honden slapen":
;; [{:sem {:mod [1]} .kliene {:sem [1]} +honden]
(def nest-the-comp-mod
  (let [comp-sem (atom :top)
        rest-mod (atom :top)]
    {:sem {:mod {:first comp-sem
                 :rest rest-mod}}
     :head {:sem {:mod rest-mod}}
     :comp {:sem comp-sem}}))

;; TODO: remove: use nest/ instead.
(comment
  {:sem {:mod {:first [[1] :top]}
         :rest [[2] :top]}
   :head {:sem {:mod [2]}}
   :comp {:sem [1]}})
(def cons-mod-adverb
  (let [head-mod (atom :top)
        comp-sem (atom :top)]
    {:sem {:mod {:first comp-sem}
           :rest head-mod}
     :head {:sem {:mod head-mod}}
     :comp {:sem comp-sem}}))

(comment {:head {:sem {:subj {:ref [[1] :top]}}}
          :comp {:sem {:subj [1]}}})
(def subj-ref
  (let [head-mod (atom :top)
        subj-ref (atom :top)]
    {:head {:sem {:subj {:ref subj-ref}}}
     :comp {:sem {:subj subj-ref}}}))

(def head-aux
  (let [aux (atom :top)]
    {:aux aux
     :head {:aux aux}}))

(def head-first
  (let [head (atom :top)
        comp (atom :top)]
    {:head head
     :1 head
     :comp comp
     :2 comp}))

(def head-last
 (let [head (atom :top)
       comp (atom :top)]
   {:head head
    :1 comp
    :comp comp
    :2 head}))

;; root rules: which child (head or comp) is the root of a tree.
(def head-is-root
  (let [root (atom :top)]
    {:root root
     :head {:root root
            :canonical root}}))

(def head-modal
  (let [shared (atom :top)]
    {:modal shared
     :head {:modal shared}}))

(def head-reflexive
  (let [shared (atom :top)]
    {:reflexive shared
     :head {:reflexive shared}}))

(def head-slash
  (let [head-slash (atom :top)]
    {:slash head-slash
     :head {:slash head-slash}}))

(def slash-is-head-slash head-slash)

(def head-agr
  (let [agr (atom :top)]
    {:agr agr
     :head {:agr agr}}))

;; TODO: remove head-agr from here and
;; use head-agr from above.
;; TODO: :interogative? into :sem if
;; possible, so we don't need to specify it here.
(def head-rule
  (let [comp-cat (atom :top)
        comp-derivation (atom :top)
        head-agr (atom :top)
        head-cat (atom :top)
        head-derivation (atom :top)
        head-infl (atom :top)
        head-interogative (atom :top)
        reflexive (atom :top)]
    {:agr head-agr
     :cat head-cat
     :infl head-infl
     :interogative? head-interogative
     :reflexive reflexive
     :comp-derivation comp-derivation
     :head-derivation head-derivation
     :head {:agr head-agr
            :cat head-cat
            :infl head-infl
            :interogative? head-interogative
            :reflexive reflexive
            :head-derivation head-derivation
            :derivation head-derivation}
     :comp {:head-derivation comp-derivation
            :derivation comp-derivation}
     :phrasal true}))

(def head-sem
  (let [sem (atom :top)]
    {:sem sem
     :head {:sem sem}}))

(comment
  {:sem
   {:obj
    {:mod [[1] :top]}}
   :comp
   {:mod [1]}})
(def nest-comp-mod
  (let [mod (atom :top)]
    {:sem {:obj {:mod mod}}
     :comp {:mod mod}}))

(comment
  {:sem
   {:mod
    {:first [[1] :top]}},
   :comp
   {:sem [1]}})
(def sem-mod-first-is-comp-sem
  (let [mod (atom :top)]
    {:sem {:mod {:first mod}}
     :comp {:sem mod}}))

(def sem-mod-is-comp-mod
  (let [mod (atom :top)]
    {:sem {:mod mod}
     :comp {:mod mod}}))

(def nominal-phrase
  {:reflexive false
   :agr {:person :3rd}})

(def parent-sem-head
  (let [sem (atom :top)]
    {:sem sem
     :head {:parent-sem sem}}))      

;; use this to 'terminate' phrases that have modifiers:
(def parent-sem-comp
  (let [sem (atom :top)]
    {:sem sem
     :comp {:parent-sem sem}}))      

(def shared-agr
  (let [shared (atom :top)]
    {:comp {:agr shared}
     :head {:agr shared}}))

(def shared-def
  (let [shared (atom :top)]
    {:definite? shared
     :comp {:definite? shared}
     :head {:definite? shared}}))

(def shared-number
  (let [number (atom :top)]
    {:sem {:ref {:number number}}
     :agr {:number number}}))

(def sem-mod-is-empty
  {:sem {:mod []}})

(def times
  (let [sem1 (atom :top)
        sem2 (atom :top)]
    {:head {:sem sem1}
     :comp {:sem sem2}
     :sem {:pred :times
           :arg1 sem1
           :arg2 sem2}}))

