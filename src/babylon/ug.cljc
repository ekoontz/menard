(ns babylon.ug
  (:require [dag_unify.core :as u :refer [unify]])
  (:require [dag_unify.dissoc :refer [dissoc-in]]))

;; This file consists of language independent, or 'universal'
;; grammar rules.

(def comp-is-root
  (let [root (atom :top)]
    {:root root
     :comp {:root root
            :canonical root}}))

;; this is used for adjunct phrase structure rules
;; e.g. in "sits on the chair", "on the chair" is
;; an adjunct:
;;
;; vp-adj [ sem 1       ]
;; |      [ mod <2,3>   ]
;; |
;; |`-v   [ sem 1       ]
;; |
;;  `-pp  [ sem 2[arg 1]]
;;        [ mod 3       ]
;;
(def cons-mod
  (let [ref (atom :top)
        head-mod (atom :top)
        sem (atom {:ref ref})
        comp-sem (atom {:arg ref})]
    {:mod {:first comp-sem
           :rest head-mod}
     :sem sem
     :head {:sem sem
            :mod head-mod}
     :comp {:sem comp-sem}}))

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

(def nest-mod
  (let [arg (atom :top)
        context (atom :top)
        iobj (atom :top)
        mod (atom :top)
        obj (atom :top)
        pred (atom :top)
        quant (atom :top)
        ref (atom :top)
        subj (atom :top)]
    {:sem {:arg arg
           :context context
           :mod mod
           :obj obj
           :pred pred
           :quant quant
           :ref ref
           :subj subj}
     :head {:sem {:arg arg
                  :context context
                  :obj obj
                  :pred pred
                  :quant quant
                  :ref ref
                  :subj subj}
            :mod mod}}))

(def nominal-phrase
  {:reflexive false
   :agr {:person :3rd}})

(def parent-sem
  (let [sem (atom :top)]
    {:sem sem
     :head {:parent-sem sem}}))      

(def shared-agr
  (let [shared (atom :top)]
    {:comp {:agr shared}
     :head {:agr shared}}))

(def shared-quant
  (let [shared (atom :top)]
    {:comp {:sem {:quant shared}}
     :head {:sem {:quant shared}}}))

(def subcat-1
  (let [complement (atom {:subcat []})
        agr (atom :top)
        mod (atom :top)]
    {:agr agr
     :head {:agr agr
            :slash false
            :subcat {:1 complement :2 []}}
     :subcat []
     :comp complement}))

(def subcat-1-1
  (let [agr (atom :top)
        subcat-1 (atom :top)]
    {:head {:agr agr
            :subcat subcat-1}
     :agr agr
     :subcat subcat-1}))

;; *head<1,2> + [2] comp] -> [parent <1>]
(def subcat-2
  (let [agr (atom :top)
        complement-1 (atom {:top :top})
        complement-2 (atom {:top :top})]
    {:head {:agr agr
            :subcat {:1 complement-1
                     :2 complement-2
                     :3 []}}
     :agr agr
     :subcat {:1 complement-1
              :2 []
              :3 []}
     :comp complement-2}))

