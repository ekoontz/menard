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
        mod (atom :top)
        pred (atom :top)
        ref (atom :top)]
    {:sem {:context context
           :mod mod
           :pred pred}
     :head {:sem {:context context
                  :pred pred}
            :mod mod}}))

;; same as above but also has :arg and :quant, which are used by
;; {:cat :noun} phrases.
(def nest-mod-nominal
  (let [arg (atom :top)
        context (atom :top)
        mod (atom :top)
        pred (atom :top)
        quant (atom :top)
        ref (atom :top)]
    {:sem {:arg arg
           :context context
           :mod mod
           :pred pred
           :quant quant
           :ref ref}
     :head {:sem {:arg arg
                  :context context
                  :pred pred
                  :quant quant
                  :ref ref}
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

(def shared-def
  (let [shared (atom :top)]
    {:definite? shared
     :comp {:definite? shared}
     :head {:definite? shared}}))

(def shared-number
  (let [number (atom :top)]
    {:sem {:ref {:number number}}
     :agr {:number number}}))

;;      <>
;;     /  \
;;   h<1>  1:c
;;
(def subcat-a
  (let [complement (atom {:subcat []})
        mod (atom :top)]
    {:head {:slash false
            :subcat {:1 complement :2 []}}
     :subcat []
     :comp complement}))

;;      <1>
;;     /   \
;;   h<1>   c
;;
(def subcat-b
  (let [subcat-1 (atom :top)]
    {:head {:subcat subcat-1}
     :subcat subcat-1}))

;;      <1>
;;     /   \
;;  h<1,2>  2
;;
(def subcat-c
  (let [complement-1 (atom {:top :top})
        complement-2 (atom {:top :top})]
    {:head {:subcat {:1 complement-1
                     :2 complement-2
                     :3 []}}
     :subcat {:1 complement-1
              :2 []}
     :comp complement-2}))

;;      <1>
;;     /   \
;;   h<2>   2:<1>
;;
(def subcat-d
  (let [subcat-of-complement (atom {:top :top})
        comp (atom {:subcat subcat-of-complement})]
    {:head {:subcat {:1 comp
                     :2 []}}
     :subcat subcat-of-complement
     :comp comp}))

;;      <2>
;;     /   \
;;  h<1,2>  1
;;
(def subcat-e
  (let [complement-1 (atom {:top :top})
        complement-2 (atom {:top :top})]
    {:head {:subcat {:1 complement-1
                     :2 complement-2
                     :3 []}}
     :subcat {:1 complement-2
              :2 []}
     :comp complement-1}))

;;          <3..>
;;          /    \
;;  h<[:sem 1],2>  2[:sem|:subj 1]
;;                  [<3..>       ]
;;
(def subcat-f
  (let [complement-subcat (atom :top)
        subject (atom :top)
        complement-semantics (atom {:subj subject})
        complement (atom {:sem complement-semantics
                          :subcat complement-subcat})]
    {:head {:sem {:subj subject
                  :obj complement-semantics}
            :subcat {:1 {:sem subject}}}
     :comp complement
     :subcat complement-subcat}))




