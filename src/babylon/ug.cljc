(ns babylon.ug
  (:require [dag_unify.core :as u :refer [unify]])
  (:require [dag_unify.dissoc :refer [dissoc-in]]))

;; This file consists of language independent, or 'universal'
;; grammar rules.

(def head-sem
  (let [sem (atom :top)]
    {:sem sem
     :head {:sem sem}}))

(def head-aux
  (let [aux (atom :top)]
    {:aux aux
     :head {:aux aux}}))

(def head-modal
  (let [shared (atom :top)]
    {:modal shared
     :head {:modal shared}}))

(def head-reflexive
  (let [shared (atom :top)]
    {:reflexive shared
     :head {:reflexive shared}}))

(def head-infl
  (let [infl (atom :top)]
    {:infl infl
     :head {:infl infl}}))

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

(def head-comp-rule ;; the :cat of the _comp_ is the :cat of the parent.
  (let [comp-cat (atom :top)]
    {:cat comp-cat
     :comp {:cat comp-cat}
     :phrasal true}))

(def slash-is-head-slash
  (let [head-slash (atom :top)]
    {:slash head-slash
     :head {:slash head-slash}}))

(def head-first
  (let [head (atom :top)
        comp (atom :top)]
    (unify
     head-rule
     {:phrasal true
      :head head
      :1 head
      :comp comp
      :2 comp})))

(def head-first-1 ;; used for e.g. intensifier-phrase, where [:head :cat] != [:cat].
  (let [head (atom :top)
        comp (atom :top)]
    {:phrasal true
     :head head
      :1 head
      :comp comp
      :2 comp}))

(def head-last
 (let [head (atom :top)
       comp (atom :top)]
   (unify
    head-rule
    {:head head
     :1 comp
     :comp comp
     :2 head})))

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

(def subcat-1-slash
  (let [complement (atom {:subcat []})
        agr (atom :top)]
    {:agr agr
     :slash false
     :head {:agr agr
            :slash true
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

(def shared-quant
  (let [shared (atom :top)]
    {:comp {:sem {:quant shared}}
     :head {:sem {:quant shared}}}))

(def shared-agr
  (let [shared (atom :top)]
    {:comp {:agr shared}
     :head {:agr shared}}))

(def subcat-1-1-comp-subcat
  (let [comp-subcat (atom {:1 {:top :top}
                           :2 []})
        reflexive (atom :top)
        agr (atom :top)
        complement (atom {:agr agr
                          :reflexive reflexive
                          :subcat comp-subcat})]
    {:head {:agr agr
            :subcat {:1 complement
                     :2 []}}
     :agr agr
     :comp complement
     :reflexive reflexive
     :subcat comp-subcat}))

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

(def subcat-2-1-slash
  (let [obj-sem (atom :top)
        obj (atom {:sem obj-sem}) ;; verb must be transitive: prevent obj from being simply :unspec.
        subj-sem (atom :top)
        subj (atom {:subcat []
                    :sem subj-sem})
        sem (atom {:obj obj-sem
                   :subj subj-sem})]
    {:cat :verb
     :sem sem
     :comp subj
     :subcat {:1 obj
              :2 []}
     :slash true
     :head {:sem sem
            :slash true
            :subcat {:1 subj
                     :2 obj}}}))

(def subcat-2-2-slash
  (let [one (atom :top)
        two (atom :top)
        three (atom {:subcat {:1 one
                              :2 two}})]
    {:subcat {:1 one
              :2 two}
     :head {:subcat {:1 one
                     :2 three}}
     :comp three}))

;; *head<1,2,3> + [3] comp] -> [parent <1,2>]
(def subcat-3
  (let [agr (atom :top)
        complement-1 (atom {:top :top}) ;;  we use {:top :top} to prevent unifying with nil or :unspec.
        complement-2 (atom {:top :top}) ;;  we use {:top :top} to prevent unifying with nil or :unspec.
        complement-3 (atom {:top :top})] ;; we use {:top :top} to prevent unifying with nil or :unspec.
    {:subcat {:1 complement-1
              :2 complement-2
              :3 []}

     :head {:agr agr
            :subcat {:1 complement-1
                     :2 complement-2
                     :3 complement-3
                     :4 []}}
     :agr agr
     :comp complement-3}))

;; root rules: which child (head or comp) is the root of a tree.
(def head-is-root
  (let [root (atom :top)]
    {:root root
     :head {:root root
            :canonical root}}))

(def comp-is-root
  (let [root (atom :top)]
    {:root root
     :comp {:root root
            :canonical root}}))

(def nominal-phrase
  {:reflexive false
   :agr {:person :3rd}})


(def raise-the-comp-subcat
  (let [two (atom :top)
        one (atom {:subcat {:1 two}})]
    {:subcat {:1 two}
     :head {:subcat {:1 one}}
     :comp one}))

(def raise-two-to-one
  (let [one (atom :top)
        two (atom :top)]
    {:subcat {:1 two
              :2 []}
     :comp one
     :head {:subcat {:1 one
                     :2 two}}}))

;; "what did she [+look .at] ?"
;;
;; vp-slash <1,3>
;; |`- v<1,2> "look"
;; |
;;  `- prep<2<3>> "at"
;;
;; TODO: rename: not vp-specific.
(def vp-slash
  (let [one (atom {:top :top})
        three (atom {:top :top})
        two (atom {:subcat {:1 three
                            :2 []}})]
    {:subcat {:1 one
              :2 three
              :3 []}
     :head {:subcat {:1 one
                     :2 two
                     :3 []}}
     :comp two}))

;; also used for non-adjunct phrase-structure rules
;; e.g. in "sees the cat", "the cat" is an argument of the verb,
;; but allows for one modifier of the head:
;; vp    [ mod  <1,2..> ]
;; |
;; |`-v  [ mod  <1>     ]
;;  `-np [ mod  <2..>   ]
;;
(def complement-is-argument-one-head-mod
  (let [head-mod (atom :top)
        comp-mod (atom :top)]
    {:mod {:first head-mod
           :rest comp-mod}
     :head {:mod {:first head-mod}}
     :comp {:mod comp-mod}}))

;; this is used for non-adjunct phrase-structure rules.
;; e.g. in "the little cat"
;;
;; np      [ mod  1 ]
;; |
;; |`-det
;;  `-nbar [ mod  1 ]
;;
(def complement-is-argument-no-comp-mod
  (let [head-mod (atom :top)]
    {:mod head-mod
     :head {:mod head-mod}}))

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


