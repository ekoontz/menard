(ns babylon.ug
  (:require [dag_unify.core :as u :refer [unify]])
  (:require [dag_unify.dissoc :refer [dissoc-in]]))

;; universal grammar rules

(def head-sem
  (let [sem (atom :top)]
    {:sem sem
     :head {:sem sem}}))

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

(def head-rule
  (let [comp-cat (atom :top)
        comp-derivation (atom :top)
        head-agr (atom :top)
        head-cat (atom :top)
        head-derivation (atom :top)
        head-infl (atom :top)
        head-sem (atom :top)
        reflexive (atom :top)]
    {:agr head-agr
     :cat head-cat
     :infl head-infl
     :reflexive reflexive
     :sem head-sem
     :comp-derivation comp-derivation
     :head-derivation head-derivation
     :head {:agr head-agr
            :cat head-cat
            :infl head-infl
            :reflexive reflexive
            :sem head-sem
            :derivation head-derivation}
     :comp {:derivation comp-derivation}
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
     :head {:mod mod
            :agr agr
            :slash false
            :subcat {:1 complement :2 []}}
     :mod mod
     :subcat []
     :comp complement}))

(def subcat-1-1
  (let [agr (atom :top)
        subcat-1 (atom :top)]
    {:head {:agr agr
            :subcat subcat-1}
     :agr agr
     :subcat subcat-1}))

(def sem-mod
  (let [reference (atom :top)
        adjunct (atom {:ref reference})
        head-mod (atom :top)
        pred (atom :top)]
    {:head {:mod head-mod
            :sem {:pred pred
                  :ref reference}}
     :comp {:sem adjunct}
     :mod {:first adjunct
           :rest head-mod}
     :sem {:ref reference
           :pred pred}}))

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
                     :2 complement-2}}
     :agr agr
     :subcat {:1 complement-1
              :2 []}
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

;; for nbar2:
(def object-of-comp-is-head
  (let [head-sem (atom :top)]
    {:sem head-sem
     :comp {:sem {:obj head-sem}}}))

;; for nbar3:
(def subject-of-comp-is-head
  (let [head-sem (atom :top)
        head-agr (atom :top)]
    {:agr head-agr
     :sem head-sem
     :head {:agr head-agr}
     :comp {:agr head-agr
            :sem {:subj head-sem}}}))

(def comp-mod-is-subj-mod
  (let [comp-mod (atom :top)]
    {:sem {:subj-mod comp-mod}
     :comp {:mod comp-mod}}))

(def comp-mod-is-obj-mod
  (let [comp-mod (atom :top)]
    {:sem {:obj-mod comp-mod}
     :comp {:mod comp-mod}}))

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
