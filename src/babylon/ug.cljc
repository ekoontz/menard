(ns babylon.ug
  (:require [dag_unify.core :as u :refer [unify]])
  (:require [dag_unify.dissoc :refer [dissoc-in]]))

;; universal grammar rules

(def head-rule
  (let [comp-cat (atom :top)
        head-agr (atom :top)
        head-cat (atom :top)
        reflexive (atom :top)]
    {:agr head-agr
     :cat head-cat
     :reflexive reflexive
     :head {:agr head-agr
            :cat head-cat
            :reflexive reflexive}
     :phrasal true}))

(def head-sem-is-parent-sem
  (let [head-sem (atom :top)]
    {:sem head-sem
     :head {:sem head-sem}}))

(def slash-is-head-slash
  (let [head-slash (atom :top)]
    {:slash head-slash
     :head {:slash head-slash}}))

(def head-first
  (let [head (atom :top)
        comp (atom :top)]
    (unify
     head-rule
     {:head head
      :1 head
      :comp comp
      :2 comp})))

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
  (let [reference (atom :top)
        adjunct (atom {:ref reference})
        head-mod (atom :top)
        agr (atom :top)
        pred (atom :top)
        subcat-1 (atom :top)]
    {:head {:mod head-mod
            :agr agr
            :sem {:pred pred
                  :ref reference}
            :subcat {:1 subcat-1 :2 []}}
     :agr agr
     :comp {:sem adjunct}
     :mod {:first adjunct
           :rest head-mod}
     :sem {:ref reference
           :pred pred}
     :subcat {:1 subcat-1 :2 []}}))

(def subcat-1-1-comp-subcat
  (let [comp-subcat (atom :top)
        agr (atom :top)
        complement (atom {:agr agr
                          :subcat comp-subcat})]
    {:head {:agr agr
            :subcat {:1 complement
                     :2 []}}
     :agr agr
     :comp complement
     :subcat comp-subcat}))

(def subcat-2
  (let [agr (atom :top)
        complement-1 (atom {:subcat []})
        complement-2 (atom {:subcat []})]
    {:head {:agr agr
            :subcat {:1 complement-1
                     :2 complement-2}}
     :agr agr
     :subcat {:1 complement-1
              :2 []}
     :comp complement-2}))

(def subcat-2-slash
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
            :phrasal false
            :slash false
            :subcat {:1 subj
                     :2 obj}}}))
;; for nbar2:
(def object-of-comp-is-head
  (let [head-sem (atom :top)]
    {:sem head-sem
     :comp {:sem {:obj head-sem}}}))

;; for nbar3:
(def subject-of-comp-is-head
  (let [head-sem (atom :top)]
    {:sem head-sem
     :comp {:sem {:subj head-sem}}}))

(def comp-mod-is-subj-mod
  (let [head-mod (atom :top)]
    {:sem {:subj-mod head-mod}
     :comp {:mod head-mod}}))
