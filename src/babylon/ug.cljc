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
           :reflexive reflexive}}))

(def head-sem-is-parent-sem
 (let [head-sem (atom :top)]
   {:sem head-sem
    :head {:sem head-sem}}))

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
  (let [complement (atom {:subcat []})]
    {:head {:subcat {:1 complement :2 []}}
     :subcat []
     :comp complement}))

(def subcat-1-1
  (let [reference (atom :top)
        adjunct (atom {:ref reference})
        head-mod (atom :top)
        pred (atom :top)
        subcat-1 (atom :top)]
    {:head {:mod head-mod
            :sem {:pred pred
                  :ref reference}
            :subcat {:1 subcat-1 :2 []}}
     :comp {:sem adjunct}
     :mod {:first adjunct
           :rest head-mod}
     :sem {:ref reference
           :pred pred}
     :subcat {:1 subcat-1 :2 []}}))

(def subcat-2
  (let [complement-1 (atom {:subcat []})
        complement-2 (atom {:subcat []})]
    {:head {:subcat {:1 complement-1
                     :2 complement-2}}
     :subcat {:1 complement-1}
     :comp complement-2}))
