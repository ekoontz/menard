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
    {:head {:slash false
            :subcat {:1 complement :2 []}}
     :slash false
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
            :slash false
            :subcat {:1 subcat-1 :2 []}}
     :comp {:sem adjunct}
     :mod {:first adjunct
           :rest head-mod}
     :sem {:ref reference
           :pred pred
           :mod {:first adjunct
                 :rest head-mod}}
     :slash false
     :subcat {:1 subcat-1 :2 []}}))

(def subcat-2
  (let [complement-1 (atom {:subcat []})
        complement-2 (atom {:subcat []})]
    {:head {:slash false
            :subcat {:1 complement-1
                     :2 complement-2}}
     :slash false
     :subcat {:1 complement-1}
     :comp complement-2}))

(def subcat-1-slash
  (let [obj (atom {:top :top})
        sem (atom :top)
        subj (atom :top)]
    {:cat :verb
     :sem sem
     :subcat {:1 obj
              :2 []}
     :comp subj
     :slash true
     :head {:sem sem
            :slash false
            :subcat {:1 subj
                     :2 obj}}}))

(def subcat-new-rule
  (let [reference (atom :top)
        object-of-adjunct (atom {:ref reference})
        adjunct (atom {:ref reference})
        head-mod (atom :top)
        pred (atom :top)
        subcat-of-head (atom {:subcat {:1 :top
                                       :2 []}})
        subcat-of-adjunct (atom {:sem {:obj object-of-adjunct}
                                 :subcat {:1 subcat-of-head
                                          :2 []}})]
    {:subcat {:1 subcat-of-head
              :2 []}
     :sem {:mod {:first adjunct
                 :rest head-mod}
           :pred pred
           :ref reference}
     :head {:sem {:mod head-mod
                  :pred pred
                  :ref reference}
            :subcat subcat-of-head}
     :comp {:sem adjunct
            :subcat {:1 subcat-of-adjunct
                     :2 []}}}))

