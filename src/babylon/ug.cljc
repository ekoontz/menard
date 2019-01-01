(ns babylon.ug
  (:require [dag_unify.core :as u :refer [unify]])
  (:require [dag_unify.dissoc :refer [dissoc-in]]))

;; universal grammar rules
(def head-rule
  (let [comp-cat (atom :top)
        head-agr (atom :top)
        head-pred (atom :top)
        head-cat (atom :top)
        reflexive (atom :top)]
    {:agr head-agr
     :cat head-cat
     :reflexive reflexive
     :pred head-pred
     :head {:agr head-agr
            :cat head-cat
            :reflexive reflexive
            :pred head-pred}}))

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

(def subcat-1
  (let [complement (atom {:subcat []})]
    {:head {:subcat {:1 complement :2 []}}
     :subcat []
     :comp complement}))

(def subcat-2
  (let [complement-1 (atom {:subcat []})
        complement-2 (atom {:subcat []})]
    {:head {:subcat {:1 complement-1
                     :2 complement-2}}
     :subcat {:1 complement-1}
     :comp complement-2}))

(defn process [grammar]
  (->>
   grammar
   (map #(apply unify
                (cons (dissoc % :unify)
                      (map eval (:unify %)))))))
