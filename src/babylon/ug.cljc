(ns babylon.ug
  (:require [dag_unify.core :as u :refer [unify]])
  (:require [dag_unify.dissoc :refer [dissoc-in]]))

;; universal grammar rules
(def head-rule
  (let [comp-cat (atom :top)
        head-pred (atom :top)
        head-cat (atom :top)]
    {:cat head-cat
     :pred head-pred
     :head {:cat head-cat
            :pred head-pred}}))

(def head-last
  (let [head (atom :top)
        comp (atom :top)]
    {:head head
     :1 comp
     :comp comp
     :2 head}))

(def subcat-1
  (let [complement (atom {:subcat []})]
    {:head {:subcat {:1 complement}}
     :subcat []
     :comp complement}))

(defn process-grammar [grammar]
  (->>
   grammar
   (map #(apply unify
                (cons (dissoc-in % [:unify])
                      (map eval (:unify %)))))))
