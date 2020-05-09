(ns babylon.subcat
  (:require
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.dissoc :refer [dissoc-in]]))

;; This file consists of language independent, or 'universal'
;; grammar rules.

;;      <>
;;     /  \
;;   h<1>  1:c
;;
(def a
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
(comment
  {:subcat [[1] :top]
   :head {:subcat [1]}})
(def b
  (let [one (atom :top)]
    {:subcat one
     :head {:subcat one}}))


;;      <1>
;;     /   \
;;  h<1,2>  2
;;
(def c
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
(def d
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
(def e
  (let [complement-1 (atom {:top :top})
        complement-2 (atom {:top :top})]
    {:head {:subcat {:1 complement-1
                     :2 complement-2
                     :3 []}}
     :subcat {:1 complement-2
              :2 []}
     :comp complement-1}))

(def h
  (let [one (atom :top)
        mod (atom :top)
        sem (atom :top)
        two (atom {:mod mod
                   :sem sem
                   :subcat {:2 one}})]
    {:sem {:obj {:mod mod}}
     :subcat {:1 one}
     :head {:subcat {:1 two}}
     :comp two}))

(def i
  (let [one   (atom :top)
        three (atom :top)
        two   (atom {:subcat {:1 one
                              :2 three}})]
    {:sem {:mod []}
     :subcat {:1 one
              :2 three}
     :head {:subcat {:1 one
                     :2 two}}
     :comp two}))
