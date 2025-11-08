(ns menard.crossproduct
  (:require [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn- inner-function [[firstm & restms]]
  (when firstm
    (lazy-cat
     (map #(unify firstm %) restms)
     (inner-function restms))))

(defn- cleanup [[firstm & restms]]
  (when firstm
    (let [overs (->> restms
                     (map (fn [m]
                            (= (serialize m)
                               (serialize (unify firstm m)))))
                     (filter true?))]
      (if (seq overs)
        (cleanup restms)
        (cons firstm (cleanup restms))))))

(defn- cross-product-1 [maps]
  (->
   maps
   (->> inner-function
        (remove #(= % :fail)))
   set
   ((fn [s]
      (if (empty? s)
        (set maps)
        (cross-product-1 s))))))

(defn cross-product [maps]
  (-> maps set cleanup cross-product-1))
