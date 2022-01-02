(ns menard.ug
  (:require
   [dag_unify.core :as u :refer [unify]]
   [menard.lexiconfn :refer [read-and-eval]]
   [menard.model :refer [use-path]]))

(let [defs (-> "ug.edn" use-path read-and-eval)]
  (->> defs
       (map (fn [def]
              (let [k (:def def)
                    v (-> def (dissoc :unify) (dissoc :def))]
                (let [unify-with (:unify def)]
                  (let [value (if unify-with
                                (reduce unify (cons v
                                                    (map eval unify-with)))
                                v)]
                    (eval `(def ~(symbol k) ~value)))))))
       doall))
