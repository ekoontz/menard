(ns menard.ug
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [dag_unify.core :as u :refer [unify]]
   [menard.lexiconfn :refer [read-and-eval]]
   [menard.model :refer [use-path]]))

(let [defs (-> "ug.edn" use-path read-and-eval)]
  (log/info (str "there are: " (count defs) " definitions."))
  (->> defs
       (map (fn [def]
              (let [k (:def def)
                    v (-> def (dissoc :unify) (dissoc :def))]
                (let [unify-with (:unify def)]
                  (let [value (if unify-with
                                (reduce unify (cons v
                                                    (map eval unify-with)))
                                v)]
                    (log/info (str "defining: " k))
                    (eval `(def ~(symbol k) ~value)))))))
       doall))

            


                   






