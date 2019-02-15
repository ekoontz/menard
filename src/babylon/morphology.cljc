(ns babylon.morphology
  (:require [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [unify]]))

(def ^:dynamic morphology)

(defn morph-leaf
  "apply morphology to a leaf node of a tree; where
the morphology is a set of rules, each of which looks like:"
  [structure]
  (log/debug (str "morphology of:" structure))
  (let [matching-rules
        (filter (fn [rule]
                  (let [{u :u [from to] :g} rule
                        unified (unify u structure)]
                    (and (not (= :fail unified))
                         (re-find from (str (u/get-in structure [:canonical] ""))))))
                morphology)
        exceptions (u/get-in structure [:exceptions])
        exceptionless (if exceptions
                        (dissoc structure :exceptions))
        first-matching-exception
        (if exceptions
          (first (filter #(not (= :fail %))
                         (map #(unify exceptionless %)
                              exceptions))))]
    
    (cond
      first-matching-exception
      (morph-leaf first-matching-exception)

      (u/get-in structure [:surface])
      (u/get-in structure [:surface])

      (not (empty? matching-rules))
      (let [{[from to] :g} (first matching-rules)]
         (log/debug (str "using matching rule:" (first matching-rules)))
        (clojure.string/replace (u/get-in structure [:canonical] "")
                                from to))
      
      (u/get-in structure [:canonical])
      (u/get-in structure [:canonical])
      true
      "_")))

