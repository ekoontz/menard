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

      (= (u/get-in structure [:surface])
         "??")                     
      (let [keep-going true ;; don't throw error if true.
            find-error (or (u/get-in structure [:infl :error])
                           (u/get-in structure [:agr :error])
                           (str "unknown error: " (u/strip-refs structure)))
            error-info (cond (= find-error :unspecified-agreement)
                             (u/get-in structure [:agr])
                             (= find-error :unspecified-infl)
                             (u/get-in structure [:infl] ::unknown-infl)
                             true (u/strip-refs structure))]
        (if keep-going
          error-info
          (do
            (println (str "failed to morph leaf: " find-error))
            (throw (Exception. (str "failed to morph leaf:" find-error))))))

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

