(ns babylon.morphology
  (:require [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [unify]]))

(defn morph-leaf
  "apply morphology to a leaf node of a tree; where
the morphology is a set of rules, each of which looks like:"
  [structure morphology]
  (if (= (u/get-in structure [:cat]) :verb)
    (log/debug (str "morph-leaf:" (u/strip-refs structure))))
  (let [matching-rules
        (if (not (u/get-in structure [:inflected?]))
          (filter (fn [rule]
                    (let [{u :u [from to] :g} rule
                          unified (unify u structure)]
                      (and (not (= :fail unified))
                           (re-find from (str (u/get-in structure [:canonical] ""))))))
                  morphology))
        exceptions (u/get-in structure [:exceptions])
        exceptionless (if exceptions
                        (dissoc structure :exceptions))
        first-matching-exception
        (if exceptions
          (first (filter #(not (= :fail %))
                         (map #(unify exceptionless %)
                              exceptions))))]
    (if first-matching-exception
      (log/debug (str "morph-leaf: " (u/strip-refs first-matching-exception))))
    (cond
      first-matching-exception
      (morph-leaf first-matching-exception morphology)

      (u/get-in structure [:surface])
      (u/get-in structure [:surface])

      (= true (u/get-in structure [:inflected?] false))
      (u/get-in structure [:canonical])
      
      (nil? matching-rules)
      (throw (Exception. (str "something went wrong: no rules matched for:"
                              (u/strip-refs structure))))

      (not (seq? matching-rules))
      (throw (Exception. (str "something went wrong: matching-rules "
                              "should be a sequence but it's a: "
                              (type matching-rules)
                              " for matching: " (u/strip-refs structure))))
      
      (not (empty? matching-rules))
      (let [{[from to] :g} (first matching-rules)]
         (log/debug (str "using matching rule:" (first matching-rules)))
        (clojure.string/replace (u/get-in structure [:canonical] "")
                                from to))
      
      (u/get-in structure [:canonical])
      (u/get-in structure [:canonical])
      true
      "_")))

