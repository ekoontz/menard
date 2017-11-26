(ns babel.morphology
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [dag_unify.core :refer [fail? strip-refs unify]]))

(declare group-by-two)

(defn analyze [surface-form lexicon replace-patterns]
  "Analyze a single surface form into a set of lexical forms."
  (concat (if (get lexicon surface-form)
            (get lexicon surface-form))
          (mapcat
           (fn [{g :g agr :agr p :p u :u}]
             (mapcat
              (fn [[from to]]
                (if (re-matches from surface-form)
                  (let [;; unifies with the lexical entry to create the inflected form.
                        unify-with (or u :top)
                        lex (string/replace surface-form from to)]
                    (filter (fn [result] (not (= :fail result)))
                            (map (fn [lexical-entry]
                                   (unify unify-with lexical-entry))
                                 (get lexicon lex))))))
              (group-by-two p)))
           replace-patterns)))

(defn conjugate [infinitive unify-with replace-patterns]
  "Conjugate an infinitive into a surface form by taking the first 
   element of replace-patterns where the element's :u unifies successfully with
   unify-with."
  (log/debug (str "conjugate: infinitive=" infinitive "; unify-with=" (dissoc (strip-refs unify-with)
                                                                              :dag_unify.core/serialized)))
  (first
   (take 1
         (remove #(nil? %)
                 (map
                  (fn [replace-pattern]
                    (let [from (nth (:g replace-pattern) 0)
                          to (nth (:g replace-pattern) 1)
                          unify-against (or (:u replace-pattern) :top)]
                      (if (and from to
                               (re-matches from infinitive)
                               (not (fail? (unify unify-against
                                                  unify-with))))
                        (do
                          (log/debug (str "found match: replace-pattern=" replace-pattern))
                          (string/replace infinitive from to)))))
                  replace-patterns)))))

(defn compile-patterns [unify-with patterns]
  (map (fn [pattern]
         (merge pattern
                (let [{g :g agr :agr p :p u :u} pattern]
                  (let [u (unify unify-with
                                 (if agr {:synsem {:agr agr}} :top)
                                 (or u :top))]
                    {:u u
                     :g g
                     :agr agr
                     :p p}))))
       patterns))

(defn do-replace-on [infinitive patterns]
  (if (not (empty? patterns))
    (let [[from to] patterns]
      (if (re-find from infinitive)
        (cons (string/replace infinitive from to)
              (do-replace-on infinitive (rest (rest patterns))))
        (do-replace-on infinitive (rest (rest patterns)))))))

(defn group-by-two [remaining]
  (if (not (empty? (rest remaining)))
    (cons
     [(nth remaining 0)
      (nth remaining 1)]
     (group-by-two (rest (rest remaining))))))
