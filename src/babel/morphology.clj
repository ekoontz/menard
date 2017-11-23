(ns babel.morphology
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [dag_unify.core :refer [fail? strip-refs unify unifyc]]))

(defn analyze [surface-form lexicon replace-patterns]
  "Analyze a single surface form into a set of lexical forms."
  (concat (if (get lexicon surface-form)
            (get lexicon surface-form))
          (mapcat
           (fn [replace-pattern]
             (let [ ;; regular expression that matches the surface form
                   from (nth (:p replace-pattern) 0)]
               (log/debug (str "matching replace-pattern:" replace-pattern " against surface-form: " surface-form))
               (if (re-matches from surface-form)
                 (let [;; expression that is used by string/replace along with the first regexp and the surface form,
                       ;; to create the lexical string
                       to (nth (:p replace-pattern) 1)

                       ;; unifies with the lexical entry to create the inflected form.
                       unify-with (if (:u replace-pattern)
                                    (:u replace-pattern)
                                    :top) ;; default unify-with
                     
                       lex (string/replace surface-form from to)]
                   (filter (fn [result] (not (= :fail result)))
                           (map (fn [lexical-entry]
                                  (unifyc unify-with lexical-entry))
                                (get lexicon lex)))))))
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
                               (not (fail? (unifyc unify-against
                                                   unify-with))))
                        (do
                          (log/debug (str "found match: replace-pattern=" replace-pattern))
                          (string/replace infinitive from to)))))
                  replace-patterns)))))

(defn group-by-two [remaining]
  (if (> (count remaining) 1)
    (cons
     [(nth remaining 0)
      (nth remaining 1)]
     (group-by-two (rest (rest remaining))))))

(defn compile-patterns [unify-with patterns]
  (mapcat (fn [{g :g agr :agr p :p u :u}]
            (map (fn [pair]
                   {:u (unify unify-with
                              (or agr :top)
                              (or u :top))
                    :g g
                    :agr agr
                    :p pair})
                 (group-by-two p)))
       patterns))

(defn do-replace-on [infinitive patterns]
  (if (not (empty? patterns))
    (let [[from to] patterns]
      (if (re-find from infinitive)
        (cons (string/replace infinitive from to)
              (do-replace-on infinitive (rest (rest patterns))))
        (do-replace-on infinitive (rest (rest patterns)))))))
