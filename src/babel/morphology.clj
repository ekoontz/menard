(ns babel.morphology
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [dag_unify.core :refer [fail? strip-refs unify unifyc]]))

(defn conjugation [specification]
  "compile a high-level conjugation pattern into 
  {:p/:g/:u} tuples, which are then useable by
  (defn analyze) and (defn conjugate)."
  (let [{infinitive :infinitive
         common :common
         forms :forms} specification]
    (log/debug (str "conjugation for: " infinitive " with common:" common))

    (map (fn [form]
           (let [suffix (first form)
                 info (unifyc (second form)
                              common)]
             ;; TODO: check for unifyc returning :fail.
             {:p [(re-pattern (str "^(.+)" suffix "$"))
                  (str "$1" infinitive)]
              :g [(re-pattern (str "^(.+)" infinitive "$"))
                  (str "$1" suffix)]
              :u info}))
         forms)))

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

(defn expand-replace-patterns [unify-with patterns]
  (mapcat (fn [pattern]
            (map (fn [pair]
                   {:u (unify unify-with
                              (:agr pattern))
                    :p pair})
                 (group-by-two (:p pattern))))
          patterns))

;; TODO: replace expand-replace-patterns with this: compile-patterns
(defn compile-patterns [tense-spec patterns]
  (map (fn [{g :g agr :agr p :p}]
         {:u {:agr (unify (get-in agr [:synsem :subcat :1 :agr] :top)
                          agr
                          (get-in tense-spec [:synsem]))}
          :p p
          :g g})
       patterns))

(defn do-replace-on [infinitive patterns]
  (if (not (empty? patterns))
    (let [[from to] patterns]
      (if (re-find from infinitive)
        (cons (string/replace infinitive from to)
              (do-replace-on infinitive (rest (rest patterns))))
        (do-replace-on infinitive (rest (rest patterns)))))))
