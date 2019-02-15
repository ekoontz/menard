(ns babylon.lexiconfn
  (:require
   [babylon.exception :refer [exception]]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [dag_unify.serialization :as s :refer [serialize]]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.dissoc :as d]))

;; These functions are used to a convert human-friendly lexicon
;; into a machine-friendly data structure.

(defn apply-rule [rule lexeme consequent antecedent]
  (let [result (unify lexeme consequent)]
    (log/debug (str "apply-rule: lexeme: " lexeme "; consequent: " consequent "; antecedent:" antecedent
                   "; result: " result))
    (cond (= :fail result)
          (let [error-message (str "rule: " rule " matched antecedent: " antecedent
                                   ", but failed to unify lexeme:" (vec (s/serialize lexeme))
                                   " and consequent: " (vec (s/serialize consequent)))]
              (log/error error-message)
              (throw (Exception. error-message)))
          true
          (do (log/debug (str "apply-rule: lexeme: " lexeme " with conseq: " consequent "= " result))
              [(unify result
                        (if rule
                          {:rules-matched {rule true}}
                          :top))]))))

(defn apply-rules [rules lexeme if-no-rules-matched?]
  (let [with-rules
         (->> rules
              (filter #(let [{antecedent :if
                              consequents :then} %]
                          (not (= :fail (unify antecedent lexeme)))))
              (mapcat #(let [{rule :rule
                              antecedent :if
                              consequents :then} %]
                        (mapcat (fn [consequent]
                                    (apply-rule rule lexeme consequent antecedent))
                                consequents))))]
    (cond (not (empty? with-rules))
          with-rules
          if-no-rules-matched?
          [(unify lexeme {:rules-matched {::no-rules-matched? true}})]
          true
          [lexeme])))

(defn apply-rules-to-lexicon [lexicon rules if-no-rules-matched?]
  (into {}
        (for [[k lexemes] lexicon]
          (if (not (empty? lexemes))
            [k (->> lexemes
                    (mapcat (fn [lexeme]
                               (apply-rules rules lexeme if-no-rules-matched?)))
                    (mapcat (fn [lexeme]
                               [(unify lexeme
                                       {:phrasal false
                                        :canonical (u/get-in lexeme [:canonical] k)})])))]))))
(defn apply-one-rule [rule lexeme]
  (let [[antecedent consequent] rule]
    (cond
       (= :fail (unify lexeme antecedent))
       []
       (fn? consequent)
       ;; 1. _consequent_ can be a function that
       ;; takes a map and returns a sequence of maps.
       (let [result (consequent lexeme)]
         (filter #(not (u/isomorphic? lexeme %))
                 result))

       true
       ;; 2. ..or (the more frequent use case)
       ;; _consequent_ can be another map that
       ;; we unify against the lexeme.
       (let [result (unify lexeme consequent)]
         (cond
           (= :fail result)
           []
           (u/isomorphic? result lexeme)
           []
           true
           [result])))))

(defn read-rules [rules-filename]
  (-> rules-filename
      io/resource
      slurp
      read-string
      ((fn [rule]
        (eval rule)))))

(defn apply-rules-in-order [lexicon rules]
  (if (empty? rules)
    lexicon
    (-> lexicon
        (apply-rules-in-order (rest rules))
        (apply-rules-to-lexicon [(first rules)] false))))

(defn apply-rules-cyclicly [rules lexemes]
  (let [one-round
        (->> rules
             (mapcat
              (fn [rule]
                (let [[antecedent consequent] rule]
                  (mapcat (fn [lexeme]
                            (apply-one-rule rule lexeme))
                          lexemes)))))]
    (cond (empty? one-round)
          lexemes
          true
          (apply-rules-cyclicly rules one-round))))

(defn process [lexicon rules]
  (into {} (for [[canonical lexemes]
                 lexicon]
             [canonical
              (let [lexemes (if (vector? lexemes) lexemes (vec lexemes))]
                (->> lexemes
                     (map (fn [lexeme]
                            (merge lexeme {:phrasal false
                                           :canonical canonical})))
                     (apply-rules rules)))])))

(def ^:dynamic lexicon)
(def ^:dynamic morphology)

(defn matching-lexemes
  "given a surface form _word_, find all matching lexical entries."
  [word]
  (let [from-inflected
         (let [canonical-forms
                 (filter #(not (nil? %))
                         (map (fn [rule]
                               (let [{u :u [from to] :p} rule]
                                 (if (re-find from word)
                                     {:canonical (clojure.string/replace word from to)
                                      :u u})))
                              morphology))]
            (mapcat #(filter (fn [lexeme]
                               (not (= :fail lexeme)))
                             (map (fn [lexeme]
                                    (unify (:u %) {:surface word} lexeme))
                                  (get lexicon (:canonical %))))
                    canonical-forms))]
    (if (not (empty? from-inflected))
      from-inflected
      (get lexicon word))))

(defn exceptions
  "generate exceptional lexical entries given a _canonical_ surface form and an input lexeme"
  [canonical lexeme]
  (map (fn [exception]
         (let [surface (:surface exception)]
           {surface
            [(unify (d/dissoc-in lexeme [:exceptions])
                    exception
                    {:exception true
                     :canonical canonical})]}))
       (:exceptions lexeme)))

(defn merge-with-all
  "having some personal cognitive difficulty in using apply with merge-with,
   so instead using this function as a workaround."
  [merge-with-fn args]
  (if (not (empty? args))
    (merge-with merge-with-fn
                (first args)
                (merge-with-all merge-with-fn (rest args)))))

(defn exceptions-for
  "generate all the exceptions possible for the sequence _lexemes_, each of which 
   has _canonical_ as the canonical form for the exception."
 [canonical lexemes]
 (->> lexemes
      (mapcat (fn [lexeme]
                (exceptions canonical lexeme)))
      (merge-with-all concat)))

(defn add-exceptions-to-lexicon
  "augment existing lexicon with new entries for all the exceptions possible for the input lexicon."
  [lexicon]
  (let [canonicals (keys lexicon)])
  (merge-with-all
   concat
   (cons lexicon
         (map (fn [canonical]
                (exceptions-for canonical (get lexicon canonical)))
              (keys lexicon)))))
