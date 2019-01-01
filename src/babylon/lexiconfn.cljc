(ns babylon.lexiconfn
  (:require
   [babylon.exception :refer [exception]]
   [clojure.tools.logging :as log]
   [dag_unify.core :as u :refer [unify]]))

;; These functions are used to a convert human-friendly lexicon
;; into a machine-friendly data structure.

(defn apply-rule [rule lexeme consequent antecedent]
  (let [result (unify lexeme consequent)]
    (log/debug (str "apply-rule: lexeme: " lexeme "; consequent: " consequent "; antecedent:" antecedent
                   "; result: " result))
    (cond (= :fail result)
          (let [error-message (str "matched antecedent: " antecedent
                                   ", but failed to unify lexeme:" (vec (u/serialize lexeme))
                                   " and consequent: " (vec (u/serialize consequent)))]
              (log/error error-message)
              (throw (Exception. error-message)))
          true
          (do (log/debug (str "apply-rule: lexeme: " lexeme " with conseq: " consequent "= " result))
              [(unify result
                        (if rule
                          {:rules-matched {rule true}}
                          :top))]))))

(defn apply-rules [rules lexeme]
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
    (if (not (empty? with-rules))
      with-rules
      [(unify lexeme {:rules-matched {::no-rules-matched? true}})])))

(defn apply-rules-to-lexicon [lexicon rules]
  (into {}
        (for [[k lexemes] lexicon]
          (if (not (empty? lexemes))
            [k (->> lexemes
                    (mapcat (fn [lexeme]
                               (apply-rules rules lexeme)))
                    (mapcat (fn [lexeme]
                               [(unify lexeme
                                       {:phrasal false
                                        :canonical k})])))]))))

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
