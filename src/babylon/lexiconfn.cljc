(ns babylon.lexiconfn
  (:require
   [babylon.exception :refer [exception]]
   [clojure.tools.logging :as log]
   [dag_unify.core :as u :refer [unify]]))

;; These functions are used to a convert human-friendly lexicon
;; into a machine-friendly data structure.
(defn apply-rules [rules lexemes]
  (let [one-round
        (->> rules
             (mapcat
              (fn [rule]
                (let [[antecedent consequent] rule]
                  (mapcat (fn [lexeme]
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
                                  [result]))))
                          lexemes)))))]
    (cond (empty? one-round)
          lexemes
          true
          (apply-rules rules one-round))))

(defn process [lexicon rules]
  (into {} (for [[canonical lexemes]
                 lexicon]
             [canonical
              (->> lexemes
                   (map (fn [lexeme]
                          (merge lexeme {:phrasal false
                                         :canonical canonical})))
                   (apply-rules rules))])))

(def ^:dynamic lexicon)
(def ^:dynamic morphology)

(defn matching-lexemes [word]
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
