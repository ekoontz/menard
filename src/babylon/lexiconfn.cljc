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
                               (fn? consequent)
                               ;; 1. _consequent_ can be a function that
                               ;; takes a map and returns a sequence of maps..
                               (let [result (consequent lexeme)]
                                 (if (empty? result)
                                   [lexeme]
                                   result))
                               true
                               ;; 2. ..or (the more frequent use case)
                               ;; _consequent_ can be another map that
                               ;; we unify against the lexeme.
                               (let [result (unify lexeme consequent)]
                                 (cond (not (= :fail result))
                                       [result]
                                       true
                                       [lexeme]))))
                          lexemes)))))]
    (if (= (count (set one-round))
           (count (set (vec (clojure.set/union lexemes one-round)))))
      ;; we are done.
      lexemes
      ;; set of lexemes changed as a result of applying rules,
      ;; so re-apply rules:
      (apply-rules rules one-round))))

;; TODO: for now _rules_ assumes one-to-one rules:
;; (source lexeme -> compiled lexeme),
;; but should also support one-to-many rules:
;; (source lexeme -> collection(compiled lexeme).
(defn process [lexicon rules]
  (into {} (for [[canonical lexemes]
                 lexicon]
             [canonical
              (->> lexemes
                   (map (fn [lexeme]
                          (merge lexeme {:phrasal false
                                         :canonical canonical})))
                   (apply-rules rules))])))
