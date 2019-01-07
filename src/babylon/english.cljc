(ns babylon.english
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [babylon.lexiconfn :as l]
            [babylon.generate :as g]
            [babylon.grammar :as grammar]
            [babylon.morphology :as m]
            [babylon.parse :as p]
            [babylon.ug :as ug]))
;;
;; For generation and parsing of English.
;; 
(def lexical-rules
  (-> "babylon/english/lexical-rules.edn"
      io/resource
      slurp
      read-string
      ((fn [rule]
         (map #(eval %) rule)))))

(defn apply-rule [lexeme consequent antecedent]
  (let [result (unify lexeme consequent)]
    (cond (= :fail result)
          (let [error-message (str "matched antecedent: " antecedent
                                   ", but failed to unify lexeme:" (vec (u/serialize lexeme))
                                   " and consequent: " (vec (u/serialize consequent)))]
              (log/error error-message)
              (throw (Exception. error-message)))
          (u/isomorphic? lexeme result)
          (do (log/info (str "reached fixed point with unify lexeme:" lexeme " and consequent: " consequent "."))
              [])
          true
          (do (log/info (str "apply-rule: lexeme: " lexeme " with conseq: " consequent))
              [result]))))

(defn apply-rules [lexeme rules]
  (->> rules
       (filter #(let [{antecedent :if
                       consequents :then} %]
                  (not (= :fail (unify antecedent lexeme)))))
       (mapcat #(let [{antecedent :if
                       consequents :then} %]
                  (mapcat (fn [consequent]
                            (apply-rule lexeme consequent antecedent))
                          consequents)))))

(defn apply-rule-to [rule lexemes]
  (cond (map? rule)
        (map #(unify rule %
                     lexemes))
        (or (vector? rule)
            (seq? rule))
        (let [rules rule]
          (->>
           lexemes
           (mapcat (fn [lexeme]
                     (->> rule
                          (map (fn [rule]
                                 (let [[ante conseq] rule]
                                    (->>
                                     conseq
                                     (map (fn [each-conseq]
                                             (unify each-conseq lexeme)))
                                     (filter #(not (= :fail %))))))))))))
        true
        (throw (Exception. (str "could not process rule with this type: ")))))
         

;; the lexicon itself. we use the lexical-compile-rules
;; to transform the human-readable entries into more complete
;; entries.
(def lexicon
  (-> "babylon/english/lexicon.edn"
      io/resource
      slurp
      read-string))
;;      (l/process lexical-rules)))

;; used during generation to turn a single
;; compiled lexical entry into multiple inflected
;; forms as part of a syntax tree.
;; TODO: should be used during parsing as well to avoid
;; allowing incorrect agreeement: e.g. *"the cats see itself"
(def lexical-default-rules
  (concat
   (-> "babylon/english/lexical-default-rules.edn"
       io/resource
       slurp
       read-string
       ((fn [rule]
          (map #(eval %) rule))))))

(def grammar
  (-> "babylon/english/grammar.edn"
      io/resource
      slurp
      read-string
      ug/process))

(def morphology
  (concat
   (-> "babylon/english/morphology/nouns.edn"
       clojure.java.io/resource
       slurp
       read-string)
   (-> "babylon/english/morphology/verbs.edn"
       clojure.java.io/resource
       slurp
       read-string)))

(defn morph [structure]
  (binding [grammar/morph-leaf m/morph-leaf
            m/morphology morphology]
    (grammar/default-morph-fn structure)))

(defn syntax-tree [structure]
  (binding [grammar/morph-leaf m/morph-leaf
            m/morphology morphology]
     (grammar/syntax-tree structure)))

(defn generate [spec]
  (binding [g/grammar grammar
            g/lexicon lexicon
            g/lexical-default-rules lexical-default-rules
            m/morphology morphology
            g/morph-ps syntax-tree]
    (g/generate spec)))

(defn grow-all [spec]
  (binding [g/grammar grammar
            g/lexicon lexicon
            g/lexical-default-rules lexical-default-rules
            m/morphology morphology
            g/morph-ps syntax-tree]
    (g/grow-all (g/parent-with-head spec 0))))

(defn parse [expression]
  (binding [p/grammar grammar
            l/lexicon lexicon
            l/morphology morphology
            p/lookup-fn l/matching-lexemes]
    (p/parse expression)))

(defn demo []
  (load "grammar")
  (println "Generation:")
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate :top))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:cat :verb}))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:cat :noun}))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:pred :see :reflexive true}))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:pred :see :reflexive false}))))))
  (println "Parsing:")
  (println "===")
  (count (take 10
               (repeatedly #(let [expression (morph (generate {:subcat []}))]
                              (println (->> (parse expression)
                                            (map syntax-tree)
                                            (string/join ", "))))))))

