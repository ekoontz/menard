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

(defn apply-rule [rule lexeme consequent antecedent]
  (let [result (unify lexeme consequent)]
    (log/info (str "apply-rule: lexeme: " lexeme "; consequent: " consequent "; antecedent:" antecedent "; result: " result))
    (cond (= :fail result)
          (let [error-message (str "matched antecedent: " antecedent
                                   ", but failed to unify lexeme:" (vec (u/serialize lexeme))
                                   " and consequent: " (vec (u/serialize consequent)))]
              (log/error error-message)
              (throw (Exception. error-message)))
          true
          (do (log/info (str "apply-rule: lexeme: " lexeme " with conseq: " consequent "= " result))
              [(unify result
                      (if rule
                        {:rule {rule true}}
                        :top))]))))

(def default-rule
  {:if :top
   :rule :default
   :then [:top]})

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
      [(unify lexeme {:rule {::no-rules-matched? true}})])))

(defn apply-rules-to-lexicon [lexicon rules]
  (into {}
        (for [[k lexemes] lexicon]
          (if (not (empty? lexemes))
            [k (->> lexemes
                    (mapcat (fn [lexeme]
                              (apply-rules rules lexeme)))
                    (map (fn [lexeme]
                           (merge lexeme
                                  {:canonical k}))))]))))

;; the lexicon itself. we use the lexical-compile-rules
;; to transform the human-readable entries into more complete
;; entries.
(def lexicon
  (-> "babylon/english/lexicon.edn"
      io/resource
      slurp
      read-string
      (apply-rules-to-lexicon lexical-rules)))

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

