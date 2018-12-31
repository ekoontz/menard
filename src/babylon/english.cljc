(ns babylon.english
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.dissoc :refer [dissoc-in]]
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
  (-> "babylon/english/lexical-compile-rules.edn"
      io/resource
      slurp
      read-string
      ((fn [rule]
         (map #(eval %) rule)))))

(def lexicon
  (-> "babylon/english/lexicon.edn"
      io/resource
      slurp
      read-string
      ((fn [lexicon]
         (map #(eval %) lexicon)))
      (l/process lexical-rules)))

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
            m/morphology morphology
            g/morph-ps syntax-tree]
    (g/generate spec)))

(defn parse [expression]
  (binding [p/grammar grammar
            l/lexicon lexicon
            l/morphology morphology
            p/lookup-fn l/matching-lexemes]
    (p/parse expression)))

(defn demo []
  (println "Generation:")
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate :top))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:cat :verb}))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:cat :noun}))))))
  (println "Parsing:")
  (println "===")
  (count (take 10
               (repeatedly #(let [expression (morph (generate {:cat :top}))]
                              (println (->> (parse expression)
                                            (map syntax-tree)
                                            (string/join ", "))))))))

