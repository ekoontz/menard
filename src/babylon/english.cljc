(ns babylon.english
  (:require [clojure.string :as string]
            [babylon.lexiconfn :as l]
            [babylon.generate :as g]
            [babylon.grammar :as grammar]
            [babylon.morphology :as m]
            [babylon.parse :as p]
            [babylon.ug :as ug]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [unify]]))
;;
;; For generation and parsing of English.
;; 
(def misc-lexicon
  (-> "babylon/english/lexicon/misc.edn"
      io/resource
      slurp
      read-string
      (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules-1.edn"))
      (l/apply-rules-to-lexicon (l/read-rules "babylon/english/lexicon/rules-2.edn") true)))

(def noun-lexicon
  (-> "babylon/english/lexicon/nouns.edn"
      io/resource
      slurp
      read-string
      (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules-1.edn"))
      (l/apply-rules-to-lexicon (l/read-rules "babylon/english/lexicon/rules-2.edn") true)))

(def propernoun-lexicon
  (-> "babylon/english/lexicon/propernouns.edn"
      io/resource
      slurp
      read-string
      (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules-1.edn"))
      (l/apply-rules-to-lexicon (l/read-rules "babylon/english/lexicon/rules-2.edn") true)))

(def verb-lexicon
  (-> "babylon/english/lexicon/verbs.edn"
      io/resource
      slurp
      read-string
      (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules-1.edn"))
      (l/apply-rules-to-lexicon (l/read-rules "babylon/english/lexicon/rules-2.edn") true)))

(def lexicon
  (merge-with concat
              misc-lexicon
              noun-lexicon
              propernoun-lexicon
              verb-lexicon))

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

(defn generate-n
  "generate _n_ consecutive expressions that satisfy _spec_."
  [n spec]
  (binding [g/grammar grammar
            g/lexicon lexicon
            m/morphology morphology
            g/morph-ps syntax-tree
            g/shuffle? false]
      (vec (take n (g/grow-all (g/parent-with-head spec 0))))))

(defn parse [expression]
  (binding [p/grammar grammar
            l/lexicon lexicon
            l/morphology morphology
            p/lookup-fn l/matching-lexemes]
    (p/parse expression)))

(defn analyze [surface]
  (binding [l/lexicon lexicon
            l/morphology morphology]
    (l/matching-lexemes surface)))              

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
(defn benchmark []
  (repeatedly
   #(println
     (morph
      (time (generate
             {:cat :verb
              :pred :top
              :comp {:phrasal true}
              :head {:phrasal true
                     :comp {:phrasal true}}}))))))
