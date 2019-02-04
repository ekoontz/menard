(ns babylon.english
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
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

(defn compile-lexicon [filename]
  (-> filename
      l/read-rules
      (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules/rules-0.edn"))
      (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules/rules-1.edn"))
      (l/apply-rules-to-lexicon (l/read-rules "babylon/english/lexicon/rules/rules-2.edn") true)))

(defn exceptions
  "generate exceptional lexical entries"
  [canonical lexeme]
  (map (fn [exception]
         (let [surface (:surface exception)]
           {surface
            [(unify (dag_unify.dissoc/dissoc-in lexeme [:exceptions])
                    exception
                    {:canonical canonical})]}))
       (:exceptions lexeme)))

(defn merge-with-all
  "having some personal cognitive difficulty in using apply with merge-with,
   so instead using this function as a workaround."
  [merge-with-fn args]
  (if (not (empty? args))
    (merge-with merge-with-fn
                (first args)
                (merge-with-all merge-with-fn (rest args)))
    {}))

(defn exceptions-for
  "generate all the exceptions possible for the sequence _lexemes_, each of which 
   has _canonical_ as the canonical form for the exception."
 [canonical lexemes]
 (->> lexemes
      (mapcat (fn [lexeme]
                (exceptions canonical lexeme)))
      (merge-with-all concat)))

(defn exceptions-all
  "generate all the exceptions possible for the input lexicon."
  [lexicon]
  (let [canonicals (keys lexicon)])
  (merge-with-all
   concat
   (map (fn [canonical]
          (exceptions-for canonical (get lexicon canonical)))
        (keys lexicon))))

(def lexemes (-> "babylon/english/lexicon/verbs.edn" l/read-rules (get "be")))
(def canonical "be")

(def exceptions-test
  (exceptions-for canonical lexemes))

(def lexicon
  (merge-with concat
    (compile-lexicon "babylon/english/lexicon/adjectives.edn")
    (compile-lexicon "babylon/english/lexicon/misc.edn")
    (compile-lexicon "babylon/english/lexicon/propernouns.edn")
    (compile-lexicon "babylon/english/lexicon/nouns.edn")
    (compile-lexicon "babylon/english/lexicon/verbs.edn")
    (-> "babylon/english/lexicon/verbs.edn"
                         l/read-rules
                         exceptions-all
                         (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules/rules-0.edn"))
                         (l/apply-rules-in-order (l/read-rules "babylon/english/lexicon/rules/rules-1.edn"))
                         (l/apply-rules-to-lexicon (l/read-rules "babylon/english/lexicon/rules/rules-2.edn") true))))


(def grammar
  (-> "babylon/english/grammar.edn"
      io/resource
      slurp
      read-string
      grammar/process))

(def morphology
  (concat
   (-> "babylon/english/morphology/nouns.edn"
       l/read-rules)
   (-> "babylon/english/morphology/verbs.edn"
       l/read-rules)))

(defn morph [structure]
  (binding [grammar/morph-leaf m/morph-leaf
            m/morphology morphology]
     (grammar/default-morph-fn structure)))

(defn syntax-tree
  "print a concise representation of a tree."
  [structure]
  (binding [grammar/morph-leaf m/morph-leaf
            m/morphology morphology]
     (grammar/syntax-tree structure)))

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec]
  (binding [g/lexicon lexicon
            m/morphology morphology
            g/morph-ps syntax-tree]
    (g/generate (unify spec
                       {:subcat []})
                grammar)))

(defn generate-n
  "generate _n_ consecutive in-order expressions that satisfy _spec_."
  [spec n]
  (binding [g/lexicon lexicon
            m/morphology morphology
            g/morph-ps syntax-tree
            g/shuffle? false]
    (take n (g/generate-all (unify spec
                                   {:subcat []})
                            grammar))))
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
  (println)
  (println "= transitive sentences =")
  (println)
  (count (take 10 (repeatedly #(println (morph (generate
                                                {:cat :verb
                                                 :reflexive false
                                                 :sem {:pred :see
                                                       :obj {:pred :top}}}))))))
  (println)
  (println "= reflexive sentences =")
  (println)
  (count (take 10 (repeatedly #(println (morph (generate {:cat :verb :sem {:pred :see} :reflexive true}))))))

  (println)
  (println "= 'long' sentences =")
  (println)
  (count
   (take 10
    (repeatedly
      #(println
         (morph
          (generate
           {:cat :verb
            :subcat []
            :pred :top
            :comp {:phrasal true
                   :head {:phrasal true}}
            :head {:phrasal true
                   :comp {:phrasal true
                          :head {:phrasal true}}}}))))))
  (println)
  (println "Parsing:")
  (println "===")
  (println)
  (count (take 10
               (repeatedly #(let [expression (morph (generate {:subcat []}))]
                              (println (->> (parse expression)
                                            (map syntax-tree)
                                            (string/join ", "))))))))

(defn capitalize-first-letter
  "clojure.string/capitalize is too much: it lower-cases every word in the string *except*
   the first letter. We need a function that capitalizes the first letter and leaves
   the rest alone (they might already be upper-case like the pronoun 'I', and should stay that way)."
  [input]
  (string/join ""
     (concat (string/capitalize (first input))
             (rest input))))

(defn an
  "change 'a' to 'an' if the next word starts with a vowel."
  [input]
  (-> input (string/replace #"\b([aA]) ([aeiou])" "$1n $2")))

(defn poetry-line []
  (->
   {:cat :verb
    :subcat []
    :pred :top
    :comp {:phrasal true
           :head {:phrasal true}}
    :head {:phrasal true
           :comp {:phrasal true
                  :head {:phrasal true}}}}
   generate
   morph
   an
   capitalize-first-letter
   (str ". ")))

(defn benchmark []
  (repeatedly
   #(println
     (time (poetry-line)))))

(defn poetry []
  (repeatedly
   #(println
     (poetry-line))))

