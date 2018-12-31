(ns babylon.english
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.dissoc :refer [dissoc-in]]
            [babylon.lexiconfn :as l]
            [babylon.generate :as g]
            [babylon.grammar :as grammar]
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

(defn morph-leaf [structure]
  (log/debug (str "morphology of:" structure))
  (let [matching-rules
        (filter (fn [rule]
                  (let [{u :u [from to] :g} rule
                        unified (unify u structure)]
                    (and (not (= :fail unified))
                         (re-find from (u/get-in structure [:canonical])))))
                morphology)]
    (cond
      (u/get-in structure [:surface])
      (u/get-in structure [:surface])

      (not (empty? matching-rules))
      (let [{[from to] :g} (first matching-rules)]
         (log/debug (str "using matching rule:" (first matching-rules)))
        (clojure.string/replace (u/get-in structure [:canonical])
                                from to))
      
      (u/get-in structure [:canonical])
      (u/get-in structure [:canonical])
      true
      "_")))

(defn morph [structure]
  (binding [grammar/morph-leaf morph-leaf]
    (grammar/default-morph-fn structure)))

(defn syntax-tree [structure]
  (binding [grammar/morph-leaf morph-leaf]
     (grammar/syntax-tree structure)))

(defn generate [spec]
  (binding [g/grammar grammar
            g/lexicon lexicon
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
                              (binding [grammar/morph-leaf morph-leaf]
                                (println (->> (parse expression)
                                              (map syntax-tree)
                                              (string/join ", ")))))))))

