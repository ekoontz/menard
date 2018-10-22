(ns babel.generate.lab
  (:require [babel.generate :as g]
            [dag_unify.core
             :as u :refer [strip-refs unify]]
            [clojure.string :as string]))

(def baby-language
  {:grammar
   (->>
     [(let [one (atom :top)
            two (atom :top)
            cat (atom :top)]

        ;; rule 1: phrase where both children are lexemes,
        ;; and both lexemes have share their :cat value.
        (unify {:rule "A"
                :head {:cat cat
                       :phrasal false}
                :comp {:cat cat
                       :phrasal false}}
               {:1 one :head one
                :2 two :comp two}))

      (let [one (atom :top)
            two (atom :top)]
        (unify {:rule "B"
                :head {:phrasal false}
                :comp {:rule "A"
                       :phrasal true}}
               {:1 one
                :head one
                :2 two
                :comp two}))]
     (map #(unify % {:phrasal true}))
     (remove #(= :fail %)))
   
   :lexicon
   (into {} (for [[surface lexemes-for-surface]
                  {"ba" [{:cat :ba}]
                   "da" [{:cat :da}]
                   "ga" [{:cat :ga}]
                   "ma" [{:cat :ma}]}]
              [surface (map (fn [lexeme]
                              (merge lexeme {:phrasal false
                                             :surface surface}))
                        lexemes-for-surface)]))})

(def grammar (:grammar baby-language))
(def lexicon (:lexicon baby-language))

(defn morph [structure]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure

        (u/get-in structure [:surface])
        (morph (u/get-in structure [:surface]))
        
        (and (not (nil? (u/get-in structure [:1])))
             (not (nil? (u/get-in structure [:2]))))
        (str
         "[" (:rule structure) " "
          (->
           (->> 
            [(u/get-in structure [:1])
             (u/get-in structure [:2])]
            (map morph)
            (string/join " ")
            string/trim))
         "]")
        
        true (str "[" (:rule structure) " " (morph (u/get-in structure [:1])) "]")))

(defn generate [spec]
  (binding [g/grammar (shuffle (:grammar baby-language))
            g/lexicon (:lexicon baby-language)
            g/println? true
            g/morph-ps morph]
    (g/generate spec)))

(def working-spec-a-expression
  (generate spec-a))
