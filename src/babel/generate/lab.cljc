(ns babel.generate.lab
  (:require [babel.generate :as g]
            [dag_unify.core
             :as u :refer [strip-refs unify]]
            [clojure.string :as string]))

(def head-first
  (let [one (atom :top)
        two (atom :top)]
    {:phrasal true
     :head one
     :comp two
     :1 one
     :2 two}))

(def comp-first
  (let [one (atom :top)
        two (atom :top)]
    {:phrasal true
     :head two
     :comp one
     :1 one
     :2 two}))

(def baby-language
  {:grammar
   (->>
    [(let [cat (atom :n)
           surface (atom :top)]
       ;; rule "A": phrase where both children are lexemes,
       ;; and both lexemes have share their :cat value and surface value.
       (unify {:rule "A"
               :head {:cat cat
                      :surface surface
                      :phrasal false}
               :comp {:cat cat
                      :surface surface
                      :phrasal false}}
              head-first))
     
     ;; rule "B": phrase where the comp is a rule-"A".
     (unify {:rule "B"
             :head {:phrasal false
                    :cat :v}
             :comp {:rule "A"
                    :phrasal true}}
            comp-first)]
    (remove #(= :fail %)))
   
   :lexicon
   (into {} (for [[surface lexemes-for-surface]
                  {"ba" [{:cat :v}]
                   "da" [{:cat :n}]
                   "ga" [{:cat :v}]
                   "ma" [{:cat :n}]}]
              [surface (map (fn [lexeme]
                              (merge lexeme {:phrasal false
                                             :surface surface}))
                        lexemes-for-surface)]))})

(defn morph-ps [structure]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure

        (seq? structure)
        (map morph-ps structure)
        
        (u/get-in structure [:surface])
        (morph-ps (u/get-in structure [:surface]))

        (= false (u/get-in structure [:phrasal] false))
        "nil"
        
        true
        (let [one (if (= (get structure :1)
                         (get structure :head))
                    "h:" "c:")
              two (if (= (get structure :1)
                         (get structure :head))
                    "c:" "h:")]
          (string/join ""
            (map morph-ps
                 ["[" (:rule structure) " "
                  one (u/get-in structure [:1] "nil") " "
                  two (u/get-in structure [:2] "nil")
                  "]"])))))

(defn morph [structure]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure

        (seq? structure)
        (map morph structure)
        
        (u/get-in structure [:surface])
        (morph (u/get-in structure [:surface]))

        (= false (u/get-in structure [:phrasal] false))
        "nil"
        
        true
        (string/join " "
                     (map morph
                          [(u/get-in structure [:1] "nil")
                           (u/get-in structure [:2] "nil")]))))

(defn generate [spec]
  (binding [g/grammar (shuffle (:grammar baby-language))
            g/lexicon (:lexicon baby-language)
            g/println? false
            g/morph-ps morph-ps]
    (g/generate spec)))

(defn demo []
  (do
    (println "five rule A expressions:")
    (count (take 5 (repeatedly #(println (morph (generate {:rule "A"}))))))
    (println "five rule B expressions:")
    (count (take 5 (repeatedly #(println (morph (generate {:rule "B"}))))))
    (println "five rule A expressions: (with structure)")
    (count (take 5 (repeatedly #(println (morph-ps (generate {:rule "A"}))))))
    (println "five rule B expressions: (with structure)")
    (count (take 5 (repeatedly #(println (morph-ps (generate {:rule "B"}))))))))
