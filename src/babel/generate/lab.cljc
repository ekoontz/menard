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
        
        true
        (let [one (if (= (get structure :1)
                         (get structure :head))
                    "h:" "")
              two (if (= (get structure :1)
                         (get structure :head))
                    "" "h")]
          (string/join ""
            (map morph
                 ["[" (:rule structure) " "
                  one (u/get-in structure [:1]) " "
                  two (u/get-in structure [:2])
                  "]"])))))

(defn generate [spec]
  (binding [g/grammar (shuffle (:grammar baby-language))
            g/lexicon (:lexicon baby-language)
            g/println? true
            g/morph-ps morph]
    (g/generate spec)))

(def working-spec-a-expression
  (generate spec-a))

(def parent-with-head
  (let [spec spec-b]
    (binding [g/grammar (shuffle (:grammar baby-language))
              g/lexicon (:lexicon baby-language)
              g/println? true
              g/morph-ps morph]
      (->> grammar
           (map #(unify % spec))
           (remove #(= :fail %))
           (g/parent-with-head-1 spec depth)
           (remove #(= % :fail))
           (map #(u/assoc-in! % [::started?] true))))))

(defn grow-1 [trees]
  (let [spec spec-b]
    (binding [g/grammar (shuffle (:grammar baby-language))
              g/lexicon (:lexicon baby-language)
              g/println? true
              g/morph-ps morph]
         (if (not (empty? trees))
           ;; for each tree,
           ;; find the next point of 
           ;; 1) branching to a new subtree, or
           ;; 2) terminating with a lexeme (leaf node).
           (let [tree (first trees)
                 frontier-path (g/frontier tree)
                 debug (if (not (empty? frontier-path))
                         (println (str "tree: " (morph-ps tree) " has frontier: " frontier-path " with value at frontier: " (u/get-in tree frontier-path))))
                 depth (count frontier-path)
                 child-spec (u/get-in tree frontier-path :top)
                 child-lexemes #(g/get-lexemes child-spec)
                 child-trees #(g/parent-with-head child-spec depth)]
             {:tree tree
              :depth depth
              :child-spec child-spec
              :child-lexemes child-lexemes
              :child-trees child-trees})))))
