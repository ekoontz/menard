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

(def spec-a {:rule "A"})
(def spec-b {:rule "B"
             :comp {:phrasal true}})

(def working-spec-a-expression
  (generate spec-a))

(def parents-with-head
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
           (map #(u/assoc-in! % [:babel.generate/started?] true))))))

(def parent-with-head (first parents-with-head))

(defn grow-1 [trees]
  (let [spec spec-b]
    (binding [g/grammar (shuffle (:grammar baby-language))
              g/lexicon (:lexicon baby-language)
              g/println? false
              g/morph-ps morph]
         (if (not (empty? trees))
           ;; for each tree,
           ;; find the next point of 
           ;; 1) branching to a new subtree, or
           ;; 2) terminating with a lexeme (leaf node).
           (let [tree (first trees)
                 frontier-path (g/frontier tree)
                 depth (count frontier-path)
                 child-spec (u/get-in tree frontier-path :top)
                 child-lexemes (g/get-lexemes child-spec)
                 child-trees (g/parent-with-head child-spec depth)]
             (merge
              {:tree tree
               :frontier-path frontier-path
               :depth depth
               :child-spec child-spec
               :child-lexemes child-lexemes
               :child-trees child-trees}
              (let [children
                    (cond
                      (> depth g/max-depth) []
                      
                      (= true (u/get-in child-spec [:phrasal]))
                      child-trees
                      
                      (= false (u/get-in child-spec [:phrasal]))
                      child-lexemes
                      
                      (g/branch? depth)
                      ;; generate children that are trees before children that are leaves.
                      (lazy-cat child-trees child-lexemes)
                      
                      true ;; generate children that are leaves before children that are trees.
                      (lazy-cat child-lexemes child-trees))]
                {:children children
                 :assoc (g/assoc-children tree children frontier-path)})))))))

(println (str "after adding a lexeme at:"
              (:frontier-path g1)
              ": " (morph (first (:assoc g1)))))

;;
;;
(def g1 (grow-1 parents-with-head))
(def parent (:tree g1))
(def children (:children g1))
(def child (first children))
(def frontier-path (:frontier-path g1))





