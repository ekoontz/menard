(ns babel.generate
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.math.combinatorics :as combo]
   [clojure.string :as string]
   [dag_unify.core :as u :refer [strip-refs unify]]))

;; the higher the constant below,
;; the more likely we'll first generate leaves
;; (terminal nodes) rather than trees.
(def ^:const branching-factor 5)
(def ^:const max-depth 5)
(def ^:const branch? #(let [result (= 0 (rand-int (+ % branching-factor)))]
                        (log/debug (str "branch at: " % "? => " result))
                        result))
(def ^:dynamic truncate? false)
(def ^:dynamic println? false)
(def ^:dynamic model)

(declare gen)
(declare get-lexemes)
(declare grow)
(declare parent-with-head)
(declare parent-with-head-1)

(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  ([spec model]
   (log/debug (str "(generate) with model named: " (:name model)
                   "; truncate? " truncate?))
   (binding [model model]
     (first (gen spec)))))

(defn gen
  "generate a potentially infinite (depending on given _spec_ and _model_) 
   list of expressions that match the given _spec_."
  [spec]
  (grow (parent-with-head spec 0)))

(defn matching-rules [spec]
  (->> (:grammar model)
       (map #(unify % spec))
       (filter #(not (= :fail %)))))
      
(defn parent-with-head
  "Return every possible tree of depth 1 from the given spec and model."
  [spec depth]
  ;; get all rules that match input _spec_:
  (if (nil? spec) (throw (Exception. (str "nope: spec was nil."))))
  (let [matching-rules
        (shuffle (matching-rules spec))]
    (->> matching-rules
         ;; 2. try to add heads to each matching rule:
         (parent-with-head-1 spec depth)
         (filter #(not (= % :fail)))
         (map #(u/assoc-in! % [::started?] true)))))

(defn parent-with-head-1 [spec depth parent-rules]
  (if (not (empty? parent-rules))
    (let [parent-rule (first parent-rules)
          phrases-with-phrasal-head #(map (fn [child]
                                            (u/assoc-in parent-rule [:head] child))
                                          (:grammar model))
          phrases-with-lexical-heads #(map (fn [child]
                                             (u/assoc-in parent-rule [:head] child))
                                           (get-lexemes (unify
                                                         (u/get-in spec [:head] :top)
                                                         (u/get-in parent-rule [:head] :top))))]
      (log/debug (str "pwh-1:" (:rule parent-rule)))
      (cond
        (branch? depth)
        (lazy-cat
         ;; get all the things to be added
         ;; as the head child of parent-rule:
         ;; 1. phrases that could be the head child:
         (phrases-with-phrasal-head)
         ;; 2. lexemes that could be the head child:
         (phrases-with-lexical-heads)
         (parent-with-head-1 spec depth (rest parent-rules)))

        true
        (lazy-cat
         ;; 1. lexemes that could be the head child:
         (phrases-with-lexical-heads)
         ;; 2. phrases that could be the head child:
         (phrases-with-phrasal-head)
         (parent-with-head-1 spec depth (rest parent-rules)))))))

(defn get-lexemes [spec]
  "Get lexemes matching the spec. Use a model's index if available, where the index 
   is a function that we call with _spec_ to get a set of indices. 
   Otherwise use the model's entire lexeme."
  (->>
   (if-let [index-fn (:index-fn model)]
     (index-fn spec)
     (do
       (log/warn (str "get-lexemes: no index found: using entire lexicon."))
       (flatten (vals
                 (or (:lexicon (:generate model)) (:lexicon model))))))
   (filter #(or (= false (u/get-in % [:exception] false))
                (not (= :verb (u/get-in % [:synsem :cat])))))
   (map #(unify % spec))
   (filter #(not (= :fail %)))
   (map #(u/assoc-in! % [::done?] true))))

(defn frontier
  "get the next path to which to adjoin within _tree_."
  [tree]
  (cond

    (= (u/get-in tree [::done?]) true)
    []
    
    (and (= (u/get-in tree [:phrasal]) true)
         (not (u/get-in tree [::done?]))
         (= true (u/get-in tree [::started?]))
         (not (u/get-in tree [:head ::done?])))
    (cons :head (frontier (u/get-in tree [:head])))

    (and (= (u/get-in tree [:phrasal]) true)
         (= true (u/get-in tree [::started?])))
    (cons :comp (frontier (u/get-in tree [:comp])))
    
    true []))

(defn assoc-each-default [tree children f]
  (if (not (empty? children))
    (lazy-cat
     (let [child (first children)
           tree-with-child (u/assoc-in tree f child)]
       (-> tree-with-child
           (u/assoc-in! 
            (concat (butlast f) [::done?])
            true)
           (u/dissoc-paths (if truncate? [f] []))
           ((or (:default-fn model) (fn [x] [x])))))
     (assoc-each-default tree (rest children) f))))

(defn assoc-children [tree children f]
  (if (not (empty? children))
    (let [child (first children)
          default-fn (or (:default-fn model)
                         (fn [x] [x]))]
      (lazy-cat
       (if (= true (u/get-in child [::done?]))
         (assoc-each-default tree (default-fn child) f)
         [(u/assoc-in tree f child)])
       (assoc-children tree (rest children) f)))))

(defn grow
  "recursively generate trees given 
   input trees and model. continue recursively
   until no futher expansion is 
   possible."
  [trees]
  (if (not (empty? trees))
    ;; for each tree,
    ;; find the next point of 
    ;; 1) branching to a new subtree, or
    ;; 2) terminating with a lexeme (leaf node).
    (let [tree (first trees)
          f (frontier tree)
          depth (count f)
          child-spec (u/get-in tree f)
          child-lexemes #(get-lexemes child-spec)
          child-trees #(parent-with-head child-spec depth)]
      (when println? (println (str "grow at:" ((:morph-ps model) tree))))
      (log/debug (str "grow at:" ((:morph-ps model) tree)))
      (lazy-cat
       (if (not (empty? f))
         (grow
          (let [children
                (cond
                  (> depth max-depth) []
                  
                  (= true (u/get-in child-spec [:phrasal]))
                  (child-trees)
                  
                  (= false (u/get-in child-spec [:phrasal]))
                  (child-lexemes)
                  
                  (branch? depth)
                  ;; generate children that are trees before children that are leaves.
                  (lazy-cat (child-trees) (child-lexemes))
                  
                  true ;; generate children that are leaves before children that are trees.
                  (lazy-cat (child-lexemes) (child-trees)))]
            (assoc-children tree children f)))
         [tree])
       (grow (rest trees))))))

