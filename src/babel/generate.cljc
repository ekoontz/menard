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

(def ^:dynamic default-fn nil)
(def ^:dynamic grammar nil)
(def ^:dynamic index-fn nil)
(def ^:dynamic lexicon nil)
(def ^:dynamic lexical-filter nil)
(def ^:dynamic morph-ps nil)
(def ^:dynamic println? nil)
(def ^:dynamic truncate? nil)

(declare assoc-children)
(declare frontier)
(declare gen)
(declare get-lexemes)
(declare grow)
(declare parent-with-head)
(declare parent-with-head-1)

(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  ([spec]
   (do (log/info (str "generating with spec: " spec))
       (first (gen spec))))

  ([spec model]
   (log/debug (str "(generate) with model named: " (:name model)
                   "; truncate? " truncate?))
   (binding [default-fn (or default-fn (:default-fn model) (fn [x] [x]))
             grammar (or grammar (:grammar model))
             lexicon (or lexicon
                         (:lexicon (:generate model))
                         (:lexicon model))
             index-fn (or index-fn
                          (:index-fn model)
                          (do
                            (log/warn (str "no index available for this model: using entire lexicon."))
                            (fn [spec]
                              (flatten (vals)
                                       lexicon))))
             morph-ps (or morph-ps (:morph-ps model))]
     (first (gen spec)))))

(defn gen
  "generate a potentially infinite (depending on given _spec_ and the model)."
  [spec]
  (grow (parent-with-head spec 0)))

(defn grow
  "Recursively generate trees given input trees. continue recursively
   until no futher expansion is possible."
  [trees]
  (if (not (empty? trees))
    ;; for each tree,
    ;; find the next point of 
    ;; 1) branching to a new subtree, or
    ;; 2) terminating with a lexeme (leaf node).
    (let [tree (first trees)
          frontier-path (frontier tree)
          depth (count frontier-path)
          child-spec (u/get-in tree frontier-path)
          child-lexemes #(get-lexemes child-spec)
          child-trees #(parent-with-head child-spec depth)]
      (when println? (println (str "grow at:" (morph-ps tree)
                                   "; frontier: " frontier-path
                                   "; looking for spec with "
                                   "cat=" (u/get-in child-spec [:synsem :cat])
                                   " and infl=" (u/get-in child-spec [:synsem :infl]))))
      (log/debug (str "grow at:" (morph-ps tree)))
      (lazy-cat
       (if (not (empty? frontier-path))
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
            (assoc-children tree children frontier-path)))
         [tree])
       (grow (rest trees))))))

(defn frontier
  "get the next path to which to adjoin within _tree_, or empty path [], if tree is complete."
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

(defn parent-with-head
  "Return every possible tree of depth 1 from the given spec."
  [spec depth]
  ;; get all rules that match input _spec_:
  (->> grammar
       (map #(unify % spec))
       (remove #(= :fail %))
       (parent-with-head-1 spec depth)
       (remove #(= % :fail))
       (map #(u/assoc-in! % [::started?] true))))

(defn parent-with-head-1 [spec depth parent-rules]
  (if (not (empty? parent-rules))
    (let [parent-rule (first parent-rules)
          parent-cat (u/get-in parent-rule [:synsem :cat])
          phrases-with-phrasal-head #(map (fn [child]
                                            (u/assoc-in parent-rule [:head] child))
                                          (filter (fn [grammar-rule]
                                                    (= parent-cat (u/get-in grammar-rule [:synsem :cat])))
                                                  grammar))
          phrases-with-lexical-heads #(map (fn [child]
                                             (u/assoc-in parent-rule [:head] child))
                                           (get-lexemes (unify
                                                         (u/get-in spec [:head] :top)
                                                         (u/get-in parent-rule [:head] :top))))]
      (cond
        (branch? depth)
        (lazy-cat
         ;; phrases that could be the head child, then lexemes that could be the head child.
         (phrases-with-phrasal-head)
         (phrases-with-lexical-heads)
         (parent-with-head-1 spec depth (rest parent-rules)))

        true
        (lazy-cat
         ;; lexemes that could be the head child, then phrases that could be the head child.
         (phrases-with-lexical-heads)
         (phrases-with-phrasal-head)
         (parent-with-head-1 spec depth (rest parent-rules)))))))

(defn- get-lexemes 
  "Get lexemes matching the spec. Use index, where the index 
   is a function that we call with _spec_ to get a set of lexemes
   that matches the given _spec_."
  [spec]
  (if (= true (u/get-in spec [:phrasal]))
    []
    (->> (index-fn spec)
         (filter #(and (or (nil? lexical-filter) (lexical-filter %))
                       ;; TODO: probably remove this in favor of per-language filtering as we
                       ;; do in italiano.lab/lexical-filter, where we (binding [babel.generate/lexical-filter-fn]).
                       (or (= false (u/get-in % [:exception] false))
                           (not (= :verb (u/get-in % [:synsem :cat]))))))
         (map #(unify % spec))
         (filter #(not (= :fail %)))
         (map #(u/assoc-in! % [::done?] true)))))

(defn- assoc-each-default [tree children path]
  (if (not (empty? children))
    (lazy-cat
     (let [child (first children)
           tree-with-child (u/assoc-in tree path child)]
       (-> tree-with-child
           (u/assoc-in! 
            (concat (butlast path) [::done?])
            true)
           (u/dissoc-paths (if truncate? [path] []))
           (default-fn)))
     (assoc-each-default tree (rest children) path))))

(defn- assoc-children [tree children path]
  (if (not (empty? children))
    (let [child (first children)]
      (lazy-cat
       (if (= true (u/get-in child [::done?]))
         (assoc-each-default tree (default-fn child) path)
         [(u/assoc-in tree path child)])
       (assoc-children tree (rest children) path)))))

