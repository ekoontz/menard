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
(def ^:const branching-factor 8)
(def ^:const max-depth 8)
(def ^:const branch? #(let [result (= 0 (rand-int (+ % branching-factor)))]
                        (log/debug (str "branch at: " % "? => " result))
                        result))

(def ^:dynamic morph-ps (fn [x] x))
(def ^:dynamic default-fn nil)
(def default-default-fn (fn [x]
                          (do
                            (println (str "DEFAULT-DEFAULT-FN."))
                            [x])))

(def ^:dynamic grammar nil)
(def ^:dynamic lexicon nil)
(def ^:dynamic index-fn (fn [spec]
                          (shuffle (flatten (vals lexicon)))))
(def ^:dynamic lexical-filter nil)
(def ^:dynamic println? nil)
(def ^:dynamic truncate? nil)

(declare frontier)
(declare get-lexemes)
(declare grow)
(declare grow-all)
(declare parent-with-head)
(declare parent-with-head-1)

(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  ([spec]
   (do (log/debug (str "generating with spec: " spec))
       (first (gen spec))))

  ([spec model]
   (log/debug (str "(generate) with model named: " (:name model)
                   "; truncate? " truncate?))
   (binding [default-fn (or default-fn (:default-fn model) default-default-fn)
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
     (first (grow-all (parent-with-head spec 0))))))

(defn grow-all [trees]
  (if (not (empty? trees))
    (lazy-cat (grow (first trees))
              (grow-all (rest trees)))))

(defn assoc-done-to [tree path]
  (if (= :comp (last path))
    (assoc-done-to (u/assoc-in! tree (concat path [::done?]) true)
                   (butlast path))
    (u/assoc-in! tree (concat path [::done?]) true)))

(defn truncate-up [tree path morph-ps]
  (cond
    (and (empty? path)
         (= true (u/get-in tree (concat path [::done?]))))
    (u/dissoc-paths
     (u/assoc-in! tree (concat path [:morph-ps]) (morph-ps (u/get-in tree path)))
     (map (fn [each]
            (concat path each))
          [[:head][:comp][:1][:2]]))
    
    (= true (u/get-in tree (concat path [::done?])))
    (let [result
          (u/dissoc-paths
           (u/assoc-in! tree (concat path [:morph-ps]) (morph-ps (u/get-in tree path)))
           (map (fn [each]
                  (concat path each))
                [[:head][:comp][:1][:2]]))]
      (truncate-up result (butlast path) morph-ps))

    true
    tree))

(defn grow
  "Recursively generate trees given input trees. continue recursively
   until no further expansion is possible."
  [tree]
  (let [frontier-path (frontier tree)
        depth (count frontier-path)]
    (cond (empty? frontier-path) [tree]
          (> depth max-depth) []
          true
          (let [child-spec (u/get-in tree frontier-path :top)
                child-lexemes (if (not (= true (u/get-in child-spec [:phrasal])))
                                (get-lexemes child-spec))
                child-trees (if (not (= false (u/get-in child-spec [:phrasal])))
                              (parent-with-head child-spec depth))
                ;; depending on depth, generate children that are leaves before or after children that are trees.
                children (or (and (branch? depth)
                                  (lazy-cat child-trees child-lexemes))
                             (lazy-cat child-lexemes child-trees))]
            (grow-all
             (map (fn [child]
                    (let [result (u/assoc-in tree frontier-path child)
                          result (cond (= true (u/get-in child [::done?]))
                                       (do
                                         (println (str " pre-truncate:" (morph-ps tree) ":" frontier-path ":" (count (str tree))))
                                         (truncate-up (assoc-done-to result frontier-path) frontier-path morph-ps))
                                       true result)]
                      (println (str "post-truncate:" (morph-ps tree) ":" frontier-path ":" (count (str result))))
                      result))
                  children))))))
(defn frontier
  "get the next path to which to adjoin within _tree_, or empty path [], if tree is complete."
  [tree]
  (let [retval
         (cond
           (= (u/get-in tree [::done?]) true)
           []

           (= (u/get-in tree [::started?] false) false)
           []
           
           (= (u/get-in tree [:phrasal]) false)
           []
    
           (and (= (u/get-in tree [:phrasal] true) true)
                (= true (u/get-in tree [::started?]))
                (not (u/get-in tree [:head ::done?])))
           (cons :head (frontier (u/get-in tree [:head])))

           (and (= (u/get-in tree [:phrasal] true) true)
                (= true (u/get-in tree [::started?])))
           (cons :comp (frontier (u/get-in tree [:comp])))

           (and (= (u/get-in tree [:phrasal] true) true))
           [:head]
    
           true
           (throw (Exception. (str "could not determine frontier for this tree: " tree))))]
    retval))

(defn parent-with-head
  "Return every possible tree from the given spec; if depth > 0, 
   tree is part of a larger subtree which we are appending at _depth_."
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
    (lazy-cat
      (let [parent-rule (first parent-rules)
            parent-cat (u/get-in parent-rule [:synsem :cat])
            phrases-with-phrasal-head (map (fn [child]
                                             (u/assoc-in parent-rule [:head] child))
                                           (filter (fn [grammar-rule]
                                                     (= parent-cat (u/get-in grammar-rule [:synsem :cat])))
                                                   grammar))
            phrases-with-lexical-heads (map (fn [child]
                                              (u/assoc-in parent-rule [:head] child))
                                            (get-lexemes (unify
                                                          (u/get-in spec [:head] :top)
                                                          (u/get-in parent-rule [:head] :top))))]
        (cond
          (branch? depth)
          (lazy-cat
            ;; phrases that could be the head child, then lexemes that could be the head child.
            phrases-with-phrasal-head
            phrases-with-lexical-heads)

          true
          (lazy-cat
           ;; lexemes that could be the head child, then phrases that could be the head child.
           phrases-with-lexical-heads
           phrases-with-phrasal-head)))
      (parent-with-head-1 spec depth (rest parent-rules)))))

(defn get-lexemes
  "Get lexemes matching the spec. Use index, where the index 
   is a function that we call with _spec_ to get a set of lexemes
   that matches the given _spec_."
  [spec]
  (if (= true (u/get-in spec [:phrasal]))
    []
    (->> (index-fn spec)
         (filter #(and (or (nil? lexical-filter) (lexical-filter %))
                       ;; TODO: probably remove this in favor of per-language filtering as we
                       ;; do in italiano.lab/lexical-filter, where we
                       ;; have: (binding [babel.generate/lexical-filter-fn]).
                       (or (= false (u/get-in % [:exception] false))
                           (not (= :verb (u/get-in % [:synsem :cat]))))))
         (map #(unify % spec))
         (filter #(not (= :fail %)))
         (map #(u/assoc-in! % [::done?] true)))))
