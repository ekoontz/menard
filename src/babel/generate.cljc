(ns babel.generate
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.math.combinatorics :as combo]
   [clojure.string :as string]
   [dag_unify.core :as u :refer [strip-refs unify]]
   [dag_unify.dissoc :as d]))

;; the higher the constant below,
;; the more likely we'll first generate leaves
;; (terminal nodes) before trees.
;; set to 0 to always put trees before leaves.
;; set to a large X (e.g. 10 to (virtually) always put leaves before trees.
(def ^:const branching-factor 1000)

(def ^:const branch? #(let [result (= 0 (rand-int (+ % branching-factor)))]
                        (log/info (str "branch at: " % "? => " result))
                        result))

;; you can experiment by modifying branching-factor and then run branching-samples
;; to see how many times out of 100 you'd branching trees before leaves.
(defn branching-samples []
  (count (filter #(= % true) (take 100 (repeatedly #(fn [] (branch?)))))))

(def ^:const max-depth 5)
;; no truncation: 
;; max-depth | time (secs)
;; ----------+----------
;;         3 |   0.16
;;         4 |   0.5
;;         5 |   2
;;         6 |   5
;;         7 |  13
;;         8 |  38
;;         9 | 160
;;        10 | 350

(def ^:dynamic morph-ps (fn [x] x))
(def ^:dynamic default-fn nil)
(def default-default-fn (fn [tree frontier-path]
                          (do
                            (println (str "DEFAULT-DEFAULT-FN."))
                            [tree])))

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
       (first (grow-all (parent-with-head spec 0)))))

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

(defn truncate-at [tree path morph-ps]
  (let [reentrances (map first (u/serialize tree))
        aliases
        (filter #(or (= (first %) :comp)
                     (= (first %) :head)
                     (= (first %) :1)
                     (= (first %) :2))
                (set (d/aliases-of path reentrances)))]
    (binding [d/remove-path?
              (fn [path]
                (some #(d/prefix? path %) aliases))]
      (log/info (str "truncating:" (morph-ps tree) " at path: " (vec path) " size=" (count (str tree))))
      (let [truncated
            (->
             tree
             (d/dissoc-in path)
             (u/assoc-in [:morph-ps] (morph-ps tree)))]
        (log/info (str "     to:   " (morph-ps truncated) "; size=" (count (str truncated))))
        truncated))))

(defn grow-all [trees]
  (if (not (empty? trees))
    (lazy-cat
     (grow (first trees))
     (grow-all (rest trees)))))

(defn pre-truncate-fn [tree frontier-path]
 (if (and (= :comp (last frontier-path))
          (u/get-in tree (concat frontier-path [::done?])))
     (u/assoc-in tree (concat (butlast frontier-path) [:morph-ps])
                      (morph-ps (u/get-in tree (butlast frontier-path))))
     tree))

(declare truncate-up)

(defn grow
  "Recursively generate trees given input trees. continue recursively
   until no further expansion is possible."
  [tree]
  (let [frontier-path (frontier tree)
        depth (count frontier-path)]
    (if true (log/info (str "grow:      " (morph-ps tree) " at: " (vec frontier-path) " size=" (count (str tree)))))
    (cond (empty? frontier-path)
          [tree]

          true
          (grow-all

           (->>
            ;; 1. get all the possible children at _frontier-path_:
            (let [child-spec (u/get-in tree frontier-path :top)
                  child-lexemes (if (not (= true (u/get-in child-spec [:phrasal])))
                                  (get-lexemes child-spec))
                  child-trees (if (not (= false (u/get-in child-spec [:phrasal])))
                                (parent-with-head child-spec depth))]
                ;; depending on depth, generate children that are leaves before or after children that are trees.
                (cond
                  (= depth max-depth) child-lexemes
                  (branch? depth)
                  (lazy-cat child-trees child-lexemes)
                  true
                  (lazy-cat child-lexemes child-trees)))
            (map (fn [child]
                   ;; 2. for each such child:
                   (-> tree
                       ;; attach _child_ to _tree_ at _frontier-path_:
                       (u/assoc-in frontier-path child)

                       ;; terminate if possible:
                       ((fn [tree]
                          (let [terminated-or-not
                                (if (and (= :comp (last frontier-path))
                                         (u/get-in tree (concat frontier-path [::done?])))
                                  (u/assoc-in tree (concat (butlast frontier-path) [::done?]) true)
                                  tree)]
                            (if false (println (str "post-termination at:" (vec frontier-path) ": ")
                                               (vec (u/serialize terminated-or-not))))
                            terminated-or-not)))

                       ;; TODO: move to lab/pre-truncate.
                       ;;(default-fn frontier-path)
                       (pre-truncate-fn frontier-path)

                       ;; truncate if desired:
                       ((fn [tree]
                          (if truncate?
                            (truncate-up tree frontier-path morph-ps)
                            tree)))

                       ;; diagnostics if desired
                       ((fn [tree]
                          (if truncate?
                            (do (log/info (str "returning: " (morph-ps tree)))
                                tree)
                            tree)))))))))))

(defn truncate-up [tree frontier-path morph-ps]
  (log/info (str "truncat-up:" (morph-ps tree) " at: " (vec frontier-path)))


  (log/info (str "trunct-up1:" (vec (concat (butlast frontier-path) [:phrasal]))))
  
  (cond (empty? frontier-path)
        tree

        (u/get-in tree (concat (butlast frontier-path) [:phrasal]))
        (->
         tree
;;         (u/assoc-in (concat frontier-path [:morph-ps]) (morph-ps tree))
;;         (truncate-at frontier-path morph-ps)
         (truncate-up (butlast frontier-path) morph-ps))

        (and (u/get-in tree (concat frontier-path [:phrasal]))
             (u/get-in tree (concat frontier-path [::done?])))
        (truncate-at tree frontier-path morph-ps)

        true
        (do
          (log/info (str "not truncating any more with path:" (vec frontier-path)))
          tree)))

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
           
           (= ::none (u/get-in tree [:comp] ::none))
           []
           
           (= ::none (u/get-in tree [:head] ::none))
           (cons :comp (frontier (u/get-in tree [:comp])))
    
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
