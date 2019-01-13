(ns babylon.generate
  (:require
   [babylon.generate.truncate :as trunc]
   [clojure.tools.logging :as log]
   [dag_unify.core :as u :refer [unify]]))

;; the higher the constant below,
;; the more likely we'll first generate leaves
;; (terminal nodes) before trees.
;; set to 0 to always put trees before leaves.
;; set to a large X (e.g. 10 to (virtually) always put leaves before trees.
(def ^:const branching-factor 1000)

(def ^:const branch? #(let [result (= 0 (rand-int (+ % branching-factor)))]
                        (log/debug (str "branch at: " % "? => " result))
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

(def ^:dynamic morph-ps (fn [x]
                          (cond (map? x)
                                (vec (u/serialize x))
                                (or (nil? x)
                                    (keyword? x)) x
                                true (str x "(type:" (type x) ")"))))
(def ^:dynamic grammar nil)
(def ^:dynamic lexicon nil)
(def ^:dynamic index-fn (fn [spec]
                          (flatten (vals lexicon))))
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
       (first (grow-all (parent-with-head spec 0))))))

(defn grow-all [trees]
  (if (not (empty? trees))
    (lazy-cat (grow (first trees))
              (grow-all (rest trees)))))

(defn terminate-up [tree frontier-path]
  (log/debug (str "terminate-up: " (vec frontier-path)))
  (cond
    (and (= :comp (last frontier-path))
         (u/get-in tree (concat frontier-path [::done?])))
    (do
      (log/debug (str "terminating at:" (vec (concat (butlast frontier-path) [::done?]))))
      (-> tree
          (u/assoc-in! (concat (butlast frontier-path) [::done?]) true)
          (terminate-up (butlast frontier-path))))
    true
    (do
      (log/debug (str "done terminating: at:" (vec frontier-path)))
      (log/debug (str "done terminating: with done?:" (u/get-in tree [::done?] ::none)))
      tree)))

(defn grow
  "Recursively generate trees given input trees. continue recursively
   until no further expansion is possible."
  [tree]
  (let [frontier-path (frontier tree)
        depth (count frontier-path)]
    (log/debug (str "grow:      " (morph-ps tree) " at: " (vec frontier-path)))
    (cond
      (empty? frontier-path) [tree]
      true
      (->>
       ;; 1. get all the possible children at _frontier-path_:
       (let [child-spec (u/get-in tree frontier-path :top)
             child-lexemes (if (not (= true (u/get-in child-spec [:phrasal])))
                             (get-lexemes child-spec))
             child-trees (if (not (= false (u/get-in child-spec [:phrasal])))
                           (parent-with-head child-spec depth))]
         ;; depending on depth, generate children that are leaves before or after children that are trees.
         (cond
           (>= depth max-depth) child-lexemes ;; max-depth has been reached: return only lexemes.
           (branch? depth)
           (lazy-cat child-trees child-lexemes) ;; order children which are trees before children which are leaves.
           true
           (lazy-cat child-lexemes child-trees))) ;; order children which are leaves before children which are trees.
       
       (map (fn [child]
              ;; 2. for each such child:
              (-> tree
                  
                  (u/copy)
                  
                  ;; attach _child_ to _tree_ at _frontier-path_:
                  (u/assoc-in! frontier-path child)
                  
                  ;; terminate if possible:
                  ((fn [tree]
                     (terminate-up tree frontier-path)))
                  
                  ;; truncate if desired:
                  ((fn [tree]
                     (if truncate?
                       (trunc/truncate-up tree frontier-path morph-ps)
                       tree))))))
       (grow-all)))))

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
  (log/debug (str "parent-with-head: checking " (count grammar) " rules."))
  (let [result
        (->> grammar
             (map #(do
                     (if (= :fail %)
                       (throw (Exception. (str "grammar rule is unexpectedly :fail: please check your grammar."))))
                     (unify % spec)))
             (remove #(= :fail %))
             (parent-with-head-1 spec depth)
             (map #(u/assoc-in! % [::started?] true)))]
    (log/debug (str "parent-with-head: found " (count result) " matches: " (vec (map :rule result))))
    result))

(defn parent-with-head-1 [spec depth parent-rules]
  (log/debug (str "parent-with-head: grammar count: " (count grammar)))
  (log/debug (str "parent-with-head: parent-rules count: " (count parent-rules)))
  (if (not (empty? parent-rules))
    (lazy-cat
     (do
       (if (= 0 (count grammar))
           (throw (Exception. (str ": grammar is empty: " (type grammar)))))
       (let [debug (log/debug (str "grammar is: " (count grammar)))
             parent-rule (first parent-rules)
             debug (log/debug (str "looking at parent-rule: " (:rule parent-rule) " with rules:" (count grammar)))
             phrases-with-phrasal-head (->> grammar
                                            shuffle
                                            (map #(u/assoc-in parent-rule [:head] %))
                                            (filter #(not (= :fail %))))
             phrases-with-lexical-head (->> (get-lexemes (unify
                                                          (u/get-in spec [:head] :top)
                                                          (u/get-in parent-rule [:head] :top)))
                                            shuffle
                                            (map #(u/assoc-in parent-rule [:head] %))
                                            (filter #(not (= :fail %))))]
           (log/debug (str "parent-with-head-1 (phrases): spec=" spec "; depth=" depth "; parent-rule=" (:rule parent-rule) ":" (count phrases-with-phrasal-head)))
           (cond
              (branch? depth)
              (lazy-cat
                ;; phrases that could be the head child, then lexemes that could be the head child.
                phrases-with-phrasal-head
                phrases-with-lexical-head)

              true
              (lazy-cat
                ;; lexemes that could be the head child, then phrases that could be the head child.
                phrases-with-lexical-head
                phrases-with-phrasal-head))))
     (parent-with-head-1 spec depth (rest parent-rules)))))

(def ^:dynamic lexical-default-rules
  [:top [:top]])

(defn lexical-defaults
  "used to create inflected forms for lexemes when generating."
  [lexeme]
  (mapcat (fn [rule]
            (let [[antecedent consequents] rule]
              (cond (not (= :fail (unify antecedent lexeme)))
                    (->> consequents
                         (map (fn [consequent]
                                (unify consequent lexeme)))
                         (filter #(not (= :fail %))))
                    true
                    [lexeme])))
          lexical-default-rules))

(defn get-lexemes
  "Get lexemes matching the spec. Use index, where the index 
   is a function that we call with _spec_ to get a set of lexemes
   that matches the given _spec_."
  [spec]
  (if (= true (u/get-in spec [:phrasal]))
    []
    (->> (index-fn spec)
         (filter #(and (or (nil? lexical-filter) (lexical-filter %))
                       (or (= false (u/get-in % [:exception] false))
                           (not (= :verb (u/get-in % [:synsem :cat]))))))
         (map #(unify % spec))
         (mapcat (fn [lexeme]
                   (lexical-defaults lexeme)))
         (filter #(not (= :fail %)))
         (map #(u/assoc-in! % [::done?] true))
         shuffle)))
