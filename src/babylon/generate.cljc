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

(def ^:const phrasal-children-first? #(let [result (= 0 (rand-int (+ % branching-factor)))]
                                         (log/debug (str "branch at: " % "? => " result))
                                         result))

;; you can experiment by modifying branching-factor and then run branching-samples
;; to see how many times out of 100 you'd branching trees before leaves.
(defn branching-samples []
  (count (filter #(= % true) (take 100 (repeatedly #(fn [] (phrasal-children-first?)))))))

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
(def ^:dynamic shuffle? true)
(def ^:dynamic truncate? nil)

(declare frontier)
(declare get-lexemes)
(declare get-lexemes-fast)
(declare grow)
(declare grow-all)
(declare parent-with-head)
(declare parent-with-head-1)
(declare shuffle-or-not)

(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  ([spec]
   (do (log/debug (str "generating with spec: " spec))
       (first (grow-all (parent-with-head spec 0))))))

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

(defn grow-all [trees]
  (if (not (empty? trees))
    (lazy-cat (grow (first trees))
              (grow-all (rest trees)))))

(defn grow
  "Recursively generate trees given input trees. continue recursively
   until no further expansion is possible."
  [tree]
  (let [frontier-path (frontier tree)
        depth (count frontier-path)]
    (log/debug (str "grow: " (morph-ps tree) " at: " (vec frontier-path)))
    (cond
      (empty? frontier-path) [tree]
      true
      (->>
       (let [child-spec (u/get-in tree frontier-path :top)
             child-lexemes (if (not (= true (u/get-in child-spec [:phrasal])))
                             (get-lexemes-fast child-spec))
             child-trees (if (not (= false (u/get-in child-spec [:phrasal])))
                           (parent-with-head child-spec depth))]
         (cond
           (>= depth max-depth) child-lexemes ;; max-depth has been reached: return only lexemes.
           (phrasal-children-first? depth)
           (lazy-cat child-trees child-lexemes) ;; order children which are trees before children which are leaves.
           true
           (lazy-cat child-lexemes child-trees))) ;; order children which are leaves before children which are trees.
       
       (map (fn [child]
               (-> tree
                   (u/copy)
                   (u/assoc-in! frontier-path child)
                   (terminate-up frontier-path)
                   (trunc/truncate-up frontier-path morph-ps truncate?))))
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

(def allowed-head-children
  {"s" #{"vp"}
   "np" #{"nbar"}})

(defn parent-with-head
  "Return every possible tree from the given spec; if depth > 0, 
   tree is part of a larger subtree which we are appending at _depth_."
  [spec depth]
  ;; get all rules that match input _spec_:
  (->> grammar
       (map #(unify % spec))
       (mapcat (fn [parent-rule]
                 (let [phrasal-children
                       (->> grammar
                            (filter #(contains? (get allowed-head-children
                                                     (:rule parent-rule))
                                                (:rule %)))
                            shuffle-or-not)
                       lexical-children
                       (->> (get-lexemes-fast
                              (unify
                                (u/get-in spec [:head] :top)
                                (u/get-in parent-rule [:head] :top))))]
                   (->> (cond (phrasal-children-first? depth)
                              (lazy-cat phrasal-children lexical-children)
                              true
                              (lazy-cat lexical-children phrasal-children))
                        (map #(u/assoc-in parent-rule [:head] %))
                        (filter #(not (= :fail %)))))))
       (map #(u/assoc-in! % [::started?] true))))

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
         (filter #(not (= :fail %)))
         shuffle-or-not
         (map #(u/assoc-in! % [::done?] true)))))

(defn get-lexemes-fast [spec]
  (take 100000000 (get-lexemes spec)))

(defn shuffle-or-not [x]
  (if shuffle? (shuffle x) x))

