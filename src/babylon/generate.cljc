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

(defn phrasal-children-first? [depth]
  (let [result (= 0 (rand-int (+ depth branching-factor)))]
    (log/debug (str "branch at: " depth "? => " result))
    result))

;; you can experiment by modifying branching-factor and then run branching-samples
;; to see how many times out of 100 you'd branching trees before leaves.
(defn branching-samples []
  (count (filter #(= % true) (take 100 (repeatedly #(fn [] (phrasal-children-first?)))))))

(def ^:const max-depth 5)
(def ^:dynamic morph-ps (fn [x]
                          (cond (map? x)
                                (vec (u/serialize x))
                                (or (nil? x)
                                    (keyword? x)) x
                                true (str x "(type:" (type x) ")"))))
(def ^:dynamic grammar [])
(def ^:dynamic lexicon nil)
(def ^:dynamic index-fn (fn [spec]
                          (flatten (vals lexicon))))

(def ^:dynamic lexical-filter nil)
(def ^:dynamic shuffle? true)
(def ^:dynamic truncate? nil)

(declare frontier)
(declare get-lexemes)
(declare grow)
(declare match-against-rules)
(declare shuffle-or-not)
(declare terminate-up)
(declare generate-all)

(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  [spec grammar]
  (first (generate-all spec grammar)))

(defn generate-all
  "Return the subset of _grammar_ that unfies with spec _spec_, and return the unified result for each member of that subset."
  [spec grammar]
  (->> (match-against-rules spec grammar)
       (mapcat (fn [tree]
                 (grow tree grammar)))))

(defn match-against-rules [spec grammar]
  (->> grammar
       shuffle-or-not
       (map (fn [grammar-rule]
              (unify grammar-rule spec)))
       (remove #(= % :fail))
       (map #(u/assoc-in! % [::started?] true))))

(defn lazy-mapcat [f seqs]
  (if (not (empty? seqs))
     (lazy-cat
       (f (first seqs))
       (lazy-mapcat f (rest seqs)))
     []))

(defn grow
  "Recursively generate trees given input trees. continue recursively
   until no further expansion is possible."
  [tree grammar]
  (let [frontier-path (frontier tree)
        depth (count frontier-path)]
    (log/debug (str "grow: " (morph-ps tree) " at: " (vec frontier-path)))
    (cond
      (empty? frontier-path) [tree]
      true
      (let [child-spec (u/get-in tree frontier-path :top)
            child-lexemes (if (not (= true (u/get-in child-spec [:phrasal])))
                            (get-lexemes child-spec))
            child-trees (if (not (= false (u/get-in child-spec [:phrasal])))
                          (match-against-rules child-spec grammar))]
        (->>
         (cond
           (>= depth max-depth) child-lexemes ;; max-depth has been reached: return only lexemes.
           (phrasal-children-first? depth)
           (lazy-cat child-trees child-lexemes) ;; order children which are trees before children which are leaves.
           true
           (lazy-cat child-lexemes child-trees)) ;; order children which are leaves before children which are trees.
         (take 100000000000000) ;; this improves performance but I don't know why.
         (map (fn [child]
                (-> tree
                    (u/copy)
                    (u/assoc-in! frontier-path child)
                    (terminate-up frontier-path))))
         (lazy-mapcat (fn [tree]
                        (log/debug (str "size of tree: " (morph-ps tree) " : " (count (str tree))))
                        (grow tree grammar)))
         lazy-seq)))))

(defn get-lexemes
  "Get lexemes matching the spec. Use index, where the index 
   is a function that we call with _spec_ to get a set of lexemes
   that matches the given _spec_."
  [spec]
  (if (= true (u/get-in spec [:phrasal]))
    (lazy-seq [])
    (->> (index-fn spec)
         lazy-seq
         (filter #(and (or (nil? lexical-filter) (lexical-filter %))
                       (or (= false (u/get-in % [:exception] false))
                           (not (= :verb (u/get-in % [:synsem :cat]))))))
         (map #(unify % spec))
         (filter #(not (= :fail %)))
         shuffle-or-not
         (map #(u/assoc-in! % [::done?] true)))))

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

(defn shuffle-or-not [x]
  (if shuffle? (shuffle x) x))
