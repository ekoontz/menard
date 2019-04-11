(ns babylon.generate
  (:require
   [babylon.generate.truncate :as trunc]
   [clojure.tools.logging :as log]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.serialization :as s]))

;; the higher the constant below,
;; the more likely we'll first generate leaves
;; (terminal nodes) before trees.
;; set to 0 to always put trees before leaves.
;; set to a large X (e.g. 10 to (virtually) always put leaves before trees.
(def ^:const branching-factor 1000)

(def ^:dynamic morph (fn [x] "-no-morph-function-"))
(def ^:dynamic syntax-tree (fn [x] "[-no-syntax-tree-function-]"))
(def ^:dynamic truncate? true)

(defn phrasal-children-first? [depth]
  (let [result (= 0 (rand-int (+ depth branching-factor)))]
    (log/debug (str "branch at: " depth "? => " result))
    result))

;; you can experiment by modifying branching-factor and then run branching-samples
;; to see how many times out of 100 you'd branching trees before leaves.
(defn branching-samples []
  (count (filter #(= % true) (take 100 (repeatedly #(fn [] (phrasal-children-first?)))))))

(def ^:const max-depth 6)

(def ^:dynamic lexicon nil)
(def ^:dynamic index-fn (fn [spec]
                          (flatten (vals lexicon))))

(def ^:dynamic lexical-filter nil)
(def ^:dynamic shuffle? true)

(declare frontier)
(declare get-lexemes)
(declare grow)
(declare match-against-rules)
(declare generate-all)
(declare lazy-mapcat)
(declare shuffle-or-not)
(declare terminate-up)
(declare truncate-in)

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

(defn grow
  "Recursively generate trees given input trees. continue recursively
   until no further expansion is possible."
  [^clojure.lang.PersistentArrayMap tree grammar]
  (let [frontier-path (frontier tree)
        depth (count frontier-path)]
    (log/debug (str "grow: " (syntax-tree tree) " at: " (vec frontier-path) "; #:" (count (str tree))))
    (cond
      (empty? frontier-path) [tree]
      true
      (let [child-spec (u/get-in tree frontier-path :top)
            child-lexemes (if (not (= true (u/get-in child-spec [:phrasal])))
                            (get-lexemes child-spec))
            child-trees (if (not (= false (u/get-in child-spec [:phrasal])))
                          (match-against-rules child-spec grammar))]
        (log/debug (str "lexical children: " (count child-lexemes)))
        (log/debug (str "phrasal children: " (count child-trees)))
        (log/debug (str "total children: " (count (concat child-trees child-lexemes))))
        (if (and (empty? child-lexemes) (empty? child-trees))
          (throw (ex-info
                  (str "cannot grow this tree: " (syntax-tree tree) " at: " frontier-path "; child-spec="
                       (u/strip-refs child-spec) " (no phrases or lexemes match)")
                  {:tree tree
                   :frontier-path frontier-path
                   :depth depth
                   :max-depth max-depth
                   :child-spec child-spec})))
        (if (and (empty? child-lexemes) (>= depth max-depth))
          (throw (ex-info
                  (str "cannot grow this tree: " (syntax-tree tree) " at: " frontier-path ". (max depth reached)")
                  {:tree tree
                   :frontier-path frontier-path
                   :depth depth
                   :max-depth max-depth
                   :child-spec child-spec})))
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
                    ((fn [tree]
                       (log/debug (str "adding child at: " frontier-path))
                       tree))
                    (u/assoc-in! frontier-path child)
                    (terminate-up frontier-path))))
         (lazy-mapcat (fn [tree]
                        (log/debug (str "size of tree: " (syntax-tree tree) " : " (count (str tree))))
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
                       (= false (u/get-in % [:exception] false))))
         (map #(unify % spec))
         (filter #(not (= :fail %)))
         shuffle-or-not
         (map #(u/assoc-in! % [::done?] true)))))

;; https://github.com/weavejester/medley/blob/1.1.0/src/medley/core.cljc#L20
(defn dissoc-in
  "Dissociate a value in a nested associative structure, identified by a sequence
  of keys. Any collections left empty by the operation will be dissociated from
  their containing structures."
  [m ks]
  (if-let [[head & tail] ks]
    (if tail
      (let [v (dissoc-in (get m head) tail)]
        (if (empty? v)
          (dissoc m head)
          (assoc m head v)))
      (dissoc m head))
    m))

(defn find-tail-path [tree path]
  (if (nil? (u/get-in tree path))
    path
    (find-tail-path tree (concat path [:rest]))))

(defn make-word []
   {:agr (atom :top)
    :canonical (atom :top)
    :exceptions (atom :top)
    :cat (atom :top)
    :infl (atom :top)
    :inflected? (atom :top)
    :root (atom :top)})

(def unify-morphology-leaf-leaf
  (let [one (make-word)
        two (make-word)]
    {:1 one
     :2 two
     :words {:first one
             :rest {:first two}}}))

(def unify-morphology-leaf-tree
  (let [one (make-word)
        two (atom :top)]
    {:1 one
     :2 {:words two}
     :words {:first one
             :rest two}}))

(def unify-morphology-tree-leaf
  (make-word))

(defn create-words [tree frontier-path]
  (cond (and
         (= false (u/get-in tree (concat frontier-path [:1 :phrasal]) false))
         (= false (u/get-in tree (concat frontier-path [:2 :phrasal]) false)))
        ;; (1): both children at _frontier-path_ are leaves.
        (do
          (-> tree
              (u/assoc-in frontier-path unify-morphology-leaf-leaf)))

        ;; (2) child :1 at _frontier-path_ is a leaf, child :2 is a tree.
        (and (= false (u/get-in tree (concat frontier-path [:1 :phrasal]) false))
             (= true (u/get-in tree (concat frontier-path [:2 :phrasal]) false)))
        (do
          (log/debug (str "create-words(1)"))
          (-> tree
              (u/assoc-in frontier-path unify-morphology-leaf-tree)))

        ;; (3): child :1 at _frontier-path_ is a tree; child :2 is a leaf.
        (and (= true (u/get-in tree (concat frontier-path [:1 :phrasal]) false))
             (= false (u/get-in tree (concat frontier-path [:2 :phrasal]) false)))
        (let [tail-path (find-tail-path (u/get-in tree (concat frontier-path [:1 :words]))
                                        [])]
          (log/debug (str "create-words(3); tree is at: " (vec (concat frontier-path [:2]))
                          "; tail-path is: " (vec tail-path)))
          (->
           tree
           (u/assoc-in (concat frontier-path [:words])
                       (u/get-in tree (concat frontier-path [:1 :words])))
           (u/assoc-in frontier-path
                       (unify {:2 unify-morphology-tree-leaf
                               :words
                               (s/create-path-in (concat tail-path [:first])
                                                 unify-morphology-tree-leaf)}))))

        ;; (4) both children at _frontier-path_ are trees.
        (and (= true (u/get-in tree (concat frontier-path [:1 :phrasal]) false)))
        (let [tail-path (find-tail-path (u/get-in tree (concat frontier-path [:1 :words]))
                                        [])]
          (log/debug (str "create-words(4)"))
          (-> tree
              (u/assoc-in
               (concat frontier-path [:words])
               (u/assoc-in (u/get-in tree (concat frontier-path [:1 :words]))
                           tail-path
                           (u/get-in tree (concat frontier-path [:2 :words]))))))

        ;; TODO: throw exception here.
        true
        (let [tail-path (find-tail-path (u/get-in tree (concat frontier-path [:words])))]
          (log/warn (str "create-words(5)..??? tail-path: " tail-path))
          tree)))

(defn truncate [m]
  (-> (reduce (fn [m path]
                 (dissoc-in m path))
              m
              [[:comp] [:1]
               [:head] [:2]])
      (assoc :syntax-tree (syntax-tree m))))

(defn truncate-in
  "Truncate the value at path _path_ within _m_. if path is not empty, then 
  (get (u/get-in m (butlast path)) (last path)) must be an atom."
  [m path]
  (if (not (empty? path))
    (do
      (swap! (get (u/get-in m (butlast path))
                  (last path))
             (fn [x] (truncate (u/get-in m path))))
      m)
    (truncate m)))

(defn terminate-up [tree frontier-path]
  (log/debug (str "terminate-up: " (vec frontier-path)))
  (cond
    (and (= :comp (last frontier-path))
         (u/get-in tree (concat frontier-path [::done?])))
    (do
      (->
       tree
       (create-words (butlast frontier-path))
       ((fn [tree]
          (if (and (u/get-in tree (concat (butlast frontier-path) [:phrasal]))
                   truncate?)
              (truncate-in tree (butlast frontier-path))
           tree)))
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

(defn lazy-mapcat [f seqs]
  (if (not (empty? seqs))
     (lazy-cat
       (f (first seqs))
       (lazy-mapcat f (rest seqs)))
     []))

