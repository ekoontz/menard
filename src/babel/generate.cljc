(ns babel.generate
  (:refer-clojure :exclude [assoc-in get-in deref resolve find parents])
  (:require
   [babel.index :refer [intersection-with-identity]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.math.combinatorics :as combo]
   [clojure.string :as string]
   [dag_unify.core :refer [assoc-in assoc-in! copy create-path-in
                           dissoc-paths fail-path get-in fail? strip-refs unify unify!]]))
                                        
;; during generation, will not decend deeper than this when creating a tree:
;; TODO: should also be possible to override per-language.
(def ^:const max-depth 5)
(def ^:const max-total-depth max-depth)

;; use map or pmap.
(def ^:const mapfn map)

(def ^:const handle-unify-fail #(log/debug %))
(def ^:const throw-exception-on-unify-fail false)

(def ^:const shufflefn
  (fn [x]
    ;; deterministic generation:
;;    x
    ;; nondeterministic generation
    (lazy-seq (shuffle x))

    ))

(declare add-comp-to-bolts)
(declare add-comps-to-bolt)
(declare add-to-bolt-at-path)
(declare get-bolts-for)
(declare get-lexemes)
(declare lightning-bolts)
(declare comp-paths)
(declare gen)
(declare show-spec)

(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  [spec language-model]
  ;; 
  ;; Return the tree of an expression, generated by the given model,
  ;; that satisfies given spec:
  ;;
  ;; The tree will look like:
  ;; 
  ;; Where H is the head of a certain node in the tree, and C
  ;; is the complement. Terminal nodes are lexemes given by the model.
  ;;
  ;;       H   
  ;;      / \  
  ;;     H   C 
  ;;        / \  
  ;;       H   C 
  ;; 
  ;; A convenient wrapper around (defn gen) (below).

  (log/debug (str "(generate) with model named: " (:name language-model)))
  (first (gen spec language-model 0)))

(defn gen
  "Return a lazy sequence of every possible expression given the spec and model,
  each of whose depth is no greater than the given depth. Trees are returned in 
  ascending depth."
  [spec model depth & [from-bolts]]
  ;; 
  ;; Given a spec and a model, return the (potentially infinite) set
  ;; of all trees, in ascending depth, that satisfy the given spec.
  ;;
  ;;
  ;; These trees look like:
  ;;
  ;;  First all the depth=0 trees (simply lexemes) that satisfy the spec:
  ;; 
  ;;         H .. H ..

  ;;   Then we have the trees of depth=1 that satisfy the spec:
  ;; 
  ;;       H         H
  ;;  ..  / \  ..   / \ ..
  ;;     H   C     C   H
  ;;
  ;;   And then all trees of depth=2 that satisfy the spec:
  ;;
  ;; 
  ;;       H            H
  ;;      / \          / \
  ;;  .. H   C  ..    C   H .. 
  ;;    /            / \
  ;;   H            C   H
  ;;
  ;; And so on.
  ;;
  ;;
  (log/trace (str "gen@" depth "; spec=" (show-spec spec)))
  (when (< depth max-depth)
    (let [bolts (or from-bolts
                    (get-bolts-for model spec 
                                   depth))]
      (if (not (empty? bolts))
        (do
          (log/trace (str "gen@" depth "; found bolts with spec=" (dag_unify.core/strip-refs spec)))
          (lazy-cat
           (let [bolt (first bolts)]
             (or
              (and (= false (get-in bolt [:phrasal] true))
                   ;; This is not a bolt but rather simply a lexical head,
                   ;; so just return a list with this lexical head:
                   [bolt])
              ;; ..otherwise it's a phrase, so return the lazy
              ;; sequence of adding all possible complements at every possible
              ;; position at the bolt.
              (add-comps-to-bolt bolt model
                                 (reverse (comp-paths depth)))))
           (gen spec model depth (rest bolts))))
        (if (not (= false (get-in spec [:phrasal] true)))
          (gen spec model (+ 1 depth)))))))

(defn get-bolts-for
  "Return every possible bolt for the given model and spec."
  [model spec depth]
  ;; Wrapper around (defn lightning-bolts) to provide a way to
  ;; test indexing and memoization strategies.
  (let [result
        ;; TODO: this is an example of obtaining pre-computed bolts
        ;; from a model given a spec and a depth.
        ;; This is a contrived example - should be
        ;; for the model to provide these given a spec and a depth.
        (and (= () (get-in spec [:synsem :subcat]))
             (= :verb (get-in spec [:synsem :cat]))
             (= :present (get-in spec [:synsem :sem :tense]))
             (= true (get-in spec [:synsem :sem :reflexive]))
             (= :perfect (get-in spec [:synsem :sem :aspect]))
             (not (nil? (:reflexive-bolts model))))]
    (cond
      (and (= depth 3) (= result true))
      (shufflefn (->> (:reflexive-bolts model)
                      (map #(unify spec %))
                      (filter #(not (= :fail %)))))
      (= result true) []
      true (lightning-bolts model spec 0 depth))))

(defn lightning-bolts
  "Return every possible bolt for the given model and spec. Start at the given depth and
   keep generating until the given max-depth is reached."
  [model spec depth max-depth & [use-candidate-parents]]
  ;; 'lightning bolts' look like:
  ;; 
  ;; 
  ;;   H        H    H
  ;;    \      /      \
  ;;     H    H        H        ...
  ;;    /      \        \
  ;;   H        ..       ..
  ;;    \
  ;;     ..
  ;; 
  ;; Each bolt is a tree with only one child per parent: the head child.
  ;; Each head child may be a leaf or
  ;; otherwise has a child with the same two options (leaf or a head child),
  ;; up to the maximum depth.
  (if (and (< depth max-depth)
           (not (= false (get-in spec [:phrasal] true))))
    (let [candidate-parents (or use-candidate-parents
                                (->>
                                 (:grammar model)
                                 (map #(unify % spec))
                                 (filter #(not (= :fail %)))
                                 (shufflefn)))]
      (if (not (empty? candidate-parents))
        (let [candidate-parent (first candidate-parents)]
          (lazy-cat
           (->> (lightning-bolts model
                                 (get-in candidate-parent [:head])
                                 (+ 1 depth)
                                 max-depth)
                (map (fn [head]
                       (assoc-in candidate-parent [:head] head))))
           (lightning-bolts model spec depth max-depth (rest candidate-parents))))))
    (shufflefn (get-lexemes model spec))))

(defn add-comps-to-bolt
  "bolt + paths => trees"
  [bolt model comp-paths]
  (if (and true (not (empty? comp-paths)))
    (let [comp-path (first comp-paths)]
      (log/debug (str "add-comps-to-bolt: " ((:morph-ps model) bolt) "@[" (string/join " " comp-path) "]"))
      (add-comp-to-bolts 
       (add-comps-to-bolt bolt model (rest comp-paths))
       comp-path
       model))
    [bolt]))

(defn add-comp-to-bolts
  "bolts + path => partial trees"
  [bolts path model]
  (if (not (empty? bolts))
    (let [bolt (first bolts)]
      (log/debug (str "add-comp-to-bolts: " ((:morph-ps model) bolt) "@[" (string/join " " path) "]"))
      (lazy-cat
       (add-to-bolt-at-path bolt path model)
       (add-comp-to-bolts (rest bolts) path model)))))

(defn hd [] :head)

(defn comp-paths
  "Find all paths to all complements (both terminal and non-terminal) given a depth. Returned in 
   ascending length (shortest first)."
  ;; e.g., a tree of depth 2
  ;; will have the following paths:
  ;;   [:comp] [:head :comp]
  ;;   because it looks like:
  ;; 
  ;;   H
  ;;  / \
  ;; C   H
  ;;    / \
  ;;   H   C
  ;;
  [depth]
  (cond
    (= depth 0)
    []
    (= depth 1)
    (list [:comp])
    true
    (cons
     (concat (take (- depth 1)
                   (repeatedly hd))
             [:comp])
     (comp-paths (- depth 1)))))

(defn add-to-bolt-at-path
  "generate all complements for bolt at given path, and create a partial tree: bolt + complement => partial tree"
  [bolt path model]
  (->>
   (gen (get-in bolt path) model 0) ;; generate all complements for _bolt_ at _path_.
   (map #(let [partial-tree
               (dag_unify.core/assoc-in! (dag_unify.core/copy bolt) path %)] ;; add the complement to the bolt at _path_.
           ;; apply model's :default-fn, if any.
           (if (:default-fn model)
             ((:default-fn model) partial-tree)
             partial-tree)))))

(defn get-lexemes [model spec]
  "Get lexemes matching the spec. Use a model's index if available, where the index is a function that we call with _spec_ to get a set of indices. otherwise use the model's entire lexeme."
  (->>
   (if (= false (get-in spec [:phrasal] false))
     (if-let [index-fn (:index-fn model)]
       (index-fn spec)
       (do
         (log/warn (str "get-lexemes: no index found: using entire lexicon."))
         (flatten (vals
                   (or (:lexicon (:generate model)) (:lexicon model)))))))
   (filter #(or (= false (get-in % [:exception] false))
                (not (= :verb (get-in % [:synsem :cat])))))
   (map #(unify % spec))
   (filter #(not (= :fail %)))))

(defn show-spec [spec]
  (str "cat=" (get-in spec [:synsem :cat])
       (if (get-in spec [:rule])
         (str "; rule=" (strip-refs (get-in spec [:rule]))))
       (if (not (= (get-in spec [:synsem :agr] ::none) ::none))
         (str "; agr=" (strip-refs (get-in spec [:synsem :agr]))))
       (if (not (= (get-in spec [:synsem :sem :pred] ::none) ::none))
         (str "; pred=" (strip-refs (get-in spec [:synsem :sem :pred]))))
       (if (get-in spec [:synsem :subcat :1 :cat])
         (str "; subcat1=" (strip-refs (get-in spec [:synsem :subcat :1 :cat]))))
       (if (get-in spec [:synsem :subcat :2 :cat])
         (str "; subcat2=" (strip-refs (get-in spec [:synsem :subcat :2 :cat]))))
       (if (get-in spec [:synsem :subcat :3 :cat])
         (str "; subcat3=" (strip-refs (get-in spec [:synsem :subcat :3 :cat]))))
       (if (not (= (get-in spec [:phrasal] ::none) ::none))
         (str "; phrasal=" (strip-refs (get-in spec [:phrasal]))))))
