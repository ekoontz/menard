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

;; whether to remove [:head] and [:comp] paths from generated trees after generation:
;; for performance.
(def ^:const truncate false)

(declare add-comp-to-bolts)
(declare add-comps-to-bolt)
(declare add-to-bolt-at-path)
(declare candidate-parents)
(declare get-bolts-for)
(declare get-lexemes)
(declare lightning-bolts)
(declare comp-paths)
(declare gen)
(declare show-spec)

(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  [spec language-model]
  (log/debug (str "(generate) with model named: " (:name language-model)))
  (first (gen spec language-model 0)))

(defn gen
  "Return a lazy sequence of every possible tree given a specification and a model."
  [spec model depth & [from-bolts at-path]]
  (log/debug (str "gen@" depth "; spec=" (show-spec spec)))
  (if (< depth max-depth)
    (let [bolts (or from-bolts
                    (get-bolts-for model spec 
                                   depth))]
      (if (not (empty? bolts))
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
                               (comp-paths depth))))
         (gen spec model depth
               (rest bolts)
               at-path))
        (if (not (= false (get-in spec [:phrasal] true)))
          (gen spec model (+ 1 depth) nil at-path))))))

(defn get-bolts-for [model spec depth]
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
  [model spec depth max-depth & [use-candidate-parents]]
  ;; Generate 'lightning bolts':
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
  (if (< depth max-depth)
    (let [candidate-parents (or use-candidate-parents
                                (->>
                                 (candidate-parents (:grammar model) spec depth)
                                 (map #(unify % spec))
                                 (filter #(not (= :fail %)))
                                 (shufflefn)))]
      (if (not (empty? candidate-parents))
        (let [candidate-parent (first candidate-parents)]
          (lazy-cat
           (if (not (= false (get-in spec [:phrasal] true)))
             (->> (lightning-bolts model
                                   (get-in candidate-parent [:head])
                                   (+ 1 depth)
                                   max-depth)
                  (map (fn [head]
                         (assoc-in candidate-parent [:head] head)))))
           (lightning-bolts model spec depth max-depth (rest candidate-parents))))))
    (shufflefn (get-lexemes model spec))))

(defn add-comps-to-bolt
  "bolt + paths => trees"
  [bolt model comp-paths]
  ;;  (log/debug (str "add-comps-to-bolt: " ((:morph-ps model) bolt) " with this many paths: " (count comp-paths)))
  (if (not (empty? comp-paths))
    (add-comp-to-bolts 
     (add-comps-to-bolt bolt model (rest comp-paths))
     (first comp-paths)
     model)
    [bolt]))

(defn add-comp-to-bolts
  "bolts + path => partial trees"
  [bolts path model]
  (if (not (empty? bolts))
    (do
      (log/debug (str "add-comp-to-bolts: path=" (vec path) "; first bolt=" ((:morph-ps model) (first bolts))))
      (lazy-cat
       (let [result
             (add-to-bolt-at-path (first bolts) path model)]
         result)
       (add-comp-to-bolts (rest bolts) path model)))))

(defn comp-paths
  "Find all paths to all complements (both terminal and non-terminal) given a depth. Returned in 
   ascending length (shortest first)."
  ;; e.g., a tree of depth 2
  ;; will have the following paths:
  ;;   [:comp] [:head :comp]
  ;;   because it looks like:
  ;; 
  ;;   H
  ;;  /=\
  ;; C   H
  ;;    /=\
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
                   (repeatedly (fn [] :head)))
             [:comp])
     (comp-paths (- depth 1)))))

(defn add-to-bolt-at-path
  "bolt + path => partial trees"
  [bolt path model]
  (->>
   ;; set of all complements at _path_ for _bolt_:
   (gen (get-in bolt path) model 0 nil path)
     
   ;; add each member _each_comp_ of this set to _bolt_:
   (map (fn [each-comp]
          (->
           bolt
           (dag_unify.core/assoc-in path
                                    each-comp)
           ((fn [tree]
              (if (:default-fn model)
                ((:default-fn model) tree)
                tree))))))))

(defn candidate-parents
  "find subset of _rules_ for which each member unifies successfully with _spec_; _depth_ is only used for diagnostic logging."
  [rules spec depth]
  (filter #(not (= :fail %))
          (mapfn (fn [rule]
                   (log/trace (str "candidate-parents: testing rule: " (:rule rule) "; depth: " depth))
                   (let [unified (unify spec rule)]
                     (if (= :fail unified)
                       (log/trace (str "candidate parent: " (:rule rule) " failed at:" (fail-path spec rule)))
                       (log/debug (str "candidate parent: " (:rule rule) " spec:" (show-spec spec)
                                       "; depth: " depth)))
                     unified))
                 rules)))

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
       (if (get-in spec [:synsem :subcat :1 :cat])
         (str "; subcat1=" (strip-refs (get-in spec [:synsem :subcat :1 :cat]))))
       (if (get-in spec [:synsem :subcat :2 :cat])
         (str "; subcat2=" (strip-refs (get-in spec [:synsem :subcat :2 :cat]))))
       (if (get-in spec [:synsem :subcat :3 :cat])
         (str "; subcat3=" (strip-refs (get-in spec [:synsem :subcat :3 :cat]))))
       (if (not (= (get-in spec [:phrasal] ::none) ::none))
         (str "; phrasal=" (strip-refs (get-in spec [:phrasal]))))))
