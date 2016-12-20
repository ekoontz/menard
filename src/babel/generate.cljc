(ns babel.generate
  (:refer-clojure :exclude [assoc-in get-in deref resolve find parents])
  (:require
   [babel.index :refer [intersection-with-identity]]
   [babel.over :as over :refer [show-bolt spec-info]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.math.combinatorics :as combo]
   [clojure.string :as string]
   [dag_unify.core :refer [assoc-in assoc-in! copy dissoc-paths fail-path get-in fail? strip-refs unify unify!]]))
                                        
;; during generation, will not decend deeper than this when creating a tree:
;; TODO: should also be possible to override per-language.
(def ^:const max-total-depth 8)

;; use map or pmap.
(def ^:const mapfn map)

;; deterministic generation:
;;(def ^:const shufflefn (fn [x] x))

;; nondeterministic generation
(def ^:const shufflefn shuffle)

(def ^:const randomize-lexemes-before-phrases
  false)
;;(def ^:const randomize-lexemes-before-phrases
;;  true)

(def ^:const truncate true)

(declare candidate-parents)
(declare comp-path-to-complements)
(declare comp-paths-to-complements)
(declare do-defaults)
(declare find-comp-paths)
(declare get-lexemes)
(declare lexemes-before-phrases)
(declare lightning-bolts)
(declare not-fail?)

(defn generate-all [spec model & [depth max-depth]]
  (let [depth (or depth 0)
        truncate truncate
        max-depth (or max-depth max-total-depth)]
    (log/trace (str "generate-all:" depth "/" max-depth ":         " (strip-refs spec)))
    (->>
     (lightning-bolts model spec depth max-depth)
     (filter not-fail?)
     (map (fn [bolt] ;; for each bolt, create a map of complement positions within the bolt to possible complements for that position
            (assoc
             (comp-paths-to-complements bolt (find-comp-paths bolt) model depth max-depth)
             [] [bolt])))
     (filter #(not (some empty? (vals %))))
     (map (fn [each-bolt-and-comps] ;; for each such map in each bolt, find all possible combinations of complements, taking one complement per path.
            ;; the result is a trellis for each bolt, and a path through this trellis is one complement for each complement position.
            (map (fn [each-path-through-trellis]
                   (zipmap (keys each-bolt-and-comps)
                           each-path-through-trellis))
                 (apply combo/cartesian-product (vals each-bolt-and-comps)))))
     (filter #(not (empty? %)))
     (map (fn [bolt-group]
            (->> bolt-group
                 (map (fn [bolt-and-comps]
                        (apply unify
                               (let [bolt (get bolt-and-comps [])
                                     paths-and-comps (dissoc bolt-and-comps [])
                                     paths (keys paths-and-comps)]
                                 (cons bolt
                                       (map (fn [path]
                                              (let [result (assoc-in bolt path (get paths-and-comps path))]
                                                (if (= true truncate)
                                                  (dissoc-paths result [path])
                                                  result)))
                                            paths))))))
                 (filter #(not (= :fail %))))))
;     (map (fn [expression]
;            (do-defaults expression model)))
;     (filter not-fail?)
;     (map (fn [final]
;            (log/trace (str "final: " ((:morph-ps model) final)))
;            final))
     )))

(defn generate
  "Return one (by default) or _n_ (using :take _n_) expressions matching spec _spec_ given the model _model_."
  [spec language-model
   & {:keys [max-total-depth truncate-children? lexicon take-n]
      :or {max-total-depth max-total-depth
           lexicon nil
           shuffle? nil
           truncate-children? true
           take-n 1}}]
  (first (generate-all spec language-model)))

(defn lightning-bolts
  "Returns a lazy sequence of all possible bolts given a spec, where a bolt is a tree
  such that only the head children are generated. This sequence is used by (generate (above))
  to generate expressions by adding complements using (add-all-comps)."
  [language-model spec depth total-depth
                       & {:keys [max-total-depth]
                          :or {max-total-depth max-total-depth}}]
  (if (nil? spec)
    (throw (Exception. (str "given a null spec for lightning-bolts."))))
  (log/trace (str "lightning-bolts: depth: (" depth "/" max-total-depth ") and spec-info:"
                 (spec-info spec)))
  (let [grammar (:grammar language-model)
        depth (if depth depth 0)
        ;; this is the relative depth; that is, the depth from the top of the current lightning bolt.
        ;; total-depth, on the other hand, is the depth all the way to the top of the entire
        ;; expression, which might involve several parent lightning bolts.
        parents
        (let [parents (shufflefn (candidate-parents grammar spec))]
          (log/trace (str "lightning-bolts: candidate-parents:" (string/join "," (map :rule parents))))
          parents)]
    (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
          (when (= false (get-in spec [:head :phrasal] false))
            (mapcat
             (fn [parent]
               (log/trace (str "lightning-bolts: parent: " (:rule parent) " over lexical heads."))
               (let [subset (get-lexemes language-model (get-in parent [:head] :top))]
                 (map #(assoc-in parent [:head] %)
                      (shufflefn subset))))
             (filter #(= false
                         (get-in % [:head :phrasal] false))
                     parents)))
          phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
          (if (and (< depth max-total-depth)
                   (= true (get-in spec [:head :phrasal] true)))
            (mapcat (fn [parent]
                      (map #(assoc-in parent [:head] %)
                           (lightning-bolts language-model (get-in parent [:head])
                                            (+ 1 depth) (+ 1 total-depth)
                                            :max-total-depth max-total-depth)))
                    (filter #(= true
                                (get-in % [:head :phrasal] true))
                            parents)))]
      (if (lexemes-before-phrases total-depth max-total-depth)
        (concat lexical phrasal)
        (concat phrasal lexical)))))

(defn do-defaults [tree language-model]
  (log/trace (str "calling do-defaults on tree:" ((:morph language-model) tree)))
  (if-let [default-fn (:default-fn language-model)]
    (let [result
          (default-fn tree)]
      (log/trace (str "result of calling do-defaults on tree:" ((:morph language-model) result)))
      result)
    ;;
    (do
      (log/trace (str "language-model has no default function."))
      tree)))

;; TODO: rewrite using (recur)
(defn comp-paths-to-complements [bolt comp-paths model depth max-depth]
  (log/info (str "comp-paths-to-complements: depth=" depth "; max-depth: " max-depth ":" ((:morph-ps model) bolt)))
  (if (not (empty? comp-paths))
    (let [path (first comp-paths)
          comps (filter not-fail? (comp-path-to-complements bolt path model depth max-depth))]
      (assoc
       (comp-paths-to-complements bolt (rest comp-paths) model depth max-depth)
       path comps))))

;; TODO: lightning-bolts should use this.
(defn get-lexemes [model spec]
  (if (= false (get-in spec [:phrasal] false))
    (filter not-fail?
            (map #(unify % spec)
                 (if-let [index-fn (:index-fn model)]
                   (lazy-seq (index-fn spec))
                   (do
                     (log/warn (str "get-lexemes: no index found: using entire lexicon."))
                     (flatten (vals
                               (or (:lexicon (:generate model)) (:lexicon model))))))))))

(defn comp-path-to-complements
  "return a lazy sequence of bolts for all possible complements that can be added to the end of the _path_ within _bolt_."
  [bolt path model depth max-depth]
  (log/debug (str "comp-path-to-complements:" depth "/" max-depth ":" ((:morph-ps model) bolt) "@" path))
  (let [spec (get-in bolt path)
        lexemes (shufflefn (get-lexemes model spec))
        bolts-at (if (< depth max-depth)
                           ;;                                   (generate-all (get-in bolt path) model
                           (lightning-bolts model (get-in bolt path)
                                            (+ 1 depth) max-depth))
        lexemes-before-phrases
        (lexemes-before-phrases depth max-depth)]
    (if lexemes-before-phrases
      (lazy-cat lexemes bolts-at)
      (lazy-cat bolts-at lexemes))))

(defn lexemes-before-phrases
  "returns true or false: true means generate by adding lexemes first;
  otherwise, by adding phrases first. Takes depth as an argument,
  which makes returning true (i.e. lexemes first) increasingly likely
  as depth increases."
  [depth max-total-depth]
  (if (not randomize-lexemes-before-phrases)
    false
    (if (> max-total-depth 0)
      (let [prob (- 1.0 (/ (- max-total-depth depth)
                           max-total-depth))]
        (log/trace (str "P(c," depth ") = " prob " (c: probability of choosing lexemes rather than phrases given depth " depth ")"))
        (> (* 10 prob) (rand-int 10)))
      false)))

(defn not-fail? [arg]
  (not (= :fail arg)))

(defn find-end-of-bolt [bolt]
  (cond (= true (get-in bolt [:phrasal]))
        (cons :head
              (find-end-of-bolt (get-in bolt [:head])))
        true
        []))

(defn find-comp-paths [bolt & [path]]
  (if (not (= false (get-in bolt [:phrasal])))
    (let [path (if (nil? path)
                 (rest (find-end-of-bolt bolt))
                 path)]
      (if (not (empty? path))
        (concat
         [(vec (concat path [:comp]))]
         (find-comp-paths bolt (rest path)))
        [[:comp]]))))

(defn candidate-parents
  "find subset of _rules_ for which each member unifies successfully with _spec_"
  [rules spec]
  (log/trace (str "candidate-parents: spec: " (strip-refs spec)))
  (let [result
        (filter not-fail?
                (mapfn (fn [rule]
                         (log/trace (str "candidate-parents: testing rule: " (:rule rule)))
                         (if (and (not-fail? (unify (get-in rule [:synsem :cat] :top)
                                                    (get-in spec [:synsem :cat] :top))))
                           ;; TODO: add checks for [:synsem :subcat] valence as well as [:synsem :cat].
                           (do
                             (log/trace (str "candidate-parents: " (:rule rule) " is a head candidate for spec:"
                                             (strip-refs spec)))
                             (unify spec rule))
                           (do
                             (log/trace (str "candidate-parents: " (:rule rule) " is *not* a head candidate for spec:"
                                             (strip-refs spec)))
                             :fail)))
                       rules))]
    (log/trace (str "candidate-parents: "
                    (string/join "," (map :rule result))
                    " for spec: " (strip-refs spec)))
    (if (empty? result)
      (log/trace (str "candidate-parents: "
                      "no parents found for spec: " (spec-info spec)))
      (log/trace (str "candidate-parents: "
                    (string/join "," (map :rule result))
                    " for: " (spec-info spec))))
    result))

(defn find-comp-paths [bolt & [path]]
  (if (not (= false (get-in bolt [:phrasal])))
    (let [path (if (nil? path)
                 (rest (find-end-of-bolt bolt))
                 path)]
      (if (not (empty? path))
        (concat
         [(vec (concat path [:comp]))]
         (find-comp-paths bolt (rest path)))
        [[:comp]]))))

