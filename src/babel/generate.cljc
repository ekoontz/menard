(ns babel.generate
  (:refer-clojure :exclude [assoc-in get-in deref resolve find parents])
  (:require
   [babel.index :refer [intersection-with-identity]]
   [babel.over :as over :refer [show-bolt spec-info truncate truncate-expressions]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.math.combinatorics :as combo]
   [clojure.string :as string]
   [dag_unify.core :refer [assoc-in assoc-in! copy dissoc-paths fail-path get-in fail? strip-refs unify unify!]]))
                                        
;; during generation, will not decend deeper than this when creating a tree:
;; TODO: should also be possible to override per-language.
(def ^:const max-total-depth 2)

;; TODO support setting max-generated-complements to :unlimited
(def ^:const max-generated-complements 20000)

;; use map or pmap.
(def ^:const mapfn map)

;; deterministic generation:
(def ^:const shufflefn (fn [x] x))

;; nondeterministic generation
;;(def ^:const shufflefn shuffle)

(def ^:const randomize-lexemes-before-phrases
  false)
;;(def ^:const randomize-lexemes-before-phrases
;;  true)

(def ^:const error-if-no-complements false)

(declare bolts-with-comps)
(declare candidate-parents)
(declare comp-path-to-complements)
(declare comp-paths-to-complements)
(declare do-defaults)
(declare find-comp-paths)
(declare get-lexemes)
(declare lazy-mapcat)
(declare lexemes-before-phrases)
(declare lightning-bolts)
(declare not-fail?)

(defn generate-all [spec model & [depth max-depth]]
  (let [depth (or depth 0)
        truncate true
        max-depth (or max-depth max-total-depth)]
    (log/debug (str "generate-all:" depth "/" max-depth ":         " (strip-refs spec)))
    (->>
     (lightning-bolts model spec depth max-depth)
     (pmap (fn [bolt]
             (let [cp2c
                   (comp-paths-to-complements bolt (find-comp-paths bolt) model depth max-depth)]
               (if (not (nil? cp2c))
                 (merge
                  {[]
                   (filter not-fail? [bolt])}
                  cp2c)))))
     (remove nil?)
     (map (fn [each-bolt-and-comps]
            (let [trellis (apply combo/cartesian-product (vals each-bolt-and-comps))]
              (map (fn [each-path-through-trellis]
                     (zipmap (keys each-bolt-and-comps)
                             each-path-through-trellis))
                   trellis))))
     flatten
     (map (fn [bolt-and-comps]
            (log/debug (str "doing defaults on: " depth "/" max-depth ":" ((:morph-ps model) (get bolt-and-comps []))))
            (do-defaults
             (apply unify
                    (let [bolt (get bolt-and-comps [])
                          bolt-and-comps (dissoc bolt-and-comps [])]
                      (cons bolt
                            (map (fn [path]
                                   (let [result (assoc-in bolt path (get bolt-and-comps path))]
                                     (if (= true truncate)
                                       (dissoc-paths result [path])
                                       result)))
                                 (keys bolt-and-comps)))))
             model)))
     (filter not-fail?))))

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
  "Returns a lazy-sequence of all possible bolts given a spec, where a bolt is a tree
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
        (let [parents (lazy-seq (shufflefn (candidate-parents grammar spec)))]
          (log/trace (str "lightning-bolts: candidate-parents:" (string/join "," (map :rule parents))))
          parents)]
    ;; TODO: use (defn get-lexemes) rather than this code which duplicates (get-lexemes)'s functionality.
    (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
          (when (= false (get-in spec [:head :phrasal] false))
            (lazy-mapcat
             (fn [parent]
               (log/trace (str "lightning-bolts: parent: " (:rule parent) " over lexical heads."))
               (let [lexicon (or (:lexicon (:generate language-model)) (:lexicon language-model))
                     subset (if-let [index-fn (:index-fn language-model)]
                              (do
                                (log/trace (str "using index to find lexical heads for parent:"
                                                (:rule parent)))
                                (index-fn (get-in parent [:head] :top)))
                              (flatten (vals lexicon)))]
                 (let [shuffled-subset (shuffle subset)
                       log (log/debug (str "lexical head candidates:"
                                           (string/join "," (sort (map #((:morph language-model) %)
                                                                       subset)))))
                       result
                       (mapcat #(do
                                  (log/trace (str "trying parent: " (:rule parent) " with lexical head:"
                                                  ((:morph language-model) %)))
                                  (over/overh parent %))
                               (lazy-seq (shufflefn subset)))]
                   (if (and (not (empty? subset)) (empty? result)
                            (> (count subset)
                               50))
                     ;; log/warn because it's very expensive to run
                     ;; over/overh: for every candidate, both parent
                     ;; and candidate head must be copied.
                     (log/warn (str "tried: " (count subset) " lexical candidates with spec:"
                                    (strip-refs spec) " and all of them failed as heads of parent:" (get-in parent [:rule]))))
                   result)))
             (filter #(= false
                         (get-in % [:head :phrasal] false))
                     parents)))
          phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
          (if (and (< depth max-total-depth)
                   (= true (get-in spec [:head :phrasal] true)))
            (lazy-mapcat (fn [parent]
                           (log/trace (str "lightning-bolts: parent: " (:rule parent) " over phrasal heads."))
                           (mapcat #(over/overh parent %)
                                   (lightning-bolts language-model (get-in parent [:head])
                                                    (+ 1 depth) (+ 1 total-depth)
                                                    :max-total-depth max-total-depth)))
                           (filter #(= true
                                       (get-in % [:head :phrasal] true))
                                   parents)))]
      (log/debug (str "lexical-heads for parents:" (string/join "," (map :rule parents)) ":"
                      (string/join ","
                                   (map #((:morph language-model) %)
                                        lexical))))
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
  (if (not (empty? comp-paths))
    (let [path (first comp-paths)
          comps (filter not-fail? (comp-path-to-complements bolt path model depth max-depth))]
      (if (not (empty? comps))
        (merge
         {path comps}
         (comp-paths-to-complements bolt (rest comp-paths) model depth max-depth))))))

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
                   (generate-all
                    (get-in bolt path)
                    model
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

-;; Thanks to http://clojurian.blogspot.com.br/2012/11/beware-of-mapcat.html
(defn lazy-mapcat  [f coll]
  (lazy-seq
   (if (not-empty coll)
     (concat
      (f (first coll))
      (lazy-mapcat f (rest coll))))))

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

