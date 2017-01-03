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
(def ^:const shufflefn (fn [x] (lazy-seq (shuffle x))))

(def ^:const randomize-lexemes-before-phrases
;;  false)
;;(def ^:const randomize-lexemes-before-phrases
  true)

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
    (log/debug (str "generate-all:" depth "/" max-depth ";pred=" (get-in spec [:synsem :sem :pred])))
    (->>
     (lightning-bolts model spec depth max-depth)

     ;; for each bolt, create a map of complement positions
     ;; within the bolt to possible complements for that position
     (map #(assoc
            (comp-paths-to-complements % model depth max-depth)
            [] (list %)))

     ;; for each such map in each bolt, find all possible combinations of complements, taking one complement per path.
     ;; the result is a trellis for each bolt, and a path through this trellis is one complement for each complement position.
     (map (fn [each-bolt-and-comps]
            (let [paths-to-comps (keys each-bolt-and-comps)
                  vals (vals each-bolt-and-comps)]
              (map (fn [each-path-through-trellis]
                     (zipmap paths-to-comps
                             each-path-through-trellis))
                   (apply combo/cartesian-product vals)))))

     (mapcat (fn [bolt-group]
               (->> bolt-group
                    (map (fn [bolt-and-comps]
                           (let [bolt (get bolt-and-comps [])
                                 paths-and-comps (dissoc bolt-and-comps [])
                                 paths-to-comps (keys paths-and-comps)]
                             (apply unify
                                    (cons bolt
                                          (map (fn [path-to-comp]
                                                 (let [complement (get paths-and-comps path-to-comp)
                                                       result (assoc-in bolt path-to-comp complement)]
                                                   (if (= true truncate)
                                                     (dissoc-paths result [path-to-comp])
                                                     result)))
                                               paths-to-comps)))))))))
     (map #(do-defaults % model)))))

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

(defn comp-paths-to-complements [bolt model depth max-depth]
  (log/info (str "comp-paths-to-complements: bolt:" ((:morph-ps model) bolt)))
  (into {}
        (map (fn [path]
               [path (lazy-seq (comp-path-to-complements bolt path model depth max-depth))])
             (find-comp-paths bolt))))

(defn comp-path-to-complements
  "return a lazy sequence of bolts for all possible complements that can be added to the end of the _path_ within _bolt_."
  [bolt path model depth max-depth]
  (log/info (str "comp-path-to-complements:" depth "/" max-depth ":" ((:morph-ps model) bolt) "@" path))
  (let [spec (get-in bolt path)
        lexemes (shufflefn (get-lexemes model spec))
        bolts-at (if (< depth max-depth)
                   (lazy-seq (generate-all (get-in bolt path) model
                                           (+ 1 depth) max-depth)))
        lexemes-before-phrases
        (or true (lexemes-before-phrases depth max-depth))]
    (if (not (nil? bolts-at)) (log/info (str "realized? of bolts-at:" (realized? bolts-at))))
    (cond (nil? bolts-at)
          (lazy-seq lexemes)
          lexemes-before-phrases
          (lazy-cat lexemes bolts-at)
          true
          (lazy-cat bolts-at lexemes))))

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

