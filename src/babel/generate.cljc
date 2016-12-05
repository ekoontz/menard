(ns babel.generate
  (:refer-clojure :exclude [assoc-in get-in deref resolve find parents])
  (:require
   [babel.index :refer [intersection-with-identity]]
   [babel.over :as over :refer [show-bolt spec-info truncate truncate-expressions]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.string :as string]
   [dag_unify.core :refer [assoc-in copy fail-path get-in fail? strip-refs unify unify!]]))
                                        
;; during generation, will not decend deeper than this when creating a tree:
;; TODO: should also be possible to override per-language.
(def ^:const max-total-depth 6)

;; TODO support setting max-generated-complements to :unlimited
(def ^:const max-generated-complements 20000)

;; use map or pmap.
(def ^:const mapfn map)

;; should generation be deterministic or random?
(def ^:const deterministic false)

;; deterministic generation:
(def ^:const shufflefn (fn [x] x))

;; nondeterministic generation
;; (def ^:const shufflefn shuffle)

(def ^:const randomize-lexemes-before-phrases
  false)

(def ^:const error-if-no-complements false)

(declare candidate-parents)
(declare do-defaults)
(declare exception)
(declare find-comp-paths)
(declare lazy-mapcat)
(declare lexemes-before-phrases)
(declare lightning-bolts)
(declare not-fail?)

;; TODO: lightning-bolts should use this.
(defn get-lexemes [model spec]
  (if (= false (get-in spec [:phrasal] false))
    (lazy-seq (filter #(not-fail? (unify % spec))
                      (if-let [index-fn (:index-fn model)]
                        (index-fn spec)
                        (flatten (vals
                                  (or (:lexicon (:generate model)) (:lexicon model)))))))
    []))
  
(defn comp-path-to-bolts
  "return a lazy sequence of bolts for all possible complements that can be added to the end of the _path_ within _bolt_."
  [bolt path model depth max-depth]
  (let [spec (get-in bolt path)
        lexemes (lazy-seq (shufflefn (get-lexemes model spec)))
        bolts-at (if (< depth max-depth)
                   (lightning-bolts
                    model
                    (get-in bolt path)
                    depth max-depth))
        lexemes-before-phrases
        (lexemes-before-phrases depth max-depth)]
    (if lexemes-before-phrases
      (lazy-seq (concat lexemes bolts-at))
      (lazy-seq (concat bolts-at lexemes)))))

(declare add-bolt-at)

(defn add-bolts-to-path [path bolts-at bolt top-bolt model depth max-depth truncate? take-n]
  (mapfn #(let [bolt (assoc-in bolt path %)]
            (if (= false (get-in % [:phrasal]))
              [bolt]
              (let [result
                    (add-bolt-at top-bolt bolt path % model depth max-depth truncate? take-n)]
                (if truncate
                  (truncate result [path] model)
                  result))))
         bolts-at))

(defn add-comps-using [bolt model path comp-paths bolts-at-paths depth max-depth top-bolt truncate? take-n]
  (if (empty? comp-paths)
    [bolt]
    (mapfn (fn [bolt-at]
             (let [result (add-bolt-at top-bolt bolt path bolt-at model depth max-depth truncate? take-n)])))))

(defn add-comps
  "given a bolt, return the lazy sequence of all bolts derived from this bolt after adding,
   at each supplied path in comp-paths, the bolts for that path."
  [bolt model comp-paths bolts-at-paths depth max-depth top-bolt truncate? take-n]
  (if (empty? comp-paths)
    [bolt] ;; done: we've added all the comps to the bolt, so just return the bolt as a singleton vector.
    (mapfn #(add-comps % model
                       (rest comp-paths)
                       (rest bolts-at-paths)
                       depth max-depth top-bolt truncate? take-n)
           (flatten
            (add-bolts-to-path
             (first comp-paths) (first bolts-at-paths)
             bolt top-bolt model depth max-depth truncate? take-n)))))

(defn add-bolt-at [top-bolt bolt path bolt-at model depth max-depth truncate? & [take-n]]
  "at _path_ within _bolt_, add all complements derived from _bolt-at_"
  (mapfn #(do-defaults % model)
         (let [comp-paths (find-comp-paths bolt-at)]
           (mapfn #(assoc-in bolt path %)
                  (lazy-seq
                   (flatten
                    (add-comps bolt-at
                               model
                               comp-paths
                               (mapfn #(comp-path-to-bolts bolt-at % model (+ 1 depth) max-depth)
                                      comp-paths)
                               (+ 1 depth)
                               max-depth
                               top-bolt
                               truncate?
                               take-n)))))))
(defn generate
  "Return one (by default) or _n_ (using :take _n_) expressions matching spec _spec_ given the model _model_."
  [spec language-model
   & {:keys [max-total-depth truncate-children lexicon take-n]
      :or {max-total-depth max-total-depth
           lexicon nil
           truncate-children true
           take-n 1}}]
  (log/debug (str "generate: spec: " spec "; take-n: " take-n))
  (let [depth 0
        spec
        (->
         (if (fail? (unify spec {:synsem {:subcat []}}))
           spec
           (unify spec {:synsem {:subcat []}}))
         ;; remove metadata (if any) that's not relevant to generation:
         (dissoc :dag_unify.core/serialized))
        max-depth max-total-depth]
    (->
     (take take-n
           (reduce concat
                   (map (fn [bolt]
                          (log/trace (str "add-bolt-at:" ((:morph-ps language-model) bolt)))
                          (filter #(not (= :fail %))
                                  (add-bolt-at bolt bolt [] bolt language-model
                                               depth max-depth truncate-children take)))
                        (lightning-bolts language-model spec 0 max-total-depth))))
     ((fn [seq]
        (if (= take-n 1)
          (first seq) ;; if caller only wants one, return just that one, rather than a singleton list.
          seq)))))) ;; otherwise, return the lazy seq of expressions

(defn lightning-bolts
  "Returns a lazy-sequence of all possible bolts given a spec, where a bolt is a tree
  such that only the head children are generated. This sequence is used by (generate (above))
  to generate expressions by adding complements using (add-all-comps)."
  [language-model spec depth total-depth
                       & {:keys [max-total-depth]
                          :or {max-total-depth max-total-depth}}]
  (if (nil? spec)
    (exception (str "given a null spec for lightning-bolts.")))
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
        (lazy-cat lexical phrasal)
        (lazy-cat phrasal lexical)))))

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

(defn exception [error-string]
  #?(:clj
     (throw (Exception. (str ": " error-string))))
  #?(:cljs
     (throw (js/Error. error-string))))

;; Thanks to http://clojurian.blogspot.com.br/2012/11/beware-of-mapcat.html
(defn lazy-mapcat  [f coll]
  (lazy-seq
   (if (not-empty coll)
     (concat
      (f (first coll))
      (lazy-mapcat f (rest coll))))))

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
