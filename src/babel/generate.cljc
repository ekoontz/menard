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
(def ^:const max-total-depth 8)

;; use map or pmap.
(def ^:const mapfn map)

(def ^:const handle-unify-fail #(log/debug %))
(def ^:const throw-exception-on-unify-fail false)

;; deterministic generation:
;;(def ^:const shufflefn (fn [x] x))

;; nondeterministic generation
(def ^:const shufflefn (fn [x] (shuffle x)))

(def ^:const randomize-lexemes-before-phrases
;;  false)
;;(def ^:const randomize-lexemes-before-phrases
  true)

;; whether to remove [:head] and [:comp] paths from generated trees after generation:
;; for performance.
(def ^:const truncate false)

(declare candidate-parents)
(declare comp-path-to-complements)
(declare do-defaults)
(declare find-comp-paths)
(declare get-lexemes)
(declare lexemes-before-phrases)
(declare lightning-bolts)
(declare not-fail?)
(declare show-bolt)
(declare spec-info)
(declare unify-and-log)
;; TODO: demote 'depth' and 'max-depth' down to lower-level functions.
(defn generate-all [spec model & [depth max-depth]]
  (let [depth (or depth 0)
        truncate truncate
        max-depth (or max-depth max-total-depth)]
    (log/debug (str "generate-all:" depth "/" max-depth ":" (spec-info spec)))
    (->>

     ;; 1. Generate 'lightning bolts':
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
     ;; Complements get filled in later, at step 3.
     (lightning-bolts model spec depth max-depth)

     ;; 2. debug logging.
     (map (fn [bolt]
            (do (log/debug (str "bolt:" (show-bolt bolt model)))
                bolt)))

     ;; 3. For each bolt, create a map:
     ;;
     ;; B1:P1_1 => C1_1_1,C1_1_2,C1_1_3,..
     ;; B1:P1_2 => C1_2_1,C1_2_2,C1_2_3,..
     ;; ..
     ;; B2:P2_1 => C2_1_1,C2_1_2,C2_1_3,..
     ;; B2:P2_2 => C2_2_1,...
     ;; ..
     ;;
     ;; from paths P to possible complements C at that path in the bolt.
     (map (fn [bolt]
            (let [paths (find-comp-paths bolt)]
              {:bolt bolt
               :paths paths
               :comps (map #(comp-path-to-complements bolt % model depth max-depth) paths)})))

     ;; 4. Create the cartesian product of all the possible complements at all paths within each bolt.
     (mapcat (fn [{bolt :bolt comps :comps paths :paths}]
               ;; TODO: further flatten this into the overall ->> pipeline
               (map (fn [each-path-through-trellis]
                      {:bolt bolt
                       :paths paths
                       :trellis each-path-through-trellis})
                    (lazy-seq (apply combo/cartesian-product comps)))))

     ;; 5. For each bold, create tree parts for each path-complement pair of the bolt.
     (map (fn [{bolt :bolt paths :paths trellis :trellis}]
            (cons bolt
                  ;; TODO: further flatten this into the overall ->> pipeline
                  (map (fn [[path complement]]
                         (assoc-in {} path complement))
                       (zipmap paths trellis)))))

     ;; 6. Combine each set of tree parts into a tree.
     (map (fn [tree-parts]
            (reduce (fn [a b]
                      (unify-and-log a b model))
                    tree-parts)))

     ;; 7. Apply model's defaults, if any, to each tree.
     (map #(do-defaults % model))

     ;; 8. If desired, truncate trees to just a root node with no children, for efficiency of future processing.
     (map #(if (= true truncate)
             (dag_unify.core/strip-refs (dissoc-paths % [[:head][:comp]]))
             %))

     ;; 11. Remove fails.
     (remove #(= :fail %))
     )))

(def expensive-logging false)

(defn unify-and-log [a b model]
  (let [result (unify a b)]
    (when (and (not (= :fail a)) (not (= :fail b))
               expensive-logging)
      (if (= :fail result)
        (let [message (str "failed to unify:( "
                           "  a: " ((:morph-ps model) a) ","
                           "  b: " ((:morph-ps model) b)
                           "  morph a: " ((:morph model) a) ","
                           "  b: " (keys b)
                           ") => fail@" (fail-path a b))]
          (handle-unify-fail message)
          (if (= true throw-exception-on-unify-fail)
            (throw (Exception. message))))
        
        (log/debug (str "unify(" ((:morph-ps model) a)
                        " , " ((:morph-ps model) b) ") => "
                        ((:morph-ps model) result)))))
    result))

(defn comp-path-to-complements
  "return a lazy sequence of bolts for all possible complements 
  that can be added to the end of the _path_ within _bolt_."
  [bolt path model depth max-depth]
  (let [spec (get-in bolt path)
        log-message-prefix
        (if expensive-logging
          (str "comp-path-to-complements:" depth "/" max-depth ":"
               ((:morph-ps model) bolt) "@" path ":spec:" spec)
          (str "comp-path-to-complements:" depth "/" max-depth ":"
               spec "@path: " path))
        debug (log/debug (str log-message-prefix ": start."))
        lexemes (shufflefn (get-lexemes model spec))
        bolts-at (if (< depth max-depth)
                   (lazy-seq (generate-all (get-in bolt path) model
                                           (+ 1 depth) max-depth)))
        lexemes-before-phrases true]
    (cond (nil? bolts-at)
          (lazy-seq lexemes)

          lexemes-before-phrases
          (lazy-cat lexemes bolts-at)

          true
          (lazy-cat bolts-at lexemes))))

(defn generate
  "Return one (by default) or _n_ (using :take _n_) expressions matching spec _spec_ given the model _model_."
  [spec language-model
   & {:keys [max-total-depth truncate-children? lexicon take-n]
      :or {max-total-depth max-total-depth
           lexicon nil
           shuffle? nil
           truncate-children? true
           take-n 1}}]
  (log/debug (str "(generate) with model named: " (:name language-model)))
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
          (log/debug (str "lightning-bolts: candidate-parents:" (string/join "," (map :rule parents))))
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
        (lazy-cat lexical phrasal)
        (lazy-cat phrasal lexical)))))

(defn do-defaults [tree {default-fn :default-fn
                         morph :morph}]
  (if (not (fn? morph))
    (throw (Exception. (str "(do-defaults) was unexpectedly called without a 'morph' function."))))
  (or (and (fn? default-fn)
           (default-fn tree))
      tree))

(defn get-lexemes [model spec]
  "Get lexemes matching the spec. Use a model's index if available, where the index is a function that we call with _spec_ to get a set of indices. otherwise use the model's entire lexeme."
  (->>

   (if (= false (get-in spec [:phrasal] false))
     (if-let [index-fn (:index-fn model)]
       (lazy-seq (index-fn spec))
       (do
         (log/warn (str "get-lexemes: no index found: using entire lexicon."))
         (flatten (vals
                   (or (:lexicon (:generate model)) (:lexicon model)))))))
   
   (map #(unify % spec))
   (filter #(not (= :fail %)))))

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
  (filter not-fail?
          (mapfn (fn [rule]
                   (log/trace (str "candidate-parents: testing rule: " (:rule rule)))
                   (if (not-fail? (unify (get-in rule [:synsem :cat] :top)
                                         (get-in spec [:synsem :cat] :top)))
                     ;; TODO: add checks for [:synsem :subcat] valence as well as [:synsem :cat].
                     (do
                       (log/trace (str "candidate-parents: " (:rule rule) " is a cat-wise candidate for spec:"
                                       (strip-refs spec)))
                       (let [unified (unify spec rule)]
                         (if (= :fail unified)
                           (log/trace (str "candidate parent: " (:rule rule) " failed at:" (fail-path spec rule)))
                           (log/debug (str "candidate parent: " (:rule rule) " unified successfully with spec:" (strip-refs spec))))
                         unified))
                     (do
                       (log/debug (str "candidate-parents: " (:rule rule) " is *not* a head candidate for spec:"
                                       (strip-refs spec)))
                       :fail)))
                 rules)))

(defn show-bolt [bolt language-model]
  (if (nil? bolt)
    (throw (Exception. (str "don't call show-bolt with bolt=null.")))
    (let [morph (:morph language-model)]
      (if (nil? morph)
        (throw (Exception. (str "don't call show-bolt with morph=null.")))
        (str (if (get-in bolt [:rule]) (str "[" (get-in bolt [:rule]) " "))
             (let [head-bolt (get-in bolt [:head])]
               (if (nil? head-bolt)
                 (morph bolt)
                 (let [rest-str (show-bolt (get-in bolt [:head]) language-model)]
                   (if (not (nil? rest-str))
                     (str "-> " rest-str)))))
             (if (get-in bolt [:rule]) "]"))))))

(defn spec-info
  "give a human-readable summary of _spec_."
  [spec]
  (strip-refs
   (merge
    (if-let [cat (get-in spec [:synsem :cat])]
      {:cat cat})
    (if-let [subcat (get-in spec [:synsem :subcat])]
      {:subcat subcat})
    (if-let [rule (get-in spec [:rule])]
      {:rule rule})
    (if-let [mod (get-in spec [:synsem :mod])]
      {:mod mod})
    (if-let [essere (get-in spec [:synsem :essere])]
      {:essere essere})
    (if-let [pred (get-in spec [:synsem :sem :pred])]
      {:pred pred})
    (if-let [agr (get-in spec [:synsem :agr])]
      {:agr agr})
    (if-let [def (get-in spec [:synsem :sem :spec :def])]
      {:def def})
    (if-let [infl (get-in spec [:synsem :infl])]
      {:infl infl})
    (if-let [pronoun (get-in spec [:synsem :pronoun])]
      {:pronoun pronoun})
    ;; :synsem/:sem/:mod is sometimes used with nil explicitly, so need to have a special test for it
    (let [mod (get-in spec [:synsem :sem :mod] :not-found-by-spec-info)]
      (if (not (= :not-found-by-spec-info mod))
        {:mod mod}))
    (if-let [modified (get-in spec [:modified])]
      {:modified modified})
    (if-let [subcat1 (if (not (empty? (get-in spec [:synsem :subcat])))
                      (get-in spec [:synsem :subcat :1 :cat]))]
      {:subcat/:1/:cat subcat1
       :subcat/:1/:agr (get-in spec [:synsem :subcat :1 :agr])}))))
