(ns babel.generate
  (:refer-clojure :exclude [get-in deref resolve find parents])
  (:require
   [babel.index :refer [get-lex]]
   [babel.over :as over :refer [show-bolt truncate truncate-expressions]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.string :as string]
   [dag_unify.core :refer [copy get-in fail? strip-refs unify unifyc]]))
                                        
;; during generation, will not decend deeper than this when creating a tree:
;; TODO: should also be possible to override per-language.
(def ^:const max-total-depth 20)
(def ^:const max-generated-complements 200)

;; use map or pmap.
(def ^:const mapfn map)

(def ^:const randomize-lexemes-before-phrases true)
(def ^:const error-if-no-complements false)

(declare add-all-comps)
(declare add-all-comps-with-paths)
(declare add-complement-to-bolt)
(declare any-possible-complement?)
(declare bolt-depth)
(declare candidate-parents)
(declare exception)
(declare find-comp-paths-in)
(declare intersection-with-identity)
(declare lazy-mapcat)
(declare lazy-shuffle)
(declare lexemes-before-phrases)
(declare lightning-bolts)
(declare generate-all)
(declare not-fail?)
(declare spec-info)

;; FIXME: truncate-children=false (the non-default option) is not propagated through all calls,
;; causing trees to be unexpectedly truncated.
(defn generate [spec language-model
                & {:keys [max-total-depth truncate-children lexicon]
                   :or {max-total-depth max-total-depth
                        lexicon nil
                        truncate-children true}}]
  "Returns a single expression generated by the given language model, constrained by the given spec."
  (let [lexicon (if lexicon lexicon (:lexicon language-model))
        morph (:morph language-model)
        morph-ps (:morph-ps language-model)]
    (log/debug (str "generate: generating from spec: "
                    (strip-refs spec) " with max-total-depth: " max-total-depth ";truncate: " truncate-children))
    (let [expression (first (take 1 (generate-all spec language-model 0
                                                  :max-total-depth max-total-depth
                                                  :truncate-children truncate-children)))]
      (if expression
        (do
          (log/debug (str "generate: generated "
                          (morph-ps expression)
                          " for spec:" (strip-refs spec)))
          (log/trace (str "generate: generated "
                          "'" (morph expression) "';"
                          " expr spec: "
                          (unifyc
                           spec
                           {:synsem {:sem (strip-refs (get-in expression [:synsem :sem]))}}))))
        (log/warn (str "generate: no expression could be generated for spec:" (strip-refs spec))))
      expression)))

(defn generate-all [spec language-model total-depth
                    & {:keys [max-total-depth truncate-children]
                       :or {max-total-depth max-total-depth
                            truncate-children true}}]
  "Returns all possible expressions generated by the given language model, constrained by the given spec.
   Depending on the grammar in the language model, could be an infinite number of expressions."
  (log/trace (str "generate-all: spec:" (strip-refs spec)))
  
  (let [total-depth (if total-depth total-depth 0)]
    (if truncate-children
      (->
       (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
       (add-all-comps language-model total-depth true max-total-depth)
       (truncate-expressions [[:head]] language-model))
      (->
       (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
       (add-all-comps language-model total-depth false max-total-depth)))))

(defn lightning-bolts [language-model spec depth total-depth
                       & {:keys [max-total-depth]
                          :or {max-total-depth max-total-depth}}]
  "Returns a lazy-sequence of all possible bolts given a spec, where a bolt is a tree
such that only the head children are generated. This sequence is used by (generate (above))
to generate expressions by adding complements using (add-all-comps)."
  (let [grammar (:grammar language-model)
        depth (if depth depth 0)
        ;; this is the relative depth; that is, the depth from the top of the current lightning bolt.
        ;; total-depth, on the other hand, is the depth all the way to the top of the entire
        ;; expression, which might involve several parent lightning bolts.
        parents (lazy-shuffle (candidate-parents grammar spec))]
    (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
          (when (= false (get-in spec [:head :phrasal] false))
            (lazy-mapcat
             (fn [parent]
               (let [pred (get-in spec [:synsem :sem :pred])
                     cat (get-in spec [:synsem :cat])
                     pred-set (if (:pred2lex language-model) (get (:pred2lex language-model) pred))
                     cat-set (if (:cat2lex language-model) (get (:cat2lex language-model) cat))
                     non-empty-index-sets (filter #(not (empty? %))
                                                  [cat-set pred-set])
                     subset
                     (cond
                       (not (empty? non-empty-index-sets))
                       (reduce intersection-with-identity non-empty-index-sets)

                       true
                       (do
                         (log/warn (str "no index found for spec: " (strip-refs spec)))
                         (get-lex parent :head (:index language-model))))]
                 (log/debug (str "lightning-bolts: (optimizeme) size of subset of candidate heads: " (count subset) " with spec: " (strip-refs spec) " and parent:  " (:rule parent)))
                 (let [result (over/overh parent (lazy-shuffle subset))]
                   (log/debug (str "lightning-bolts: (optimizeme) surviving candidate heads: " (count result)))
                   (if (and (not (empty? subset)) (empty? result))
                     ;; log/warn because it's very expensive to run
                     ;; over/overh: for every candidate, both parent
                     ;; and candidate head must be copied.
                     (log/warn (str "tried: " (count subset) " lexical candidates with spec:" ( strip-refs spec) " and all of them failed as heads of parent:" (:rule parent))))
                   result)))
             parents))
          phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
          (if (and (< total-depth max-total-depth)
                   (= true (get-in spec [:head :phrasal] true)))
            (lazy-mapcat (fn [parent]
                           (over/overh
                            parent
                            (lightning-bolts language-model (get-in parent [:head])
                                             (+ 1 depth) (+ 1 total-depth)
                                             :max-total-depth max-total-depth)))
                         parents))]
        (filter
         (fn [bolt]
           (any-possible-complement?
            bolt [:comp] language-model total-depth
            :max-total-depth max-total-depth))
         (if (lexemes-before-phrases total-depth max-total-depth)
           (lazy-cat lexical phrasal)
           (lazy-cat phrasal lexical))))))

(defn add-all-comps [bolts language-model total-depth truncate-children max-total-depth]
  "At each point in each bolt in the list of list of bolts,
_bolt-groups_, add all possible complements at all open nodes in the
bolt, from deepest and working upward to the top. Return a lazy
sequence of having added all possible complements at each node in the
bolt."
  (lazy-mapcat
   (fn [bolt]
     (add-all-comps-with-paths [bolt] language-model total-depth
                               (find-comp-paths-in (bolt-depth bolt))
                               truncate-children max-total-depth))
   bolts))

(defn add-all-comps-with-paths [bolts language-model total-depth comp-paths truncate-children max-total-depth]
  (if (empty? comp-paths) bolts
      (add-all-comps-with-paths
       (lazy-mapcat
        (fn [bolt]
          (let [path (first comp-paths)]
            (add-complement-to-bolt bolt path
                                    language-model (+ total-depth (count path))
                                    :max-total-depth max-total-depth
                                    :truncate-children truncate-children)))
        bolts)
       language-model total-depth (rest comp-paths) truncate-children max-total-depth)))

(defn add-complement-to-bolt [bolt path language-model total-depth
                              & {:keys [max-total-depth truncate-children]
                                 :or {max-total-depth max-total-depth
                                      truncate-children true}}]
  (log/trace (str "add-complement-to-bolt: " (show-bolt bolt language-model)
                  "@[" (string/join " " path) "]" "^" total-depth))
  (let [lexicon (or (-> :generate :lexicon language-model)
                    (:lexicon language-model))
        from-bolt bolt ;; so we can show what (add-complement-to-bolt) did to the input bolt, for logging.
        spec (get-in bolt path)
        immediate-parent (get-in bolt (butlast path))
        complement-candidate-lexemes
        (if (not (= true (get-in bolt (concat path [:phrasal]))))
          (let [pred (get-in spec [:synsem :sem :pred])
                cat (get-in spec [:synsem :cat])
                pred-set (if (and (:pred2lex language-model)
                                  (not (= :top pred)))
                           (get (:pred2lex language-model) pred))
                cat-set (if (and (:cat2lex language-model)
                                 (not (= :top cat)))
                          (get (:cat2lex language-model) cat))
                subset
                (cond (empty? pred-set)
                      cat-set
                      (empty? cat-set)
                      pred-set
                      true
                      (intersection-with-identity pred-set cat-set))]
            (if (not (empty? subset))
              subset
              (let [index (:index language-model)
                    indexed (if index (get-lex immediate-parent :comp index))]
                (if (not (empty? indexed))
                  indexed
                  (do
                    (log/warn (str "no candidate lexemes were found as a complement."))
                    nil))))))
        bolt-child-synsem (strip-refs (get-in bolt (concat path [:synsem]) :top))
        lexical-complements (lazy-shuffle
                             (filter (fn [lexeme]
                                       (and (not-fail? (unify (strip-refs (get-in lexeme [:synsem] :top))
                                                              bolt-child-synsem))))
                                     complement-candidate-lexemes))]
    (filter #(not-fail? %)
            (mapfn (fn [complement]
                     (let [unified
                           (unify (copy bolt)
                                  (assoc-in {} path 
                                            (copy complement)))]
                       (if truncate-children
                         (truncate unified [path] language-model)
                         unified)))
                   (let [phrasal-complements (if (and (> max-total-depth total-depth)
                                                      (= true (get-in spec [:phrasal] true)))
                                               (generate-all spec language-model (+ (count path) total-depth)
                                                             :max-total-depth max-total-depth))
                         lexemes-before-phrases (lexemes-before-phrases total-depth max-total-depth)]
                     (cond (and lexemes-before-phrases
                                (empty? lexical-complements)
                                (= false (get-in spec [:phrasal] true)))
                           (log/warn (str "failed to generate any lexical complements with spec: "
                                  (strip-refs spec)))
                           
                           (and lexemes-before-phrases
                                (= true (get-in spec [:phrasal] false))
                                (empty? phrasal-complements))
                           (log/warn (str "failed to generate any phrasal complements with spec: "
                                          (strip-refs spec)))
                           
                           (and (empty? lexical-complements)
                                (empty? phrasal-complements))
                           
                           (let [message (str "add-complement-to-bolt: could generate neither phrasal "
                                              "nor lexical complements for "
                                      "bolt:" (show-bolt bolt language-model) "; immediate parent: "
                                      (get-in bolt (concat (butlast path) [:rule]) :norule) " "
                                      "while trying to create a complement: "
                                      (spec-info spec)
                                      )]
                             (log/warn message)
                             (if error-if-no-complements (exception message)))
                           
                           lexemes-before-phrases
                           (take max-generated-complements
                                 (lazy-cat lexical-complements phrasal-complements))
                           true
                           (take max-generated-complements
                                 (lazy-cat phrasal-complements lexical-complements))))))))

;; TODO: was copied from (defn add-complement-to-bolt) and then modified:
;; refactor both above and below so that commonalities are shared.
(defn any-possible-complement? [bolt path language-model total-depth
                                & {:keys [max-total-depth]
                                   :or {max-total-depth max-total-depth}}]
  (let [lexicon (or (-> :generate :lexicon language-model)
                    (:lexicon language-model))
        spec (get-in bolt path)
        immediate-parent (get-in bolt (butlast path))
        complement-candidate-lexemes
        (if (not (= true (get-in bolt (concat path [:phrasal]))))
          (let [pred (get-in spec [:synsem :sem :pred])
                cat (get-in spec [:synsem :cat])
                pred-set (if (and (:pred2lex language-model)
                                  (not (= :top pred)))
                           (get (:pred2lex language-model) pred))
                cat-set (if (and (:cat2lex language-model)
                                 (not (= :top cat)))
                          (get (:cat2lex language-model) cat))
                subset
                (cond (empty? pred-set)
                      cat-set
                      (empty? cat-set)
                      pred-set
                      true
                      (intersection-with-identity pred-set cat-set))]
            (if (not (empty? subset))
              subset
              (let [index (:index language-model)
                    indexed (if index (get-lex immediate-parent :comp index))]
                indexed))))
        bolt-child-synsem (strip-refs (get-in bolt (concat path [:synsem]) :top))
        lexical-complements (filter (fn [lexeme]
                                      (and (not-fail? (unify (strip-refs (get-in lexeme [:synsem] :top))
                                                             bolt-child-synsem))))
                                    complement-candidate-lexemes)]
    (or (not (empty? lexical-complements))
        (not (empty?
              (filter #(not-fail? %)
                      (mapfn (fn [complement]
                               (unify (strip-refs (get-in bolt [:synsem]))
                                      (assoc-in {} (concat path [:synsem])
                                                complement)))
                             (if (and (> max-total-depth total-depth)
                                      (= true (get-in spec [:phrasal] true)))
                               (generate-all spec language-model (+ (count path) total-depth)
                                             :max-total-depth max-total-depth)))))))))
  
(defn bolt-depth [bolt]
  (if-let [head (get-in bolt [:head] nil)]
    (+ 1 (bolt-depth head))
    0))

(defn find-comp-paths-in [depth]
  (cond
    ;; most-frequent cases done statically:
    (= 0 depth) nil
    (= 1 depth) [[:comp]]
    (= 2 depth) [[:head :comp][:comp]]
    (= 3 depth) [[:head :head :comp][:head :comp][:comp]]
    (= 4 depth) [[:head :head :head :comp][:head :head :comp][:head :comp][:comp]]

    ;; 
    true
    (cons (vec (concat (take (- depth 1) (repeatedly (fn [] :head))) [:comp]))
          (find-comp-paths-in (- depth 1)))))

(defn candidate-parents [rules spec]
  "find subset of _rules_ for which each member unifies successfully with _spec_"
  (log/trace (str "candidate-parents: spec: " (strip-refs spec)))
  (filter #(not (= :fail %))
          (pmap (fn [rule]
                  (log/trace (str "candidate-parents: rule: " (:rule rule)))
                  (if (and (not-fail? (unifyc (get-in rule [:synsem :cat] :top)
                                              (get-in spec [:synsem :cat] :top)))
                           (not-fail? (unifyc (get-in rule [:synsem :infl] :top)
                                              (get-in spec [:synsem :infl] :top)))
                           (not-fail? (unifyc (get-in rule [:synsem :sem :tense] :top)
                                               (get-in spec [:synsem :sem :tense] :top)))
                           (not-fail? (unifyc (get-in rule [:synsem :modified] :top)
                                              (get-in spec [:synsem :modified] :top))))
                    (let [result
                           (unifyc spec rule)]
                      (if (fail? result)
                        :fail
                        result))
                    :fail))
                rules)))

(defn lazy-shuffle [seq]
  (lazy-seq (shuffle seq)))

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

(defn lexemes-before-phrases [depth max-total-depth]
  "returns true or false: true means generate by adding lexemes first; otherwise, by adding phrases first. Takes depth as an argument, which makes returning true (i.e. lexemes first) increasingly likely as depth increases."
  (if (not randomize-lexemes-before-phrases)
    false
    (if (> max-total-depth 0)
      (let [prob (- 1.0 (/ (- max-total-depth depth)
                           max-total-depth))]
        (log/trace (str "P(c," depth ") = " prob " (c: probablity of choosing lexemes rather than phrases given a depth)."))
        (> (* 10 prob) (rand-int 10)))
      false)))

(defn spec-info [spec]
  "give a human-readable summary of _spec_."
  (strip-refs
   (merge
    (if-let [cat (get-in spec [:synsem :cat])]
      {:cat cat})
    (if-let [essere (get-in spec [:synsem :essere])]
      {:essere essere})
    (if-let [pred (get-in spec [:synsem :sem :pred])]
      {:pred pred})
    (if-let [agr (get-in spec [:synsem :agr])]
      {:agr agr})
    (if-let [def (get-in spec [:synsem :sem :spec :def])]
      {:def def})
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

(defn intersection-with-identity [set1 set2]
  (if (> (count set1)
         (count set2))
    (filter (fn [member2]
              (some (fn [member1]
                      (identical? member1 member2))
                    set1))
            set2)
    (filter (fn [member1]
              (some (fn [member2]
                      (identical? member1 member2))
                    set2))
            set1)))

(defn not-fail? [arg]
  (not (= :fail arg)))
