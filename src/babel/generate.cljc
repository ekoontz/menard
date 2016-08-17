(ns babel.generate
  (:refer-clojure :exclude [get-in deref resolve find parents])
  (:require
   [babel.index :refer [get-lex]]
   [babel.over :as over :refer [morph-with-recovery show-bolt truncate truncate-expressions]]
   [babel.stringutils :refer [show-as-tree]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.string :as string]
   [dag_unify.core :refer (copy get-in fail? fail-path
                                ref? remove-false remove-top-values-log
                                strip-refs unify unifyc
                                        )]))
                                        
;; during generation, will not decend deeper than this when creating a tree:
;; should also be possible to override per-language.
(def ^:const max-total-depth 20)
(def ^:const max-generated-complements 200)

;; use map or pmap.
(def ^:const mapfn map)

(def ^:const randomize-lexemes-before-phrases true)
(def ^:const error-if-no-complements false)

(declare add-complement-to-bolt)
(declare exception)
(declare lazy-mapcat)
(declare lexemes-before-phrases)
(declare lightning-bolts)
(declare generate-all)
(declare path-to-map)
(declare spec-info)

;; FIXME: truncate-children=false (the non-default option) is not propagated through all calls,
;; causing trees to be unexpectedly truncated.
(defn generate [spec language-model
                & {:keys [max-total-depth truncate-children lexicon]
                   :or {max-total-depth max-total-depth
                        lexicon nil
                        truncate-children true}}]
  (let [language-model (if (future? language-model) @language-model language-model)
        lexicon (if lexicon lexicon (:lexicon language-model))
        morph (:morph language-model)]
    (log/debug (str "generate: generating from spec: "
                    (strip-refs spec) " with max-total-depth: " max-total-depth ";truncate: " truncate-children))
    (let [expression (first (take 1 (generate-all spec language-model 0
                                                  :max-total-depth max-total-depth
                                                  :truncate-children truncate-children)))]
      (if expression
        (do
          (log/info (str "generate: generated "
                         "'" (morph expression) "'"
                         " for spec:" (strip-refs spec)))
          (log/trace (str "generate: generated "
                          "'" (morph expression) "';"
                          " expr spec: "
                          (unifyc
                           spec
                           {:synsem {:sem (strip-refs (get-in expression [:synsem :sem]))}}))))
        (log/warn (str "generate: no expression could be generated for spec:" (strip-refs spec))))
      expression)))

(declare add-complements)
(declare add-all-comps)
(declare candidate-parents)
(declare lazy-shuffle)
(declare find-comp-paths-in)
(declare bolt-depth)
(declare add-all-comps)
(declare add-all-comps-with-paths)

(defn generate-all [spec language-model total-depth
                    & {:keys [max-total-depth truncate-children]
                       :or {max-total-depth max-total-depth
                            truncate-children true}}]
  (log/trace (str "generate-all: generating from spec with cat "
                  (get-in spec [:synsem :cat])))
  (log/debug (str "generate-all:"
                  (strip-refs (get-in spec [:synsem :cat])) "^" total-depth))
  
  (let [language-model (if (future? language-model) @language-model language-model)
        total-depth (if total-depth total-depth 0)]
    (if truncate-children
      (->
       (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
       (add-all-comps language-model total-depth true max-total-depth)
       (truncate-expressions [[:head]] language-model))
      (->
       (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
       (add-all-comps language-model total-depth false max-total-depth)))))

(defn add-all-comps-from-group [bolt-group language-model total-depth
                                truncate-children max-total-depth]
  (mapfn
   (fn [bolt]
     (log/debug (str "add-all-comps: adding comps to bolt: " (show-bolt bolt language-model)))
     (add-all-comps-with-paths [bolt] language-model total-depth
                               (find-comp-paths-in (bolt-depth bolt))
                               truncate-children max-total-depth))
   bolt-group))

(defn log-bolt-groups [bolt-groups language-model]
  (log/debug
   (str "bolt-groups:\n"
        (string/join "\n"
                     (map (fn [bolt-group]
                            (str "bolt-group:\n"
                                 (string/join "\n"
                                              (map (fn [bolt]
                                                     (str " " (show-bolt bolt language-model)))
                                                   bolt-group))))
                          bolt-groups)))))

;; TODO: catch exception thrown by add-complement-by-bolt: "could generate neither phrasal nor lexical complements for bolt"
(defn add-all-comps [bolt-groups language-model total-depth truncate-children max-total-depth]
  (let [bolt-groups
        (filter #(not (empty? %))
                bolt-groups)
        bolts (reduce concat bolt-groups)]
    (when (not (empty? bolt-groups))
      (log/debug (str "bolt group count: " (count bolt-groups)))
      (log/debug (str "total bolt count: " (count bolts)))
      (log/debug (str "counts by group: [ "
                      (string/join ","
                                   (map (fn [group]
                                          (count group))
                                        bolt-groups))
                      " ]"))
      (lazy-mapcat
       (fn [bolt]
         (log/debug (str "add-all-comps: adding comps to bolt: " (show-bolt bolt language-model)))
         (add-all-comps-with-paths [bolt] language-model total-depth
                                   (find-comp-paths-in (bolt-depth bolt))
                                   truncate-children max-total-depth))
       (do (log-bolt-groups bolt-groups language-model)
           (reduce concat bolt-groups))))))



;; TODO: make this non-recursive by using mapcat.
(defn add-all-comps-with-paths [bolts language-model total-depth comp-paths truncate-children max-total-depth]
  (if (not (empty? comp-paths))
    (let [path (first comp-paths)
          result
             (lazy-mapcat
              (fn [bolt]
                (add-complement-to-bolt bolt path
                                        language-model (+ total-depth (count path))
                                        :max-total-depth max-total-depth
                                        :truncate-children truncate-children))
              bolts)]
      (add-all-comps-with-paths
       result
       language-model
       total-depth
       (rest comp-paths)
       truncate-children
       max-total-depth))
    bolts))

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

(defn add-complement-to-bolt [bolt path language-model total-depth
                              & {:keys [max-total-depth truncate-children]
                                 :or {max-total-depth max-total-depth
                                      truncate-children true}}]
  (log/debug (str "add-complement-to-bolt: " (show-bolt bolt language-model)
                  "@[" (string/join " " path) "]" "^" total-depth))
  (let [index (:index language-model)
        lexicon (if (-> :generate :lexicon language-model)
                  (-> :generate :lexicon language-model)
                  (:lexicon language-model))
        from-bolt bolt ;; so we can show what (add-complement-to-bolt) did to the input bolt, for logging.
        spec (get-in bolt path)
        immediate-parent (get-in bolt (butlast path))
        complement-candidate-lexemes
        (if (not (= true
                    (get-in bolt (concat path [:phrasal]))))
          (let [indexed (if index
                         (get-lex immediate-parent :comp index))]
            (if indexed indexed
                (flatten (vals lexicon)))))
        
        complement-pre-check (fn [child parent path-to-child]
                               (let [child-in-bolt (get-in bolt path-to-child)]
                                 (not (fail?
                                       (unifyc (get-in child [:synsem] :top)
                                               (get-in child-in-bolt [:synsem] :top))))))

        filtered-lexical-complements (lazy-shuffle
                                      (filter (fn [lexeme]
                                                (complement-pre-check lexeme bolt path))
                                              complement-candidate-lexemes))]
    (filter #(not (fail? %))
            (mapfn (fn [complement]
                     (let [unified
                           (unify (copy bolt)
                                  (path-to-map path
                                               (copy complement)))]
                       (if truncate-children
                         (truncate unified [path] language-model)
                         unified)))
                  (let [debug (log/trace (str "add-complement-to-bolt(total-depth=" total-depth
                                              ",path=" path ",bolt=(" (show-bolt bolt language-model)
                                              "): calling generate-all(" (strip-refs spec) ");"
                                              "spec: " spec))
                        phrasal-complements (if (and (> max-total-depth total-depth)
                                                     (= true (get-in spec [:phrasal] true)))
                                              (generate-all spec language-model (+ (count path) total-depth)
                                                            :max-total-depth max-total-depth))
                        lexemes-before-phrases (lexemes-before-phrases total-depth max-total-depth)]
                    (cond (and lexemes-before-phrases
                               (empty? filtered-lexical-complements)
                               (= false (get-in spec [:phrasal] true)))
                          (log/warn (str "failed to generate any lexical complements with spec: "
                                         (strip-refs spec)))

                          (and lexemes-before-phrases
                               (= true (get-in spec [:phrasal] false))
                               (empty? phrasal-complements))
                          (log/warn (str "failed to generate any phrasal complements with spec: "
                                         (strip-refs spec)))

                          (and (empty? filtered-lexical-complements)
                               (empty? phrasal-complements))

                          (let [message (str "add-complement-to-bolt: could generate neither phrasal "
                                         "nor lexical complements for "
                                         "bolt:" (show-bolt bolt language-model) "; immediate parent: "
                                          (get-in bolt (concat (butlast path) [:rule]) :norule) " "
                                          "while trying to create a complement: "
                                          (spec-info spec)
                                          )]
                            (log/warn message)
                            ;; TODO: add optional exception-throwing if
                            ;; "could generate neither phrasal nor lexical complements for bolt.." is
                            ;; reached.
                            (if error-if-no-complements (exception message)))

                          lexemes-before-phrases
                          (take max-generated-complements
                                (lazy-cat filtered-lexical-complements phrasal-complements))
                          true
                          (do
                            (log/debug (str "successfully generated some complements for bolt:"
                                            (show-bolt bolt language-model)
                                            " matching spec:"
                                            (spec-info spec)))

                            (take max-generated-complements (lazy-cat phrasal-complements filtered-lexical-complements)))))))))

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

(defn lightning-bolts [language-model spec depth total-depth
                       & {:keys [max-total-depth]
                          :or {max-total-depth max-total-depth}}]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."
  (log/debug (str "lightning-bolts(depth=" depth
                 "; total-depth=" total-depth
                 "; max-total-depth=" max-total-depth
                 "; spec info:" (spec-info spec) ")"))
  (let [morph (:morph language-model)
        grammar (:grammar language-model)
        index (:index language-model)
        morph (if morph morph (fn [input] (get-in input [:rule] :default-morph-no-rule)))
        depth (if depth depth 0)
        ;; this is the relative depth; that is, the depth from the top of the current lightning bolt.
        ;; total-depth, on the other hand, is the depth all the way to the top of the entire
        ;; expression, which might involve several parent lightning bolts.
        parents (shuffle (candidate-parents grammar spec))]
    (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
          (mapfn (fn [parent]
                         (if (= false (get-in parent [:head :phrasal] false))
                           (let [candidate-lexemes (get-lex parent :head index)
                                 filter-on-spec {:synsem {:cat (get-in parent [:head :cat] :top)
                                                          :essere (get-in parent [:head :essere] :top)
                                                          :sem (get-in parent [:head :synsem :sem] :top)}}
                                 subset (filter #(not (fail? (unifyc filter-on-spec %)))
                                                candidate-lexemes)]
                             (filter #(not (nil? %))
                                     (do (when (not (empty? subset))
                                           (log/debug (str "adding lexical heads to parent:" (:rule parent)))
                                           
                                           (log/debug (str " with lexemes:" (string/join ";" (sort (map morph subset)))))
                                           (log/debug (str " with spec:" (spec-info spec))))
                                         (if (not (empty? subset))
                                           (over/overh (copy parent) (map copy (shuffle subset)))
                                           []))))))
                       parents)
          phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
          (if (and (< total-depth max-total-depth)
                   (= true (get-in spec [:head :phrasal] true)))
            (mapfn (fn [parent]
                     (over/overh parent
                                 (lightning-bolts language-model (get-in parent [:head])
                                                  (+ 1 depth) (+ 1 total-depth)
                                                  :max-total-depth max-total-depth)))
                   parents)
            (do
              (log/debug (str "hit max-total-depth: " max-total-depth ": will not generate phrasal head children."))
              nil))]
      (if (lexemes-before-phrases total-depth max-total-depth)
        (lazy-cat lexical phrasal)
        (lazy-cat phrasal lexical)))))

(defn candidate-parents [rules spec]
  "find subset of _rules_ for which each member unifies successfully with _spec_"
  (log/trace (str "candidate-parents: spec: " (strip-refs spec)))
  (filter #(not (fail? %))
          (mapfn (fn [rule]
                   (if (and (not (fail? (unifyc (get-in rule [:synsem :cat] :top)
                                                (get-in spec [:synsem :cat] :top))))
                            (not (fail? (unifyc (get-in rule [:synsem :infl] :top)
                                                (get-in spec [:synsem :infl] :top))))
                            (not (fail? (unifyc (get-in rule [:synsem :sem :tense] :top)
                                                (get-in spec [:synsem :sem :tense] :top))))
                            (not (fail? (unifyc (get-in rule [:synsem :modified] :top)
                                                (get-in spec [:synsem :modified] :top)))))
                     (unifyc spec rule)
                     :fail))
                 rules)))

(defn lazy-shuffle [seq]
  (lazy-seq (shuffle seq)))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. (str ": " error-string))))
  #?(:cljs
     (throw (js/Error. error-string))))

(defn current-time []
  #?(:clj (System/currentTimeMillis))
  #?(:cljs (.getTime (js/Date.))))

;; Thanks to http://clojurian.blogspot.com.br/2012/11/beware-of-mapcat.html
(defn lazy-mapcat  [f coll]
  (lazy-seq
   (if (not-empty coll)
     (concat
      (f (first coll))
      (lazy-mapcat f (rest coll))))))

(defn path-to-map [path val]
  (let [feat (first path)]
    (if feat
      {feat (path-to-map (rest path) val)}
      val)))

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
