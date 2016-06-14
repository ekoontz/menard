(ns babel.generate
  (:refer-clojure :exclude [get-in deref resolve find parents])
  (:require
   [babel.cache :refer [get-lex]]
   [babel.over :as over]
   [babel.stringutils :refer [show-as-tree]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.string :as string]
   [dag_unify.core :refer (copy dissoc-paths get-in fail? fail-path
                                        ref? remove-false remove-top-values-log
                                        strip-refs unify unifyc

                                        ;; temporary: until we move (truncate) from here to dag_unify.
                                        deserialize serialize
                                        )]))
                                        
;; during generation, will not decend deeper than this when creating a tree:
;; should also be possible to override per-language.
(def ^:const max-total-depth 20)

;; use map or pmap.
(def ^:const mapfn map)

(def ^:const randomize-lexemes-before-phrases true)

(declare add-complement-to-bolt)
(declare exception)
(declare lazy-mapcat)
(declare lexemes-before-phrases)
(declare lightning-bolts)
(declare generate-all)
(declare path-to-map)
(declare show-bolt)
(declare spec-info)
(declare truncate)
(declare truncate-expressions)

;; {:synsem {:cat :noun,
;;           :pronoun true,
;;           :sem {:pred :cat}}

(defn generate [spec language-model
                & {:keys [max-total-depth truncate-children]
                   :or {max-total-depth max-total-depth
                        truncate-children true}}]
  (let [morph (:morph language-model)]
    (log/info (str "generate: generating from spec: "
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
  
  (let [total-depth (if total-depth total-depth 0)]
    (if truncate-children
      (->
       (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
       (add-all-comps language-model total-depth true max-total-depth)
       (truncate-expressions [[:head]]))
      (->
       (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
       (add-all-comps language-model total-depth false max-total-depth)))))

(defn add-all-comps [bolts language-model total-depth truncate-children max-total-depth]
  (log/trace (str "add-all-comps with (empty? bolts): " (empty? bolts)))
  (lazy-mapcat
   (fn [bolt]
     (log/debug (str "add-all-comps: adding comps to bolt: " (show-bolt bolt language-model)))
     (add-all-comps-with-paths [bolt] language-model total-depth
                               (find-comp-paths-in (bolt-depth bolt))
                               truncate-children max-total-depth))
   bolts))

(defn add-all-comps-with-paths [bolts language-model total-depth comp-paths truncate-children max-total-depth]
  (log/trace (str "add-all-comps-with-paths with (empty? bolts): "
                  (empty? bolts)))
  (if (not (empty? comp-paths))
    (let [path (first comp-paths)]
      (add-all-comps-with-paths
       (lazy-mapcat
        (fn [bolt]
          (log/trace (str "add-all-comps-with-paths: " (show-bolt bolt language-model)
                          "@[" (string/join " " path) "]" "^" total-depth))
          (add-complement-to-bolt bolt path
                                  language-model (+ total-depth (count path))
                                  :max-total-depth max-total-depth
                                  :truncate-children truncate-children))
        bolts)
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
                         (get-lex immediate-parent :comp index spec))]
            (if indexed indexed
                (flatten (vals lexicon)))))
        
        complement-pre-check (fn [child parent path-to-child]
                               (let [child-in-bolt (get-in bolt path-to-child)]
                                 (not (fail?
                                       (unifyc (get-in child [:synsem] :top)
                                               (get-in child-in-bolt [:synsem] :top))))))

        filtered-lexical-complements (filter (fn [lexeme]
                                               (complement-pre-check lexeme bolt path))
                                             complement-candidate-lexemes)]
    (filter #(not (fail? %))
            (mapfn (fn [complement]
                     (let [unified
                           (unify (copy bolt)
                                  (path-to-map path
                                               (copy complement)))]
                       (if truncate-children
                         (truncate unified [path])
                         unified)))
                  (let [debug (log/trace (str "add-complement-to-bolt(total-depth=" total-depth
                                              ",path=" path ",bolt=(" (show-bolt bolt language-model)
                                              "): calling generate-all(" (strip-refs spec) ");"
                                              "spec: " spec))
                        phrasal-complements (if (and (> max-total-depth total-depth)
                                                     (= true (get-in spec [:phrasal] true)))
                                              (generate-all spec language-model (+ (count path) total-depth)
                                                            :max-total-depth max-total-depth))
                        lexemes-before-phrases (lexemes-before-phrases total-depth)]
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
                          (log/warn (str "add-complement-to-bolt: could generate neither phrasal nor lexical complements for bolt:" (show-bolt bolt language-model) "; immediate parent: "
                                          (get-in bolt (concat (butlast path) [:rule]) :norule) " "
                                          "while trying to create a complement: " (spec-info spec)))

                          lexemes-before-phrases
                          (lazy-cat (lazy-shuffle filtered-lexical-complements) phrasal-complements)

                          true
                          (lazy-cat phrasal-complements (lazy-shuffle filtered-lexical-complements))))))))

(defn spec-info [spec]
  "give a human-readable summary of _spec_."
  (merge
   (if-let [cat (get-in spec [:synsem :cat] :top)]
     {:cat cat})
   (if-let [pred (get-in spec [:synsem :sem :pred] :top)]
     {:pred pred})
   (if-let [subcat1 (if (not (empty? (get-in spec [:synsem :subcat])))
                      (get-in spec [:synsem :subcat :1 :cat]))]
     {:subcat1 subcat1})))

(defn lightning-bolts [language-model spec depth total-depth
                       & {:keys [max-total-depth]
                          :or {max-total-depth max-total-depth}}]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."
  (log/info (str "lightning-bolts(depth=" depth
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
          (lazy-mapcat (fn [parent]
                         (if (= false (get-in parent [:head :phrasal] false))
                           (let [candidate-lexemes (get-lex parent :head index spec)]
                             (filter #(not (nil? %))
                                     (over/overh parent (mapfn copy (shuffle candidate-lexemes)))))))
                       parents)
          phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
          (if (and (< total-depth max-total-depth)
                   (= true (get-in spec [:head :phrasal] true)))
            (lazy-mapcat (fn [parent]
                           (over/overh parent
                                       (lightning-bolts language-model (get-in parent [:head])
                                                        (+ 1 depth) (+ 1 total-depth)
                                                        :max-total-depth max-total-depth)))
                         parents)
            (do
              (log/info (str "hit max-total-depth: " max-total-depth ": will not generate phrasal head children."))
              nil))]
      (if (lexemes-before-phrases total-depth)
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

(declare morph-with-recovery)

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

(defn try-hard-to [function]
  "try 100 times to do (function), where function presumable has some randomness that causes it to return nil. Ignore such nils and keep trying."
  (first
   (take
    1
    (filter
     #(not (nil? %))
     (take 100
           (repeatedly function))))))

(defn subpath? [path1 path2]
  "return true if path1 is subpath of path2."
  (if (empty? path1)
    true
    (if (= (first path1) (first path2))
      (subpath? (rest path1)
                (rest path2))
      false)))

(defn truncate [input truncate-paths]
  (let [serialized (if (:serialized input)
                     (:serialized input)
                     (serialize input))
        paths-and-vals (rest serialized)
        path-sets (mapfn first paths-and-vals)
        path-vals (mapfn second paths-and-vals)
        truncated-path-sets (mapfn
                             (fn [path-set] 
                               (filter (fn [path] 
                                         (not (some (fn [truncate-path]
                                                      (subpath? truncate-path path))
                                                    truncate-paths)))
                                       path-set))
                             path-sets)
        skeleton (first serialized)
        truncated-skeleton (dissoc-paths skeleton truncate-paths)
        truncated-serialized
        (cons truncated-skeleton
              (zipmap truncated-path-sets
                      path-vals))]
    (deserialize truncated-serialized)))

(defn truncate-expressions [expressions truncate-paths]
  (map #(truncate % truncate-paths)
       expressions))

;; Thanks to http://clojurian.blogspot.com.br/2012/11/beware-of-mapcat.html
(defn lazy-mapcat  [f coll]
  (lazy-seq
   (if (not-empty coll)
     (concat
      (f (first coll))
      (lazy-mapcat f (rest coll))))))


(defn morph-with-recovery [morph-fn input]
  (if (nil? input)
    (exception (str "don't call morph-with-recovery with input=nil.")))
  (if (nil? morph-fn)
    (exception (str "don't call morph-with-recovery with morph-fn=nil.")))
  (let [result (morph-fn input)
        result (if (or (nil? result)
                       (= "" result))
                 (get-in input [:english :english] "")
                 result)
        result (if (or (nil? result)
                       (= "" result))
                 (get-in input [:english] "")
                 result)
        result (if (or (nil? result)
                       (= "" result))
                 (get-in input [:rule] "")
                 result)
        result (if (or (nil? result)
                       (= "" result))
                 (exception
                  (str "r5: " input "/" (nil? input)))
                 result)]
    result))


(defn path-to-map [path val]
  (let [feat (first path)]
    (if feat
      {feat (path-to-map (rest path) val)}
      val)))

(defn lexemes-before-phrases [depth]
  "returns true or false: true means generate by adding lexemes first; otherwise, by adding phrases first. Takes depth as an argument, which makes returning true (i.e. lexemes first) increasingly likely as depth increases."
  (if (not randomize-lexemes-before-phrases)
    false
    (if (> max-total-depth 0)
      (let [prob (- 1.0 (/ (- max-total-depth depth) max-total-depth))]
        (> (* 10 prob) (rand-int 10)))
      false)))

(defn show-bolt [bolt language-model]
  (if (nil? bolt)
    (exception (str "don't call show-bolt with bolt=null."))
    (let [morph (:morph language-model)]
      (if (nil? morph)
        (exception (str "don't call show-bolt with morph=null."))
        (str "[" (get-in bolt [:rule])
             " '" (morph-with-recovery morph bolt) "'"
             (let [head-bolt (get-in bolt [:head])]
               (if (not (nil? head-bolt))
                 (let [rest-str (show-bolt (get-in bolt [:head]) language-model)]
                   (if (not (nil? rest-str))
                     (str " -> " rest-str)))))
             "]")))))

