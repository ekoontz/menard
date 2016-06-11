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
                   (strip-refs spec) " with max-total-depth: " max-total-depth))
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
(declare add-all-comps)
(declare add-all-comps-with-paths)

(defn generate-all [spec language-model total-depth
                    & {:keys [max-total-depth truncate-children]
                       :or {max-total-depth max-total-depth
                            truncate-children true}}]
  (log/trace (str "generate-all: generating from spec with cat "
                  (get-in spec [:synsem :cat])))
  (log/debug (str "generate-all:"
                  (strip-refs spec) "^" total-depth))
  
  (let [total-depth (if total-depth total-depth 0)]
    (->
     (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
     (add-all-comps language-model total-depth)
     (truncate-expressions [[:head][:comp]])
     )))

(defn add-all-comps [bolts language-model total-depth]
  (log/trace (str "add-all-comps with (empty? bolts): " (empty? bolts)))
  (lazy-mapcat
   (fn [bolt]
     (add-all-comps-with-paths [bolt] language-model total-depth
                               (find-comp-paths-in bolt [:head])))
   bolts))

(defn add-all-comps-with-paths [bolts language-model total-depth comp-paths]
  (log/trace (str "add-all-comps-with-paths with (empty? bolts): "
                  (empty? bolts)))
  (if (not (empty? comp-paths))
    (let [path (first comp-paths)]
      (add-all-comps-with-paths
       (lazy-mapcat
        (fn [bolt]
          (log/debug (str "+comps: '" ((:morph language-model) bolt) "'@"
                          "[" (string/join " " path) "]"
                          "^" total-depth))
          (add-complement-to-bolt bolt path
                                  language-model (+ total-depth (count path))
                                  :max-total-depth max-total-depth))
        bolts)
       language-model
       total-depth
       (rest comp-paths)))
    bolts))

(defn find-comp-paths-in [bolt path]
  (if (not (= (get-in bolt path :none) :none))
    (cons (vec (concat (butlast path) [:comp]))
          (find-comp-paths-in bolt (concat path [:head])))))

(defn add-complement-to-bolt [bolt path language-model total-depth
                              & {:keys [max-total-depth]
                                 :or {max-total-depth max-total-depth}}]
  (log/debug (str "add-complement-to-bolt: " (show-bolt bolt path language-model) "@"
                  "[" (string/join " " path) "]" "^" total-depth))
  (let [index (:index language-model)
        lexicon (:lexicon language-model)
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
                    (unify (copy bolt)
                           (path-to-map path
                                        (copy complement))))
                  (let [debug (log/trace (str "add-complement-to-bolt(total-depth=" total-depth
                                              ",path=" path ",bolt=(" (show-bolt bolt path language-model)
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
                          (log/warn (str "failed to generate both phrasal or lexical complements with spec: "
                                         (strip-refs spec)))

                          lexemes-before-phrases
                          (lazy-cat (lazy-shuffle filtered-lexical-complements) phrasal-complements)

                          true
                          (lazy-cat phrasal-complements (lazy-shuffle filtered-lexical-complements))))))))

(defn lightning-bolts [language-model spec depth total-depth
                       & {:keys [max-total-depth]
                          :or {max-total-depth max-total-depth}}]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."
  (log/trace (str "lightning-bolts(depth=" depth "; total-depth=" total-depth "; cat=" (get-in spec [:synsem :cat]) "; spec=" (strip-refs spec) ")"))
  (let [morph (:morph language-model)
        grammar (:grammar language-model)
        index (:index language-model)
        morph (if morph morph (fn [input] (get-in input [:rule] :default-morph-no-rule)))
        depth (if depth depth 0)        
        parents (shuffle (candidate-parents grammar spec))]
    (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
          (lazy-mapcat (fn [parent]
                         (if (= false (get-in parent [:head :phrasal] false))
                           (let [candidate-lexemes (get-lex parent :head index spec)]
                             (filter #(not (nil? %))
                                     (over/overh parent (mapfn copy (shuffle candidate-lexemes)))))))
                       parents)
          phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
          (if (and (< depth max-total-depth)
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

(defn show-bolt [bolt path language-model]
  (if (nil? bolt)
    (exception (str "don't call show-bolt with bolt=null."))
    (let [morph (:morph language-model)]
      (if (nil? morph)
        (exception (str "don't call show-bolt with morph=null."))
        (if (not (empty? path))
          (str "[" (get-in bolt [:rule])
               " '" (morph-with-recovery morph bolt) "'"
               (let [head-bolt (get-in bolt [:head])]
                 (if (not (nil? head-bolt))
                   (let [rest-str (show-bolt (get-in bolt [:head]) (rest path) language-model)]
                     (if (not (nil? rest-str))
                       (str " -> " rest-str)))))
               "]"))))))

