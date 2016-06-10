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
                                        strip-refs show-spec unify unifyc

                                        ;; temporary
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

(defn generate [spec language-model
                & {:keys [max-total-depth truncate-children]
                   :or {max-total-depth max-total-depth
                        truncate-children true}}]
  (let [morph (:morph language-model)]
    (cond (or (vector? spec)
              (seq? spec))
          (let [spec (vec (set spec))]
            (log/debug (str "generating from " (count spec) " spec(s)"))
            (let [expression
                  (first (take 1
                               (lazy-mapcat (fn [each-spec]
                                              (log/info (str "generate: generating from spec: "
                                                             (strip-refs each-spec) " with max-total-depth: " max-total-depth))
                                              (let [expressions
                                                    (generate-all each-spec language-model 0
                                                                  :max-total-depth max-total-depth
                                                                  :truncate-children truncate-children)]
                                                expressions))
                                            spec)))]
              ;; TODO: show time information
              (if expression
                (log/info (str "generate: generated "
                               "'" (morph expression) "'"
                               " from " (count spec) " spec(s)"))
                (log/warn (str "generate: no expression could be generated for any of the "
                               " from " (count spec) " spec(s)")))
              expression))
          true
          (do
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
              expression)))))

(declare add-complements-to-bolts)
(declare add-all-comps)
(declare candidate-parents)
(declare lazy-shuffle)

(defn generate-all [spec language-model total-depth
                    & {:keys [max-total-depth truncate-children]
                       :or {max-total-depth max-total-depth
                            truncate-children true}}]
  (log/trace (str "generate-all: generating from spec with cat "
                  (get-in spec [:synsem :cat])))
  (log/debug (str "generate-all: generating from spec with spec "
                  (strip-refs spec) "; max total depth: " max-total-depth))
  
  (let [total-depth (if total-depth total-depth 0)
        expressions
        (->
         (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
         (add-all-comps (take max-total-depth (repeatedly (fn [] :head)))
                        language-model total-depth))]
    (map (fn [expr]
           (if truncate-children
             (truncate expr [[:head][:comp]])
             ;; else, don't truncate for efficiency (i.e. don't remove :head and :comp).
             expr))
         expressions)))

(defn add-all-comps [bolts path language-model total-depth]
  (if (not (empty? path))
    (add-all-comps 
     (add-complements-to-bolts bolts (concat path [:comp])
                               language-model total-depth
                               :max-total-depth max-total-depth)
     (rest path) language-model total-depth)
    (add-complements-to-bolts bolts [:comp]
                              language-model total-depth)))

(defn add-complements-to-bolts [bolts path language-model total-depth
                                & {:keys [max-total-depth]
                                   :or {max-total-depth max-total-depth}}]
  (lazy-mapcat
   (fn [bolt]
     (if (not (= :none (get-in bolt path :none)))
       (add-complement-to-bolt bolt path :top language-model 0 (+ total-depth (count path))
                       :max-total-depth max-total-depth)
       (do
         (log/warn (str "bolt has no path: " (string/join " " path)))
         [bolt])))
   bolts))

(defn add-complement-to-bolt [bolt path spec language-model depth total-depth
                      & {:keys [max-total-depth]
                         :or {max-total-depth max-total-depth}}]
  (log/debug (str "add-complement-to-bolt: start: " (show-bolt bolt path language-model) "@"
                  (string/join " " path)))
  (let [index (:index language-model)
        lexicon (:lexicon language-model)
        input-spec spec
        from-bolt bolt ;; so we can show what (add-complement-to-bolt) did to the input bolt, for logging.
        spec (unifyc spec (get-in bolt path))
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
                  (let [debug (log/trace (str "add-complement-to-bolt(depth=" depth ",total-depth=" total-depth
                                              ",path=" path ",bolt=(" (show-bolt bolt path language-model)
                                              "): calling generate-all(" (strip-refs spec) ");"
                                              "input-spec: " input-spec))
                        phrasal-complements (if (and (> max-total-depth total-depth)
                                                     (= true (get-in spec [:phrasal] true)))
                                              (generate-all spec language-model (+ depth total-depth)
                                                            :max-total-depth max-total-depth))]
                    (if (lexemes-before-phrases total-depth)
                      (lazy-cat (lazy-shuffle filtered-lexical-complements) phrasal-complements)
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
  (let [morph (:morph language-model)]
    (if (nil? bolt)
      (exception (str "don't call show-bolt with bolt=null."))
      (if (not (empty? path))
        (str "[" (get-in bolt [:rule])
             " '" (morph-with-recovery morph bolt) "'"
             (let [head-bolt (get-in bolt [:head])]
               (if (not (nil? head-bolt))
                 (let [rest-str (show-bolt (get-in bolt [:head]) (rest path) language-model)]
                 (if (not (nil? rest-str))
                   (str " -> " rest-str)))))
             "]")))))
