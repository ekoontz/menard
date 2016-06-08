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

(declare add-complement)
(declare exception)
(declare lazy-mapcat)
(declare lexemes-before-phrases)
(declare lightning-bolts)
(declare generate-all)
(declare in-case-of-no-phrasal-complements)
(declare path-to-map)
(declare show-bolt)
(declare truncate)

(defn generate [spec grammar lexicon index morph
                & {:keys [max-total-depth truncate-children]
                   :or {max-total-depth max-total-depth
                        truncate-children true}}]
  (cond (or (vector? spec)
            (seq? spec))
        (let [spec (vec (set spec))]
          (log/debug (str "generating from " (count spec) " spec(s)"))
          (let [expression
                (first (take 1
                             (lazy-mapcat (fn [each-spec]
                                           (log/info (str "generate: generating from spec: "
                                                          each-spec))
                                           (let [expressions
                                                 (generate-all each-spec grammar lexicon index morph 0
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

        (empty? grammar)
        (do
          (log/error (str "grammar is empty."))
          (exception (str "grammar is empty.")))

        true
        (do
          (log/info (str "generate: generating from spec: "
                         (strip-refs spec)))
          (let [expression (first (take 1 (generate-all spec grammar lexicon index morph 0
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
              (log/info (str "generate: no expression could be generated for spec:" (strip-refs spec))))
            expression))))

(declare add-complements-to-bolts)

(defn generate-all [spec grammar lexicon index morph total-depth
                    & {:keys [max-total-depth truncate-children]
                       :or {max-total-depth max-total-depth
                            truncate-children true}}]
  (log/trace (str "generate-all: generating from spec with cat " (get-in spec [:synsem :cat])))
  (log/debug (str "generate-all: generating from spec with spec " (strip-refs spec)))
  
  (let [total-depth (if total-depth total-depth 0)]
    (map (fn [expr]
           (if truncate-children
             (truncate expr [[:head][:comp]])
             ;; else, don't truncate for efficiency (i.e. don't remove :head and :comp)
             expr))
         (-> (lightning-bolts grammar lexicon spec 0 index morph total-depth)
             ;; TODO: allow more than a fixed maximum depth of generation (here, 4 levels from top of tree).
             (add-complements-to-bolts [:head :head :head :comp] grammar lexicon index morph total-depth)
             (add-complements-to-bolts [:head :head :comp] grammar lexicon index morph total-depth)
             (add-complements-to-bolts [:head :comp] grammar lexicon index morph total-depth)
             (add-complements-to-bolts [:comp] grammar lexicon index morph total-depth)))))

(defn add-complements-to-bolts [bolts path grammar lexicon index morph total-depth]
  (lazy-mapcat
   #(if (not (= :none (get-in % path :none)))
      (do
        (log/debug (str "add-complements-to-bolts@" path))
        (add-complement % path :top grammar lexicon index morph 0 (+ total-depth (count path))))
      [%])
   bolts))

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

(defn lightning-bolts [grammar lexicon spec depth index morph total-depth]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."
  (log/trace (str "lightning-bolts(depth=" depth "; total-depth=" total-depth "; cat=" (get-in spec [:synsem :cat]) "; spec=" (strip-refs spec) ")"))
  (let [morph (if morph morph (fn [input] (get-in input [:rule] :default-morph-no-rule)))
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
                                       (lightning-bolts grammar lexicon (get-in parent [:head])
                                                        (+ 1 depth) index morph (+ 1 total-depth))))
                         parents))]
      (if (lexemes-before-phrases total-depth)
        (lazy-cat lexical phrasal)
        (lazy-cat phrasal lexical)))))

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

(defn add-complement [bolt path spec grammar lexicon cache morph depth total-depth]
  (log/debug (str "add-complement: start: " (show-bolt bolt path morph) "@" path))
  (let [input-spec spec
        from-bolt bolt ;; so we can show what (add-complement) did to the input bolt, for logging.
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
                  (let [debug (log/trace (str "add-complement(depth=" depth ",total-depth=" total-depth
                                              ",path=" path ",bolt=(" (show-bolt bolt path morph)
                                              "): calling generate-all(" (strip-refs spec) ");"
                                              "input-spec: " input-spec))
                        phrasal-complements (if (and (> max-total-depth total-depth)
                                                     (= true (get-in spec [:phrasal] true)))
                                              (generate-all spec grammar lexicon index morph (+ depth total-depth)))]
                    (if (lexemes-before-phrases total-depth)
                      (lazy-cat (lazy-shuffle filtered-lexical-complements) phrasal-complements)
                      (lazy-cat phrasal-complements (lazy-shuffle filtered-lexical-complements))))))))

(defn path-to-map [path val]
  (let [feat (first path)]
    (if feat
      {feat (path-to-map (rest path) val)}
      val)))

(defn in-case-of-no-phrasal-complements [bolt path run-time from-bolt complement-candidate-lexemes morph]
  ;; No complements could be added to this bolt: Throw an exception or log/warn. debateable about which to do
  ;; in which circumstances.
  (let [log-limit 1000
        log-fn (fn [message] (log/warn message))
        throw-exception-if-no-complements-found false
        message
        (str " add-complement to " (get-in bolt [:rule]) " at path: " path
             " took " run-time " msec, but found neither phrasal nor lexical complements for "
             "'" (morph from-bolt) "'"
             ". Bolt wants phrasal-wise: " (get-in bolt (concat path [:phrasal]))
             ". Desired complement [:synsem] was: "
             (strip-refs (get-in bolt (concat path [:synsem]))) ". "
             (if (= false (get-in bolt (concat path [:phrasal]) false))
               (str
                (count complement-candidate-lexemes) " lexical complement(s) tried were:"
                " "
                (string/join "," (sort (map morph (take log-limit complement-candidate-lexemes))))
                
                (if (< 0 (- (count complement-candidate-lexemes) log-limit))
                  (str ",.. and "
                       (- (count complement-candidate-lexemes) log-limit) " more."))
                
                ";     with preds:   "
                (string/join "," (map #(get-in % [:synsem :sem :pred]) (take log-limit complement-candidate-lexemes)))
                
                ";     fail-paths:   "
                (string/join ","
                             (map #(if
                                       (or true (not (fail? (unifyc (get-in % [:synsem :sem :pred])
                                                                    (get-in bolt (concat path
                                                                                         [:synsem :sem :pred]))))))
                                     (str "'" (morph %) "':"
                                          (fail-path (strip-refs %)
                                                     (strip-refs (get-in bolt path)))))
                                  (take log-limit complement-candidate-lexemes)))
                
                (if (< 0 (- (count complement-candidate-lexemes) log-limit))
                  (str ",.. and "
                       (- (count complement-candidate-lexemes) log-limit) " more.")))))]
    (log-fn message)
    
    ;; set to true to work on optimizing generation, since this situation of failing to add any
    ;; complements is expensive.
    (if (and throw-exception-if-no-complements-found
             (not (= true (get-in bolt (concat path [:phrasal])))))
      (exception message))))

(defn lexemes-before-phrases [depth]
  "returns true or false: true means generate by adding lexemes first; otherwise, by adding phrases first. Takes depth as an argument, which makes returning true (i.e. lexemes first) increasingly likely as depth increases."
  (if (not randomize-lexemes-before-phrases)
    false
    (if (> max-total-depth 0)
      (let [prob (- 1.0 (/ (- max-total-depth depth) max-total-depth))]
        (> (* 10 prob) (rand-int 10)))
      false)))

(defn show-bolt [bolt path morph]
  (if (nil? bolt)
    (exception (str "don't call show-bolt with bolt=null."))
    (if (not (empty? path))
      (str "[" (get-in bolt [:rule])
           " '" (morph-with-recovery morph bolt) "'"
           (let [head-bolt (get-in bolt [:head])]
             (if (not (nil? head-bolt))
               (let [rest-str (show-bolt (get-in bolt [:head]) (rest path) morph)]
                 (if (not (nil? rest-str))
                   (str " -> " rest-str)))))
           "]"))))
