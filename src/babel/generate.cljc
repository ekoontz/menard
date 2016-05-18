(ns babel.generate
  (:refer-clojure :exclude [get-in deref resolve find parents])
  (:require
   [babel.cache :refer [check-index get-head-phrases-of get-lex]]
   [babel.over :as over]
   [babel.stringutils :refer [show-as-tree]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.string :as string]
   [dag_unify.core :refer (copy dissoc-paths get-in fail? fail-path-between lazy-shuffle
                                        ref? remove-false remove-top-values-log
                                        strip-refs show-spec unify unifyc)]))
;; during generation, will not search deeper than this:
(def ^:const max-total-depth 4)

(declare add-complement)
(declare lexemes-before-phrases)
(declare lightning-bolt)
(declare generate-all)
(declare in-case-of-no-phrasal-complements)
(declare path-to-map)
(declare show-bolt)

(defn exception [error-string]
  #?(:clj
     (throw (Exception. (str ": " error-string))))
  #?(:cljs
     (throw (js/Error. error-string))))

(defn current-time []
  #?(:clj (System/currentTimeMillis))
  #?(:cljs (.getTime (js/Date.))))

(defn generate [spec grammar lexicon index morph]
  (if (empty? grammar)
    (do
      (log/error (str "grammar is empty."))
      (exception (str "grammar is empty."))))
  (first (take 1 (generate-all spec grammar lexicon index morph))))

(defn generate-all-with-model [spec {grammar :grammar
                                     index :index
                                     lexicon :lexicon
                                     morph :morph}]
  (let []
    (log/info (str "using grammar of size: " (count grammar)))
    (log/info (str "using index of size: " (count index)))
    (if (seq? spec)
      #?(:clj (map generate-all spec grammar lexicon index morph))
      #?(:cljs (map generate-all spec grammar lexicon index morph))
      (generate spec grammar
                (flatten (vals lexicon))
                index
                morph))))

(defn generate-all [spec grammar lexicon index morph & [total-depth]]
  (let [total-depth (if total-depth total-depth 0)
        add-complements-to-bolts
        (fn [bolts path]
          (reduce concat (map #(if (not (= :none (get-in % path :none)))
                                  (add-complement % path :top grammar lexicon index morph 0 (+ total-depth (count path)))
                                  [%])
                               bolts)))]
    (-> (lightning-bolt (lazy-shuffle grammar)
                        lexicon
                        spec 0 index morph total-depth)
        ;; TODO: allow more than a fixed maximum depth of generation (here, 4 levels from top of tree).
        (add-complements-to-bolts [:head :head :head :comp] )
        (add-complements-to-bolts [:head :head :comp])
        (add-complements-to-bolts [:head :comp])
        (add-complements-to-bolts [:comp]))))

(defn lightning-bolt [grammar lexicon spec depth index morph total-depth]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."
  (if (or (vector? spec) (seq? spec))
    (reduce concat (map (fn [each-spec]
                           (lightning-bolt grammar lexicon each-spec depth index morph total-depth))
                         spec))
    (do
      (log/trace (str "lightning-bolt(depth=" depth ", total-depth=" total-depth "): sem:" (strip-refs (get-in spec [:synsem :sem]))))
      (let [morph (if morph morph (fn [input] (get-in input [:rule] :default-morph-no-rule)))
            depth (if depth depth 0)        
            parents (filter #(not (fail? %)) (map (fn [rule] (unifyc spec rule)) grammar))]
        (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
              (reduce concat
                      (map (fn [parent]
                              (if (= false (get-in parent [:head :phrasal] false))
                                (let [candidate-lexemes (get-lex parent :head index spec)]
                                  (over/overh parent
                                              (map copy (take 1 (lazy-shuffle candidate-lexemes)))))))
                            parents))
              phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
              (if (< depth max-total-depth)
                (reduce concat (map (fn [parent]
                                       (over/overh parent (lightning-bolt grammar lexicon (get-in parent [:head])
                                                                          (+ 1 depth) index morph (+ 1 total-depth))))
                                     parents)))]
          (if (lexemes-before-phrases total-depth)
            (take 1 (lazy-cat lexical phrasal))
            (take 1 (lazy-cat phrasal lexical))))))))

(defn add-complement [bolt path spec grammar lexicon cache morph depth total-depth]
  (log/info (str "add-complement: " (show-bolt bolt path morph)))
  (let [input-spec spec
        from-bolt bolt ;; so we can show what (add-complement) did to the input bolt, for logging.
        spec (unifyc spec (get-in bolt path))
        immediate-parent (get-in bolt (butlast path))
        start-time (current-time)
        cached (if cache
                 (get-lex immediate-parent :comp cache spec)
                 (do (log/warn (str "no cache: will go through entire lexicon to find candidate complements."))
                     (reduce concat (vals lexicon))))
        complement-candidate-lexemes (if (not (= true
                                                 (get-in bolt (concat path [:phrasal]))))
                                       (if cached cached (flatten (vals lexicon))))
        complement-pre-check (fn [child parent path-to-child]
                               (let [child-in-bolt (get-in bolt path-to-child)]
                                 (and (not (fail?
                                            (unifyc (get-in child [:synsem] :top)
                                                    (get-in child-in-bolt [:synsem] :top)))))))
        filtered-lexical-complements (filter (fn [lexeme]
                                               (complement-pre-check lexeme bolt path))
                                             complement-candidate-lexemes)
        shuffled-candidate-lexical-complements (lazy-shuffle filtered-lexical-complements)]
    (filter (fn [complement]
              (if (fail? complement)
                (do
                  (log/trace (str "add-complement(depth=" depth ",total-depth=" total-depth
                                  ",path=" path ",bolt=(" (show-bolt bolt path morph) ") FAILED:"
                                  "'" (morph complement) "'"))
                  
                  false)
                (do
                  (log/trace (str "add-complement(depth=" depth ",total-depth=" total-depth
                                  ",path=" path ",bolt=(" (show-bolt bolt path morph) ")=>"
                                  "'" (morph complement) "'"))
                  true)))
            (take 1 (map (fn [complement]
                   (unify (copy bolt)
                          (path-to-map path
                                       (copy complement))))
                 (let [debug (log/trace (str "add-complement(depth=" depth ",total-depth=" total-depth
                                             ",path=" path ",bolt=(" (show-bolt bolt path morph)
                                             "): calling generate-all(" (strip-refs spec) ");"
                                             "input-spec: " input-spec))
                       phrasal-complements (if (> max-total-depth total-depth)
                                             (generate-all spec grammar lexicon cache morph (+ depth total-depth)))]
                   (if (lexemes-before-phrases total-depth)
                     (take 1 (lazy-cat shuffled-candidate-lexical-complements phrasal-complements))
                     (take 1 (lazy-cat phrasal-complements shuffled-candidate-lexical-complements)))))))))

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
                                          (fail-path-between (strip-refs %)
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
  ;; takes depth as an argument; make phrases decreasingly likely as depth increases.
  (let [result (> (rand-int (+ 1 depth)) 0)]
    (log/trace (str "lexemes-before-phrases: depth=" depth " => " result))
    result))

(defn show-bolt [bolt path morph]
  (if (not (empty? path))
    (str (get-in bolt [:rule])
         " '" (morph bolt) "' "
         (let [rest-str (show-bolt (get-in bolt [:head]) (rest path) morph)]
           (if (not (nil? rest-str))
             (str " -> " rest-str)))))) 
