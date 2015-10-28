(ns babel.forest
  (:refer-clojure :exclude [get-in deref resolve find future parents])
  (:require
   [babel.cache :refer (build-lex-sch-cache get-comp-phrases-of get-head-phrases-of get-lex
                                            get-parent-phrases-for-spec)]
   [babel.over :as over]
   [clojure.core :as core]
   [clojure.set :refer :all]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [dag-unify.core :refer (dissoc-paths get-in fail? fail-path-between lazy-shuffle ref? remove-false 
                                        remove-top-values-log strip-refs show-spec unifyc)]))

(def concurrent false)
(declare path-to-map)
(declare add-complement)
(declare add-all-complements-to-bolts)
(declare add-complements-to-bolts)

(declare lightning-bolt)
(declare generate-all)

(defn wrapped-morph [morph-fn structure]
  (if morph-fn
    (try (let [result (morph-fn structure)]
           result)
         (catch Exception e
           (do
             (log/warn (str "ignoring exception: " e " and returning canonical form instead."))
             (strip-refs (get-in structure [:espanol])))))
    ;; TODO: consider throwing exception here.
    "(morph-fn is null)"))

(defn generate [spec grammar lexicon index morph]
  (first (take 1 (generate-all spec grammar lexicon index morph))))

(defn generate-all-with-model [spec {grammar :grammar
                                     index :index
                                     lexicon :lexicon
                                     morph :morph}]
  (let [index (if (future? index) @index index)
        lexicon (if (future? lexicon) @lexicon lexicon)]
    (log/info (str "using grammar of size: " (.size grammar)))
    (log/info (str "using index of size: " (.size index)))
    (if (seq? spec)
      (map generate-all spec grammar lexicon index morph)
      (generate spec grammar
                (flatten (vals lexicon))
                index
                morph))))

(defn generate-all [spec grammar lexicon index morph]
  (filter #(not (fail? %))
          (cond (and (or (seq? spec)
                         (vector? spec))
                     (not (empty? spec)))
                (lazy-cat (generate-all (first spec) grammar lexicon index morph)
                          (generate-all (rest spec) grammar lexicon index morph))
                true
                (do
                  (log/trace (str "generate-all with semantics: " (show-spec (remove-false (get-in spec [:synsem :sem])))))
                  (log/trace (str "generate-all with spec: " (strip-refs spec)))
                  (log/trace (str "generate-all with pred: " (show-spec (remove-false (get-in spec [:synsem :sem :pred])))))
                  (log/trace (str "generate-all(details): " (show-spec spec)))
                  (-> (lightning-bolt grammar
                                      lexicon
                                      spec 0 index nil morph)
                      ;; TODO: allow more than a fixed maximum depth of generation (here, 4 levels from top of tree).
                      (add-complements-to-bolts [:head :head :head :comp] :top grammar lexicon index morph)
                      (add-complements-to-bolts [:head :head :comp]       :top grammar lexicon index morph)
                      (add-complements-to-bolts [:head :comp]             :top grammar lexicon index morph)
                      (add-complements-to-bolts [:comp]                   :top grammar lexicon index morph))))))
  
;; TODO: add usage of rule-to-lexicon cache (rather than using lexicon directly)
(defn lightning-bolt [grammar lexicon spec & [ depth index parent morph]]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."

  (if (not (empty? (strip-refs spec)))
    (log/debug (str "lighting-bolt@" depth " spec: " (strip-refs spec))))
  (log/trace (str "lighting-bolt@" depth " grammar:" (string/join ", " (map #(get-in % [:rule]) grammar))))
  (let [maxdepth 3 ;; maximum depth of a lightning bolt: H1 -> H2 -> H3 where H3 must be a lexeme, not a phrase.
        index (if (future? index) @index index)
        lexicon (if (future? lexicon) @lexicon lexicon)
        depth (if depth depth 0)
        ;; TODO: unifyc is expensive: factor out into a let.
        candidate-parents (lazy-shuffle (filter #(not (fail? %))
                                                (map (fn [rule]
                                                       (unifyc spec rule))
                                                     (if parent (get-head-phrases-of parent index)
                                                         grammar))))
        debug (if (not (empty? candidate-parents))
                (log/debug (str "candidate-parents: " (string/join "," (map #(get-in % [:rule])
                                                                            candidate-parents)))))]


    (if (> depth 5)
      (throw (Exception. (str "DEPTH IS GREATER THAN 5; HOW DID YOU END UP IN THIS TERRIBLE SITUATION? LOOK AT THE STACK. I'M OUTTA HERE."))))
    (if (seq candidate-parents)
      (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
            (mapcat (fn [parent]
                      (let [candidate-lexemes (get-lex parent :head index spec)]
                        (log/trace
                         (str "candidate head lexeme for parent: "
                              (get-in parent [:rule]) ": "
                              (string/join ","
                                           (map (fn [lexeme]
                                                  (morph lexeme))
                                                candidate-lexemes))))
                        (let [result (over/overh parent (lazy-shuffle (get-lex parent :head index spec)) morph)]
                          (log/trace
                           (str "parent: " (get-in parent [:rule]) " with lexemes: "
                                (string/join ","
                                             (map morph
                                                  result))))
                          (if (empty? result)
                            (log/warn (str "failed to attach any lexemes to: " (get-in parent [:rule])))
                            (log/debug (str "results of attaching lexemes to: " (get-in parent [:rule]) ":"
                                            (string/join ","
                                                         (map #(wrapped-morph morph %1)
                                                              result)))))
                          result)))
                    candidate-parents)

            ;; TODO: throw exception if (get-in parent [:head]) is null.

            phrasal-children-candidates
            (if (< depth maxdepth)
              (lightning-bolt grammar lexicon
                              (get-in parent [:head])
                              (+ 1 depth)
                              index parent morph)
              (log/debug (str "not trying to add phrases as child because depth is greater than maxdepth:" maxdepth)))

            debug (if (empty? phrasal-children-candidates)
                    (log/warn (str "NO PHRASAL CANDIDATES FOUND FOR candidate-parents: "
                                   (string/join ","
                                                (map #(get-in % [:rule]) candidate-parents)))))

             phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
             ;; recursively call lightning-bolt with (+ 1 depth).
             (if (< depth maxdepth)
               (mapcat (fn [parent]
                        (log/debug (str "calling over/overh with parent: " (:rule parent)))
                        (let [phrasal-children
                              (lightning-bolt grammar lexicon
                                              (get-in parent [:head])
                                              (+ 1 depth)
                                              index parent morph)]
                          (log/debug (str "calling overh with parent: [" (:rule parent) "]" "'" (morph parent) "'"
                                          " and children: "
                                          (if phrasal-children
                                            (str "(" (.size phrasal-children) ")")
                                            "(nil)")
                                          (string/join ","
                                           (map (fn [child]
                                                  (str "[" (:rule child) "]"
                                                       "'" (morph child) "'"
                                                       ))
                                                phrasal-children))))
                          (over/overh parent phrasal-children morph)))
                       candidate-parents))]
        (if (= (rand-int 2) 0)
          (lazy-cat lexical phrasal)
          (lazy-cat phrasal lexical))))))

(defn add-complement [bolt path spec grammar lexicon cache morph]
  (let [input-spec spec
        cache (if (future? cache) @cache cache)
        from-bolt bolt ;; so we can show what (add-complement) did to the input bolt, for logging.
        bolt-spec (get-in bolt path :no-path)
        spec (unifyc spec bolt-spec)
        lexicon (if (future? lexicon) @lexicon lexicon)]
    (log/debug (str "add-complement to bolt with bolt:["
                    (if (map? bolt) (get-in bolt [:rule]))
                    " '" (wrapped-morph morph bolt) "'"
                    "]"))
    (log/debug (str "add-complement to bolt with path:" path))

    (if (not (= bolt-spec :no-path)) ;; check if this bolt has this path in it.
      (let [immediate-parent (get-in bolt (butlast path))
            start-time (System/currentTimeMillis)
            cached (if cache
                     (do
                       (let [result (get-lex immediate-parent :comp cache spec)]
                         (if (not (nil? result))
                           (log/debug (str " cached lexical subset ratio: " 
                                           (string/replace (str (/ (* 1.0 (/ (.size lexicon) (.size result)))))
                                                           #"\.(..).*"
                                                           (fn [[_ two-digits]] (str "." two-digits))))))
                         result))
                     (do (log/warn (str "no cache: will go through entire lexicon."))
                         nil))
            complement-candidate-lexemes (if cached cached (flatten (vals lexicon)))]
        (let [semantics (get-in spec [:synsem :sem])]
          (if (not (nil? semantics))
            (if (not (nil? semantics)) (log/trace (str "  with semantics:" (strip-refs semantics))))))
        (log/trace (str " immediate parent:" (get-in immediate-parent [:rule])))
        (if (map? complement-candidate-lexemes)
          (log/error (str "complement-candidate-lexemes is unexpectedly a map with keys:" 
                          ( keys complement-candidate-lexemes))))
        (let [shuffled-candidate-lexical-complements (lazy-shuffle complement-candidate-lexemes)
              return-val
              (filter (fn [result]
                        (not (fail? result)))
                      (map (fn [complement]
                             (let [debug (log/trace (str "Trying complement:" (morph complement)))
                                   result
                                   (unifyc bolt
                                           (path-to-map path
                                                        complement))
                                   is-fail? (fail? result)]
                               (if is-fail?
                                 (do
                                   (log/trace (str "fail-path-between:" (fail-path-between (strip-refs (get-in bolt path))
                                                                                           (strip-refs complement))))))
                               (if is-fail? :fail result)))
                     
                           ;; lazy-sequence of phrasal complements to pass one-by-one to the above (map)'s function.
                           (let [phrasal-complements (generate-all spec grammar lexicon cache morph)]
                             (if (= (rand-int 2) 0)
                               (lazy-cat shuffled-candidate-lexical-complements phrasal-complements)
                               (lazy-cat phrasal-complements shuffled-candidate-lexical-complements)))))]
          (let [run-time (- (System/currentTimeMillis) start-time)]
            (if (empty? (seq return-val))

              ;; else, no complements could be added to this bolt.
              (do
                (log/warn (str " add-complement took " run-time " msec, but found no lexical complements for "
                               (try
                                 (morph from-bolt)
                                 (catch Exception e
                                   "(exception caught)"))
                               "(rule: " (:rule from-bolt) ")"
                               "; head rule:" (:rule (get-in from-bolt [:head]))
                               ". Complements tried were: " (morph complement-candidate-lexemes)))
                  
                ;; TODO: show warn about not finding any phrasal complements, as well as not finding any lexical complements.
                (vec (map (fn [lexeme]
                            (log/debug (str " path in bolt: " path))
                            (log/debug (str " cat@" path " = " (strip-refs (get-in bolt (concat path [:synsem :cat])))))
                            (log/trace (str " sem@" path " = " (strip-refs (get-in bolt (concat path [:synsem :sem])))))
                            (log/debug (str " pred@" path " = " (strip-refs (get-in bolt (concat path [:synsem :sem :pred])))))
                            (log/debug (str " agr@" path " = " (strip-refs (get-in bolt (concat path [:synsem :agr]))))))
                          complement-candidate-lexemes))))
            (do (log/debug (str "add-complement after adding complement: "
                                (try
                                  (morph return-val)
                                  (catch Exception e
                                    "(exception caught)"))))
                return-val))))

      ;; path doesn't exist in bolt: simply return the bolt unmodified.
      (do
        (list bolt)))))

(defn add-complements-to-bolts [bolts path spec grammar lexicon cache morph]
  (if (seq bolts)
    (lazy-cat (if (not (fail? (first bolts)))
                (add-complement (first bolts) path spec grammar lexicon cache morph))
              (add-complements-to-bolts (rest bolts) path spec grammar lexicon cache morph))))

(defn path-to-map [path val]
  (let [feat (first path)]
    (if feat
      {feat (path-to-map (rest path) val)}
      val)))
