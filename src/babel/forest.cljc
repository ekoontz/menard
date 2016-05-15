(ns babel.forest
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
(def concurrent false)
(declare path-to-map)
(declare add-complement)
(declare add-all-complements-to-bolts)
(declare add-complements-to-bolts)

(declare lightning-bolt)
(declare generate-all)

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
      #?(:clj (pmap generate-all spec grammar lexicon index morph))
      #?(:cljs (map generate-all spec grammar lexicon index morph))
      (generate spec grammar
                (flatten (vals lexicon))
                index
                morph))))

(defn generate-all [spec grammar lexicon index morph]
  (if (empty? grammar)
    (do
      (log/error (str "grammar is empty."))
      (exception (str "grammar is empty."))))

  (filter #(not (fail? %))
          (cond (and (or (seq? spec)
                         (vector? spec))
                     (not (empty? spec)))
                (lazy-cat (generate-all (first spec) grammar lexicon index morph)
                          (generate-all (rest spec) grammar lexicon index morph))
                true
                (do
                  (log/debug (str "generate-all with semantics: " (show-spec (remove-false (get-in spec [:synsem :sem])))))
                  (log/debug (str "generate-all with spec: " (strip-refs spec)))
                  (log/debug (str "generate-all with pred: " (show-spec (remove-false (get-in spec [:synsem :sem :pred])))))
                  (log/debug (str "generate-all(details): " (show-spec spec)))
                  (let [lb (lightning-bolt (lazy-shuffle grammar)
                                           lexicon
                                           spec 0 index nil morph)]
                    (-> lb
                        ;; TODO: allow more than a fixed maximum depth of generation (here, 4 levels from top of tree).
                        (add-complements-to-bolts [:head :head :head :comp] :top grammar lexicon index morph)
                        (add-complements-to-bolts [:head :head :comp]       :top grammar lexicon index morph)
                        (add-complements-to-bolts [:head :comp]             :top grammar lexicon index morph)
                        (add-complements-to-bolts [:comp]                   :top grammar lexicon index morph)))))))

(defn lexemes-before-phrases []
  ;; TODO: take depth as an argument; make phrases decreasingly likely as depth increases.
  (let [result (= (rand-int 3) 0)]
    (log/debug (str "lexemes-before-phrases => " result))
    result))

;; TODO: add usage of rule-to-lexicon cache (rather than using lexicon directly)
(defn lightning-bolt [grammar lexicon spec & [ depth index parent morph]]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."
  (if (empty? grammar)
    (do
      (log/error (str "grammar is empty."))
      (exception (str "grammar is empty."))))
  
  (if (and (not (= :fail spec))
           (not (= :top spec))
           (not (empty? (strip-refs spec))))
    (do
      (log/debug (str "lightning-bolt@" depth))
      (log/trace (str "lighting-bolt spec: " (strip-refs spec) "; parent: " (if parent
                                                                              (:rule parent))))))
  (if (not parent)
    (log/debug (str "no parent for lightning-bolt@" depth " with spec: " (strip-refs spec))))
  (let [maxdepth 3 ;; maximum depth of a lightning bolt: H1 -> H2 -> H3 where H3 must be a lexeme, not a phrase.
        debug (log/trace (str "lightning-bolt@" depth " grammar:" (string/join ", " (map #(get-in % [:rule]) grammar))))
        depth (if depth depth 0)        
        debug (log/debug (str "looking for candidate head phrases with parent: " (:rule parent)))

        candidate-heads (filter #(not (fail? %))
                                  (map (fn [rule]
                                         (do (log/trace (str "testing rule: " (:rule rule)
                                                             "with cat:" (get-in rule [:synsem :cat])))
                                             (unifyc spec rule)))
                                       (if parent
                                         (let [hp (get-head-phrases-of parent index)]
                                           (log/debug (str "getting head-phrases: hp count:" (count hp)))
                                           hp)
                                         (do
                                           (log/debug (str "not getting head phrases: using grammar"))
                                           grammar))))

        debug (log/trace (str "done looking for candidate head phrases: "
                              (if (empty? candidate-heads)
                                "no candidate heads found."
                                (str "one or more candidate heads found; first: " (:rule (first candidate-heads))))))
        
        debug (if (not (empty? candidate-heads))
                (log/debug (str "candidate-heads: " (string/join "," (map #(get-in % [:rule])
                                                                            candidate-heads))))
                (log/trace (str "no candidate head phrases for spec: " (strip-refs spec))))]
    ;; TODO: remove or parameterize this hard-coded value.
    (if (> depth 5)
      (throw (exception (str "depth is greater than 5: should never happen."))))

    (if (seq candidate-heads)
      (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
            (mapcat (fn [parent]
                      (if (= false (get-in parent [:head :phrasal] false))
                        (let [candidate-lexemes (get-lex parent :head index spec)]
                          (log/debug (str "candidate lexemes for: " (get-in parent [:rule]) ":" (count candidate-lexemes)))
                          (if (some fail? candidate-lexemes)
                            (throw (exception (str "some candidate lexeme was fail?=true!"))))
                          (log/debug
                           (str "candidate head lexemes for parent phrase "
                                (get-in parent [:rule]) ": "
                                (string/join ","
                                             (map (fn [lexeme]
                                                    (morph lexeme))
                                                  candidate-lexemes))))
                          (let [result
                                (if (empty? candidate-lexemes)
                                  (if (= false (get-in parent [:head :phrasal] true))
                                    (log/warn (str "no head lexemes found for parent: " (:rule parent))))
                                  (over/overh parent
                                              (map copy (lazy-shuffle candidate-lexemes))
                                              morph))]
                            (if (not (empty? result))
                              (log/debug (str "successful results of attaching head lexemes to: " (get-in parent [:rule]) ":"
                                              (string/join ","
                                                           (map #(morph %1)
                                                                result))))
                              (if (not (empty? candidate-lexemes))
                                (log/warn (str "all " (count candidate-lexemes) " candidate lexeme(s):"
                                               (string/join ","
                                                            (sort (map morph candidate-lexemes)))
                                               " failed for parent: " (get-in parent [:rule]) " with spec: " (strip-refs spec)))))
                            result))))
                    candidate-heads)

            ;; TODO: throw exception if (get-in parent [:head]) is null.
            phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
            ;; recursively call lightning-bolt with (+ 1 depth).
            (if (< depth maxdepth)
              (mapcat (fn [parent]
                        (log/debug (str "calling over/overh with parent: " (get-in parent [:rule])))
                        (let [phrasal-children
                              (lightning-bolt grammar lexicon
                                              (get-in parent [:head])
                                              (+ 1 depth)
                                              index parent morph)]
                          (if (empty? phrasal-children)
                            (let [message (str "no phrasal children for parent: " (morph parent) "(rule=" (get-in parent [:rule]) ") and spec: " (strip-refs spec))]
                              (log/debug message))
                            ;; else; there are phrasal-children, so attach them below parent:
                            (do
                              (log/debug (str "phrasal-children:" (string/join "," (map morph phrasal-children))))
                              (log/debug (str "calling overh with parent: [" (get-in parent [:rule]) "]" "'" (morph parent) "'"
                                              " and " (count phrasal-children) " phrasal children."))
                              (over/overh parent phrasal-children morph)))
                          )
                        )
                       candidate-heads))]
        (log/debug (str "lightning-bolt: #lexical children of " (get-in parent [:rule]) ":" (count lexical)))
        (log/debug (str "lightning-bolt: #phrasal children of " (get-in parent [:rule]) ":" (count phrasal)))
        (if (lexemes-before-phrases)
          (lazy-cat lexical phrasal)
          (lazy-cat phrasal lexical))))))

(defn add-complement [bolt path spec grammar lexicon cache morph]
  (let [input-spec spec
        from-bolt bolt ;; so we can show what (add-complement) did to the input bolt, for logging.
        bolt-spec (get-in bolt path :no-path)
        spec (unifyc spec bolt-spec)]
    (log/debug (str "add-complement at path: " path " to bolt with bolt:["
                    (if (map? bolt) (get-in bolt [:rule]))
                    " '" (morph bolt) "'"
                    "]"))
    (log/debug (str "add-complement to bolt with path:" path))

    (if (not (= bolt-spec :no-path)) ;; check if this bolt has this path in it.
      (let [immediate-parent (get-in bolt (butlast path))
            start-time (current-time)
            cached (if cache
                     (do
                       (let [result (get-lex immediate-parent :comp cache spec)]
                         (log/trace (str "lexical-complement candidates: " (string/join ", " (map morph result))))
                         (if (not (nil? result))
                           (log/trace (str " cached lexical subset ratio: " 
                                           (string/replace (str (/ (* 1.0 (/ (count lexicon) (count result)))))
                                                           #"\.(..).*"
                                                           (fn [[_ two-digits]] (str "." two-digits))))))
                         result))
                     (do (log/warn (str "no cache: will go through entire lexicon to find candidate complements."))
                         (reduce concat (vals lexicon))))
            complement-candidate-lexemes (if (not (= true
                                                     (get-in bolt (concat path [:phrasal]))))
                                           (if cached cached (flatten (vals lexicon))))]
        (log/debug (str " immediate parent:" (get-in immediate-parent [:rule])))
        (let [complement-pre-check (fn [child parent path-to-child]
                                     (let [child-in-bolt (get-in bolt path-to-child)]
                                       (and (not (fail?
                                                  (unifyc (get-in child [:synsem] :top)
                                                          (get-in child-in-bolt [:synsem] :top)))))))
              debug (log/debug (str "add-complement pre-filtered lexeme size:  " (count complement-candidate-lexemes)))
              filtered-lexical-complements (filter (fn [lexeme]
                                                     (complement-pre-check lexeme bolt path))
                                                   complement-candidate-lexemes)
              debug (log/debug (str "add-complement post-filtered lexeme size: " (count filtered-lexical-complements)))
              shuffled-candidate-lexical-complements (lazy-shuffle filtered-lexical-complements)
              
              return-val
              (filter (fn [result]
                        (not (fail? result)))
                      (map (fn [complement]
                             (let [debug (log/debug (str "adding complement to: ["
                                                         (get-in bolt [:rule]) " "
                                                         (morph bolt) "]: trying lexical complement:" (morph complement)))
                                   debug (if (= "" (morph complement))
                                           (log/error (str "WHY IS IT EMPTY: " complement)))
                                   result
                                   (unify (copy bolt)
                                          (path-to-map path
                                                        (copy complement)))
                                   is-fail? (fail? result)]
                               (if is-fail?
                                 (log/debug (str "fail-path-between(bolt=val1/comp=val2):"
                                                 (fail-path-between (strip-refs (get-in bolt path))
                                                                    (strip-refs complement))))
                                                                                
                                 (log/debug (str "success:" (:rule result) " returning: " (morph result))))
                                 
                               (if is-fail? :fail result)))
                     
                           ;; lazy-sequence of phrasal complements to pass one-by-one to the above (map)'s function.
                           (do
                             (log/debug (str "generating phrasal complements with spec: " (strip-refs spec)))
                             (let [phrasal-complements (generate-all spec grammar lexicon cache morph)]
                               (if (not (empty? phrasal-complements))
                                 (log/debug (str "phrasal complements were not empty; first: "
                                                 (get-in (first phrasal-complements) [:rule])))
                                 ;; phrasal complements were empty.
                                 (if (= true (get-in spec [:phrasal] false))
                                   (log/warn (str "no phrasal complements of spec: " (strip-refs spec)
                                                  " could be generated."))))
                                               
                               (if (lexemes-before-phrases)
                                 (lazy-cat shuffled-candidate-lexical-complements phrasal-complements)
                                 (do
                                   (if (not (empty? phrasal-complements))
                                     (log/debug (str "first phrasal complement for: " (get-in bolt [:rule]) ":" (morph (first phrasal-complements))))
                                     (log/debug (str "no phrasal complements for: " (get-in bolt [:rule]) ".")))
                                   (lazy-cat phrasal-complements shuffled-candidate-lexical-complements)))))))]
          (let [run-time (- (current-time) start-time)]
            (if (empty? return-val)

              ;; else, no complements could be added to this bolt: Throw an exception
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
                  (exception message))
                )

              ;; else, return-val was not empty.
              (do (log/debug (str "add-complement after adding complement: "
                                  (morph (first return-val)) ",.."))
                  return-val)))))

      ;; path doesn't exist in bolt: simply return the bolt unmodified.
      (do
        (list bolt)))))

(defn add-complements-to-bolts [bolts path spec grammar lexicon cache morph]
  (if (not (empty? bolts))
    (lazy-cat (if (or true (not (fail? (first bolts)))) ;; experimenting with using (or true ..) to disable this fail? check.
                (add-complement (first bolts) path spec grammar lexicon cache morph))
              (add-complements-to-bolts (rest bolts) path spec grammar lexicon cache morph))))

(defn path-to-map [path val]
  (let [feat (first path)]
    (if feat
      {feat (path-to-map (rest path) val)}
      val)))
