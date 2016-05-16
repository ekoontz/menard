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

(declare add-complement)
(declare lightning-bolt)
(declare generate-all)
(declare in-case-of-no-phrasal-complements)
(declare path-to-map)

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
  (filter #(not (fail? %))
          (do
            (log/debug (str "generate-all: cat: " (get-in spec [:synsem :cat])))
            (let [add-complements-to-bolts
                  (fn [bolts path]
                    (mapcat #(add-complement % path :top grammar lexicon index morph 0)
                            bolts))]
              (-> (lightning-bolt (lazy-shuffle grammar)
                                     lexicon
                                     spec 0 index morph)
                  ;; TODO: allow more than a fixed maximum depth of generation (here, 4 levels from top of tree).
                  (add-complements-to-bolts [:head :head :head :comp])
                  (add-complements-to-bolts [:head :head :comp])
                  (add-complements-to-bolts [:head :comp])
                  (add-complements-to-bolts [:comp]))))))

(defn lexemes-before-phrases [depth]
  ;; takes depth as an argument; make phrases decreasingly likely as depth increases.
  (let [result (> (rand-int (+ 1 depth)) 0)]
    (log/debug (str "lexemes-before-phrases: depth=" depth " => " result))
    result))

;; TODO: add usage of rule-to-lexicon cache (rather than using lexicon directly)
(defn lightning-bolt [grammar lexicon spec depth index morph]
  "Returns a lazy-sequence of all possible trees given a spec, where
there is only one child for each parent, and that single child is the
head of its parent. generate (above) 'decorates' each returned lightning bolt
of this function with complements."
  (log/debug (str "lightning-bolt(depth=" depth ",cat=" (get-in spec [:synsem :cat]) ")"))
  (let [maxdepth 3 ;; maximum depth of a lightning bolt: H1 -> H2 -> H3 where H3 must be a lexeme, not a phrase.
        morph (if morph morph (fn [input] (get-in input [:rule] :default-morph-no-rule)))
        depth (if depth depth 0)        
        parents (filter #(not (fail? %)) (map (fn [rule] (unifyc spec rule)) grammar))]
    (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
          (mapcat (fn [parent]
                    (if (= false (get-in parent [:head :phrasal] false))
                      (let [candidate-lexemes (get-lex parent :head index spec)]
                        (if (some fail? candidate-lexemes)
                          (exception (str "some candidate lexeme was fail?=true!")))
                        (log/debug (str "lightning-bolt: depth=" depth "; getting lexical heads for rule: " (get-in parent [:rule])))
                        (over/overh parent
                                    (map copy (lazy-shuffle candidate-lexemes))
                                    morph))))
                  parents)
          phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
          (if (< depth maxdepth)
            (mapcat (fn [parent]
                      (if (nil? (get-in parent [:head] nil))
                        (exception "get-in(parent,:head) was nil before looking for phrasal heads."))
                      (log/debug (str "lightning-bolt: depth=" depth "; getting phrasal heads for rule: " (get-in parent [:rule])
                                      " with cat: " (get-in parent [:head :synsem :cat])))
                      (over/overh parent (lightning-bolt grammar lexicon (get-in parent [:head])
                                                         (+ 1 depth) index morph)
                                  morph))
                    parents))]
      (if (lexemes-before-phrases depth)
        (lazy-cat lexical phrasal)
        (lazy-cat phrasal lexical)))))

(defn add-complement [bolt path spec grammar lexicon cache morph depth]
  (let [input-spec spec
        from-bolt bolt ;; so we can show what (add-complement) did to the input bolt, for logging.
        bolt-spec (get-in bolt path :no-path)
        depth (if depth depth 0)
        spec (unifyc spec bolt-spec)]
    (log/debug (str "add-complement(depth=" depth ", path=" path ", bolt: ["
                    (if (map? bolt) (get-in bolt [:rule])) ": '" (morph bolt) "']"))
    (if (not (= bolt-spec :no-path)) ;; check if this bolt has this path in it.
      (let [immediate-parent (get-in bolt (butlast path))
            start-time (current-time)
            cached (if cache
                     (get-lex immediate-parent :comp cache spec)
                     (do (log/warn (str "no cache: will go through entire lexicon to find candidate complements."))
                         (reduce concat (vals lexicon))))
            complement-candidate-lexemes (if (not (= true
                                                     (get-in bolt (concat path [:phrasal]))))
                                           (if cached cached (flatten (vals lexicon))))]
        (log/debug (str "add-complement(depth=" depth "): immediate parent:" (get-in immediate-parent [:rule])))
        (let [complement-pre-check (fn [child parent path-to-child]
                                     (let [child-in-bolt (get-in bolt path-to-child)]
                                       (and (not (fail?
                                                  (unifyc (get-in child [:synsem] :top)
                                                          (get-in child-in-bolt [:synsem] :top)))))))
              filtered-lexical-complements (filter (fn [lexeme]
                                                     (complement-pre-check lexeme bolt path))
                                                   complement-candidate-lexemes)
              debug (log/debug (str "add-complement: pre/post-filtered lexeme size: "
                                    (count complement-candidate-lexemes) "/"
                                    (count filtered-lexical-complements)))
              shuffled-candidate-lexical-complements (lazy-shuffle filtered-lexical-complements)
              
              complements
              (filter (fn [result]
                        (not (fail? result)))
                      (map (fn [complement]
                             (let [debug (log/debug (str "adding complement: ["
                                                         (get-in bolt [:rule]) " '"
                                                         (morph bolt) "']: "
                                                         "trying lexical complement:'" (morph complement) "'"))
                                   result
                                   (unify (copy bolt)
                                          (path-to-map path
                                                        (copy complement)))
                                   is-fail? (fail? result)]
                               (if is-fail?
                                 (log/debug (str "add-complement: fail-path-between(bolt=val1/comp=val2):"
                                                 (fail-path-between (strip-refs (get-in bolt path))
                                                                    (strip-refs complement))))
                                                                                
                                 (log/debug (str "add-complement: success:" (:rule result) " returning: '" (morph result) "'")))
                                 
                               (if is-fail? :fail result)))
                     
                           ;; lazy-sequence of phrasal complements to pass one-by-one to the above (map)'s function.
                           (do
                             (log/debug (str "generating phrasal complements with cat: " (get-in spec [:synsem :cat])))
                             (let [phrasal-complements (generate-all spec grammar lexicon cache morph)]
                               (if (not (empty? phrasal-complements))
                                 (log/debug (str "add-complement: phrasal complements were not empty; first: "
                                                 (get-in (first phrasal-complements) [:rule])))
                                 ;; phrasal complements were empty.
                                 (if (= true (get-in spec [:phrasal] false))
                                   (log/warn (str "add-complement: no phrasal complements of: " (:cat spec)
                                                  " could be generated."))))
                                               
                               (if (lexemes-before-phrases depth)
                                 (lazy-cat shuffled-candidate-lexical-complements phrasal-complements)
                                 (do
                                   (if (not (empty? phrasal-complements))
                                     (log/debug (str "first phrasal complement for: " (get-in bolt [:rule]) ":" (morph (first phrasal-complements))))
                                     (log/debug (str "no phrasal complements for: " (get-in bolt [:rule]) ".")))
                                   (lazy-cat phrasal-complements shuffled-candidate-lexical-complements)))))))]
          (let [run-time (- (current-time) start-time)]
            (if (empty? complements)
              ;; else, no complements could be added to this bolt: Throw an exception or log/warn. debateable about which to do
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
                  (exception message)))

              ;; else, complements was not empty.
              complements))))

      ;; path doesn't exist in bolt: simply return the bolt unmodified.
      (list bolt))))

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
