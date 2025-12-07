(ns menard.generate
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [menard.exception :refer [exception]]
   [menard.lexiconfn :as l]
   [menard.serialization :as ser]
   [menard.treeops :as tr]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.diagnostics :as diag :refer [strip-refs]]
   [dag_unify.serialization :refer [serialize]]))

;; See:
;; - english.cljc/generate
;; - español.cljc/generate
;; - nederlands.cljc/generate
;; for example usage.
;; Diagnostics:
;; 1. Start with
;; changing the log/debug in (defn add) (below)
;; to log/info.
;; 2. change other log/debugs to log/infos.
;; Profiling:
;; Turn on (def ^:dynamic profiling?) in your namespace with:
;; (binding [menard.generate/profiling? true]
;;     (menard.generate ..))
(declare add)
(declare add-lexeme)
(declare add-rule)
(declare frontier)
(declare generate-all)
(declare get-lexemes)

;; TODO: neither of these optimization work correctly
;; when =true: keep them =false until bugs with it are fixed.
#?(:clj (def ^:dynamic fold? false))
#?(:clj (def ^:dynamic truncate? false))

;; log-these-rules: show more logging for a certain
;; set of phrase-structure rules:
;; examples:
;;(def ^:dynamic log-these-rules #{"s" "vp"})
(def developer-mode? false)
(def ^:dynamic log-these-rules #{})

;; log-these-rules: show more logging for a certain
;; set of paths within a tree being generated.
;; examples:
;;(def ^:dynamic log-these-paths #{[:head :comp]})
;;(def ^:dynamic log-these-paths #{[:comp]})

(def ^:dynamic log-these-paths #{})

(def ^:dynamic log-all-rules? false)
(def ^:dynamic exception-if-no-lexemes-found? false)

;; clojurescript is much slower without these settings:
;; TODO: investigate why that only holds true for
;; .cljs and not .clj.
#?(:cljs (def ^:dynamic fold? true))
#?(:cljs (def ^:dynamic truncate? true))

;; TODO: rename to allow-rule-backtracking?
(def ^:dynamic allow-backtracking? true)

(def ^:dynamic allow-lexeme-backtracking? true)
(def ^:dynamic max-depth 6)
(def ^:dynamic max-fails 10000)
(def ^:dynamic profiling? false)
(def ^:dynamic counts? (or profiling? (not (nil? max-fails))))
(def ^:dynamic stop-generation-at
 "To use: in your own namespace, override this variable with the path
  before whose generation you want to stop.
  Generation will stop early
  when (frontier tree) is equal to this path."
  [])
(def ^:dynamic warn-on-no-matches?
  "warn in (add-rule) and (add) if no grammar rules matched the given spec"
  false)

(def count-adds (atom 0))
(def count-lexeme-fails (atom 0))
(def count-rule-fails (atom 0))
;; keys of the input spec to show for logging and debugging:
(def show-keys [:agr :cat :canonical :infl :rule :sem ::max])

(defn generate
  "Generate a single expression that satisfies _spec_ given the
  _grammar_, and the lexicon indexed by _lexicon-index-fn_, and an
  optionaly function to print out a tree _syntax-tree-fn_. See 
  nederlands/generate and english/generate for sample syntax-tree functions."
  ([spec grammar lexicon-index-fn syntax-tree-fn]
   (log/debug (str "menard.generate: start."))
   (when (empty? grammar)
     (log/error (str "grammar is empty."))
     (exception "grammar is empty."))
   (when counts?
     (reset! count-adds 0)
     (reset! count-lexeme-fails 0)
     (reset! count-rule-fails 0))
   (let [result
         (first (generate-all [spec] grammar lexicon-index-fn syntax-tree-fn))]
     (when profiling?
       (log/debug (str "generated: " (syntax-tree-fn result) " with "
                       @count-adds " add" (when (not (= @count-adds 1)) "s") ", "
                       @count-lexeme-fails " lexeme fail" (when (not (= @count-lexeme-fails 1)) "s") " and "
                       @count-rule-fails " rule fail" (when (not (= @count-rule-fails 1)) "s")
                       ".")))
     result)))

(defn generate-all
  "Recursively generate trees given input trees. continue recursively
   until no further expansion is possible."
  [trees grammar lexicon-index-fn syntax-tree-fn]
  (when (seq trees)
    (let [tree (first trees)
          frontier (frontier tree)]
      (log/debug (str "generate-all: " frontier ": " (syntax-tree-fn tree)))
      (cond (= :fail tree)
            (do
              (log/debug (str "tree failed. trying rest of trees."))
              (generate-all (rest trees) grammar lexicon-index-fn syntax-tree-fn))

            (and counts?
                 (> (+ @count-lexeme-fails @count-rule-fails)
                    max-fails)
                 (not (= frontier [])))
            (do
              (when (and developer-mode? (or log-all-rules? (contains? log-these-rules (u/get-in tree [:rule]))))
                (log/info (str "too many fails: " @count-lexeme-fails " lexeme fail(s) and " @count-rule-fails
                               " rule fail(s); giving up on this tree: " (syntax-tree-fn tree) " at: " frontier "; looking for: "
                               (strip-refs (u/get-in tree frontier)))))
              [])

            (> (count frontier) max-depth)
            (do
              (log/debug (str "too deep: giving up on this tree: " (syntax-tree-fn tree) "."))
              (generate-all (rest trees) grammar lexicon-index-fn syntax-tree-fn))

            (or (u/get-in tree [::done?])
                (and (seq frontier) (= frontier stop-generation-at)))
            (do
              (when (not (u/get-in tree [::done?]))
                (log/debug (str "early stop of generation: " (syntax-tree-fn tree) " at: " frontier)))
              (lazy-seq
               (cons tree
                     (generate-all (rest trees) grammar lexicon-index-fn syntax-tree-fn))))
            :else
            (lazy-cat
             (generate-all
              (add tree grammar lexicon-index-fn syntax-tree-fn) grammar lexicon-index-fn syntax-tree-fn)
             (generate-all (rest trees) grammar lexicon-index-fn syntax-tree-fn))))))


(defn strip-tops-out [m]
  (->> m
       (map (fn [[k v]]
              [k (cond (map? v)
                       (strip-tops-out v)
                       :else v)]))
       (filter (fn [[k v]]
                 (not (= v :top))))
       (map (fn [[k v]]
              [k (cond (and
                        (map? v)
                        (empty? v))
                       :top
                       :else v)]))
       (into {})
       ((fn [x]
          (cond (and (map? x)
                     (empty? x))
                :top
                :else x)))))

(defn add
  "Return a lazy sequence of all trees made by adding every possible
  leaf and tree to _tree_. This sequence is the union of all trees
  made by adding a leaf (add-lexeme) or by adding a whole
  sub-tree (add-rule)."
  [tree grammar lexicon-index-fn syntax-tree-fn]
  (when counts? (swap! count-adds (fn [_] (+ 1 @count-adds))))
  (when (and developer-mode? (or log-all-rules? (and developer-mode? (contains? log-these-rules (u/get-in tree [:rule])))))
    (log/info (str "add with tree: " (syntax-tree-fn tree) "; depth: " (count (frontier tree)))))

  (let [at (frontier tree)
        rule-at? (u/get-in tree (concat at [:rule]) false)
        phrase-at? (u/get-in tree (concat at [:phrase]) false)
        spec (u/get-in tree at)
        gen-condition
        (cond
          ;; condition 0: tree is :fail.
          (= tree :fail) :tree-is-fail

          ;; condition 1: tree is done: return a list with one member: the tree.
          (u/get-in tree [::done?]) :done

          ;; condition 2: only add rules at location _at_:
          (or
           (and (not (= false rule-at?))
                (not (= :top rule-at?)))
           (and (not (= false phrase-at?))
                (not (= :top phrase-at?)))
           (= true (u/get-in tree (concat at [:phrasal?])))
           (u/get-in tree (concat at [:head]))
           (u/get-in tree (concat at [:comp]))) :rules-only

          ;; condition 3: add only lexemes at location _at_:
          (= false (u/get-in tree (concat at [:phrasal?]))) :lexemes-only

          ;; condition 4: add both lexemes and rules at location _at_:
          :else :both)]
    (when (and developer-mode? (or log-all-rules? (contains? log-these-rules (u/get-in tree [:rule]))))
      (log/info (str "add: start: " (syntax-tree-fn tree) " at: " at
                     (str "; looking for: "
                          (when (map? (u/get-in tree at))
                            (strip-refs (select-keys (u/get-in tree at) show-keys)))
                          "; gen-condition: " gen-condition))))
    (->>
     (cond
       ;; condition 0: tree is :fail.
       (= gen-condition :fail)
       (exception (str "add: tree is unexpectedly :fail."))

       ;; condition 1: tree is done: return a list with one member: the tree.
       (= gen-condition :done)
       [tree]

       ;; condition 2: only add rules at location _at_:
       (= gen-condition :rules-only)
       (let [result (add-rule tree grammar syntax-tree-fn)]
         (when (and warn-on-no-matches? (empty? result))
           (let [fail-paths
                 (->> grammar
                      (filter #(or (= ::none (u/get-in spec [:rule] ::none))
                                   = (u/get-in spec [:rule])
                                   (u/get-in % [:rule])))
                      (map (fn [rule]
                             (str (u/get-in rule [:rule]) ":" (diag/fail-path spec rule)))))]
             (log/warn (str (syntax-tree-fn tree) ": no rule: "
                            (u/get-in spec [:rule]) " matched spec: "
                            (strip-refs (u/get-in tree at)) " at: " at
                            "; fail-paths:"
                            (when (seq fail-paths)
                              (vec fail-paths))))))
         (when (and developer-mode? (or log-all-rules? (contains? log-these-rules (u/get-in tree [:rule]))))
           (log/info (str "add: start: " (syntax-tree-fn tree) " at: " at
                          (str "; first result: "
                               (strip-refs (select-keys (first result) show-keys))))))
         result)

       ;; condition 3: add only lexemes at location _at_:
       (= gen-condition :lexemes-only)
       (add-lexeme tree lexicon-index-fn syntax-tree-fn)

       ;; condition 4: add both lexemes and rules at location _at_:
       :else ;; (= gen-condition :both)
       (let [both (lazy-cat (add-lexeme tree lexicon-index-fn syntax-tree-fn)
                            (add-rule tree grammar syntax-tree-fn))]
         (when (and developer-mode? (or log-all-rules? (contains? log-these-rules (u/get-in tree [:rule]))))
           (log/info (str "add: adding both lexemes and rules: allow-backtracking? " allow-backtracking? "; tree: "
                          (syntax-tree-fn tree))))
         (cond (and (empty? both)
                    allow-backtracking?)
               (when (and developer-mode? (or log-all-rules? (contains? log-these-rules (u/get-in tree [:rule]))))
                 (log/info (str "backtracking: " (syntax-tree-fn tree) " at rule: "
                                (u/get-in tree (concat (butlast at) [:rule])) " for child: "
                                (last at))))
               (empty? both)
               (exception (str "dead end: " (syntax-tree-fn tree)
                               "; looking for: "
                               (strip-refs (u/get-in tree at))
                               " at: " at))
               :else both)))

     (#(if (u/get-in spec [::max])
         (do
           (log/debug (str "taking only: " (u/get-in spec [::max]) " trees."))
           (take (u/get-in spec [::max]) %))

         %))

     (#(do (when (and (empty? %)
                      (u/get-in spec [::max]))
             (exception (str "add: empty results: spec: " (-> spec strip-refs) "; max: " (u/get-in spec [::max]))))
           %)))))

(declare get-lexemes)

(defn pluralize [input count]
  (cond
    (== count 1)
    (str input)

    ;; TODO add noun ending morphology e.g. try => tries, match => matches
    :else (str input "s")))

(defn add-lexeme
  "Return a lazy sequence of all trees made by adding every possible
  leaf (via (get-lexemes)) to _tree_."
  [tree lexicon-index-fn syntax-tree]
  (let [at (frontier tree)
        more-logging? (and developer-mode?
                           (or log-all-rules?
                               (contains? log-these-paths (vec at))
                               (contains? log-these-rules (u/get-in tree [:rule]))))
        done-at (concat (tr/remove-trailing-comps at) [:menard.generate/done?])
        spec (u/get-in tree at)]
    (when (and developer-mode? (or log-all-rules? (contains? log-these-rules (u/get-in tree [:rule])) (contains? log-these-paths at)))
      (log/info (str "add-lexeme: " (syntax-tree tree) " at: " (vec at) " looking for lexeme matching spec: " (select-keys (l/pprint spec) show-keys))))
    (if (= true (u/get-in spec [:phrasal?]))
      (exception (str "don't call add-lexeme with phrasal=true! fix your grammar and/or lexicon."))
      (->> (get-lexemes spec lexicon-index-fn at)
           (#(do
               (when more-logging?
                 (log/info (str "add-lexeme: post-exception-checking: found this many lexemes: " (count %) " at: " at))
                 (doall (map (fn [lexeme]
                               (log/debug (str "found lexeme: " (syntax-tree lexeme))))
                             %)))
               %))

           (#(if (and exception-if-no-lexemes-found? (empty? %))
               (do
                 (exception (str "no lexemes found at all for spec: " (l/pprint spec))))
               %))

           (#(if (and (empty? %)
                      (= false allow-lexeme-backtracking?)
                      (= false (u/get-in spec [:phrasal?] ::none)))
               (exception (str "no lexemes for tree: "
                               (syntax-tree tree) " at: " at
                               "; no lexemes matched spec: " (-> spec dag_unify.diagnostics/strip-refs strip-tops-out)))
               %))

           (map (fn [candidate-lexeme]
                  (when (and developer-mode? (or log-all-rules? (contains? log-these-rules (u/get-in tree [:rule]))))
                    (log/info (str "adding candidate lexeme at: " (vec at) ": "
                                   (syntax-tree candidate-lexeme))))
                  (-> tree
                      u/copy
                      (u/assoc-in! done-at true)

                      ;; do the actual adjoining of the child within the _tree_'s path _at_:
                      ;;
                      ;;            tree->     /\
                      ;;                       \ ..
                      ;;                       /..
                      ;;                      /
                      ;; path points here -> [] <- add candidate-lexeme here
                      ;;
                      (u/assoc-in! at candidate-lexeme)
                      (#(do (when (not (= :fail %))
                              (log/debug (str "successfully added lexeme: " (strip-refs candidate-lexeme))))
                            %))
                      (tr/update-syntax-tree at syntax-tree)
                      (#(if truncate?
                          (tr/truncate-at % at syntax-tree)
                          %))
                      (#(if fold?
                          (tr/foldup % at syntax-tree)
                          %)))))

           (remove #(= :fail %))

           (#(if (u/get-in spec [::max])
               (take (u/get-in spec [::max]) %)
               %))

           (#(do (when (and (empty? %)
                            (u/get-in spec [::max]))
                   (exception (str "add-lexeme: empty results: spec: " (-> spec strip-refs))))
                 %))))))

(defn add-rule
  "Return a lazy sequence of all trees made by adding every possible grammatical
   rule in _grammar_ to _tree_."
  [tree grammar syntax-tree & [rule-name]]
  (let [at (frontier tree)
        rule-name
        (cond rule-name rule-name
              (not (nil? (u/get-in tree (concat at [:rule])))) (u/get-in tree (concat at [:rule]))
              :else nil)
        cat (u/get-in tree (concat at [:cat]))
        at-num (tr/numeric-frontier (:syntax-tree tree {}))
        more-logging? false]
    (if more-logging? (log/info (str "add-rule: @" at ": " (when rule-name (str "'" rule-name "'")) ": "
                                     (syntax-tree tree) " at: " at " with spec: " (-> tree (u/get-in at) strip-refs))))
    (->>
     ;; start with the whole grammar, shuffled:
     (shuffle grammar)

     ;; if a :rule is supplied, then filter out all rules that don't have this name:
     (filter #(or (nil? rule-name)
                  (do
                    (log/debug (str "add-rule: looking for rule named: " (u/get-in % [:rule])))
                    (= (u/get-in % [:rule]) rule-name))))

     ;; if a :cat is supplied, then filter out all rules that specify a different :cat :
     (filter #(or (nil? cat) (= cat :top) (= :top (u/get-in % [:cat] :top)) (= (u/get-in % [:cat]) cat)))

     ;; do the actual adjoining of the child within the _tree_'s path _at_:
     ;;
     ;;            tree->     /\
     ;;                       \ ..
     ;;                       /..
     ;;                      /
     ;; _at_ points here -> [] <- add candidate grammar rule here
     ;;
     (map (fn [rule]
            (log/debug (str "trying rule: " (:rule rule)))
            (let [result (u/assoc-in tree at rule)]
              (when (= :fail result)
                (log/trace
                 (str "rule: " (:rule rule) " failed: "
                      (vec (diag/fail-path tree
                                           (u/assoc-in {} at rule))))))
              result)))

     ;; some attempts to adjoin will have failed, so remove those:
     (filter #(or (not (= :fail %))
                  (do
                    (when counts? (swap! count-rule-fails inc))
                    false)))
     (map
      #(u/unify! %
                 (assoc-in {} (concat [:syntax-tree] at-num)
                           (let [one-is-head? (tr/headness? % (concat at [:1]))]
                             {:head? (= :head (last at))
                              :reflexive? (u/get-in % (concat at [:reflexive?])
                                                    :top)
                              :1 {:head? one-is-head?}
                              :2 {:head? (not one-is-head?)}
                              :variant (u/get-in % [:variant])
                              :rule
                              (or rule-name
                                  (u/get-in % (concat at [:rule])))}))))

     (remove #(= % :fail))

     (map (fn [tree]
            (log/debug (str "add-rule: returning:  " (syntax-tree tree) "; added rule: " (u/get-in tree (concat at [:rule])) " at: " at))
            tree))

     (#(if (u/get-in tree [::max])
         (take (u/get-in tree [::max]) %)
         %))

     (#(do (when (and (empty? %)
                      (u/get-in tree [::max]))
             (exception (str "add-rule: empty results: spec: " (-> tree strip-refs))))
           %)))))

(defn get-lexemes
  "Get lexemes matching the spec. Use index, where the index 
   is a function that we call with _spec_ to get a set of lexemes
   that matches the given _spec_."
  [spec lexicon-index-fn at]
  (if (and developer-mode? (or log-all-rules? (contains? log-these-paths (vec at))))
    (log/info (str "get-lexemes with spec: " (l/pprint spec) " at: " at)))
  (if (nil? lexicon-index-fn)
    (exception (str "lexical-index-fn was null.")))
  (let [canonical-spec (u/get-in spec [:canonical] :top)]
    (->> (lexicon-index-fn spec)

         (#(do
             (when (or log-all-rules? (contains? log-these-paths (vec at)))
               (log/info (str "get-lexemes: lexicon-index-fn returned: " (count %) " lexeme(s) found.")))
             %))

         (filter (fn [lexeme]
                   (not (= :fail (unify canonical-spec (u/get-in lexeme [:canonical] :top))))))

         (map (fn [lexeme]
                (log/debug (str "looking at lexeme: " (l/pprint lexeme) " with spec: " (l/pprint spec)))
                (if (and developer-mode? (contains? log-these-paths (vec at)))
                  {:lexeme lexeme
                   :unify (unify lexeme spec)}
                  {:unify (unify lexeme spec)})))

         (filter (fn [{lexeme :lexeme
                       unify :unify}]
                   (log/debug (str "candidate lexeme surface:   " (u/get-in lexeme [:surface])))
                   (log/debug (str "candidate lexeme canonical: " (u/get-in lexeme [:canonical])))                 
                   (log/debug (str "candidate lexeme: " (l/pprint lexeme)))                 
                   (log/debug (str "candidate unify : " (l/pprint unify)))
                   (cond
                     (= unify :fail)
                     (do
                       (when counts? (swap! count-lexeme-fails inc))
                       (let [fail-path (dag_unify.diagnostics/fail-path spec lexeme)]
                         (when (and developer-mode? (contains? log-these-paths (vec at)))
                           (log/info (str "lexeme candidate: "
                                          (cond (u/get-in lexeme [:surface])
                                                (str "'" (u/get-in lexeme [:surface]) "'")
                                                (u/get-in lexeme [:canonical])
                                                (str "_" (u/get-in lexeme [:canonical]) "_"
                                                     (if (u/get-in lexeme [:sense])
                                                       (str ":" (u/get-in lexeme [:sense]))))
                                                :true
                                                (l/pprint lexeme))
                                          " failed: " (vec fail-path) "; "
                                          " lexeme's value: "
                                          (serialize (u/get-in lexeme fail-path)) "; "
                                          " spec's value: "
                                          (serialize (u/get-in spec fail-path))))))
                       false)

                     :else
                     (do
                       (when (and developer-mode? (contains? log-these-paths (vec at)))
                         (log/info (str "candidate lexeme: at: " (vec at) ": "
                                        (or (u/get-in lexeme [:canonical]) (l/pprint lexeme))))
                         (log/info (str "lexeme candidate: "
                                        (cond (u/get-in lexeme [:surface])
                                              (str "'" (u/get-in lexeme [:surface]) "'")
                                              (u/get-in lexeme [:canonical])
                                              (str "_" (u/get-in lexeme [:canonical]) "_"
                                                   (if (u/get-in lexeme [:sense])
                                                     (str ":" (u/get-in lexeme [:sense]))))
                                              :true
                                              (l/pprint lexeme)) " succeeded: " (strip-refs unify))))
                       true))))
         (map :unify))))

(defn frontier
  "get the next path to which to adjoin within _tree_, or empty path [], if tree is complete."
  [tree]

  (let [retval
        (cond
          (= :fail tree)
          []

          (= (u/get-in tree [::done?]) true)
          []

          (= (u/get-in tree [:phrasal?]) false)
          []

          (empty? tree)
          []

          (= ::none (u/get-in tree [::started?] ::none))
          []

          (and (u/get-in tree [:head ::done?])
               (u/get-in tree [:comp ::done?]))
          []

          (and (= (u/get-in tree [:phrasal?] true) true)
               (= (u/get-in tree [::started?] true) true)
               (not (u/get-in tree [:head ::done?])))
          (vec (cons :head (frontier (u/get-in tree [:head]))))

          (and (= (u/get-in tree [:phrasal?] true) true)
               (= (u/get-in tree [::started?] true) true)
               (not (u/get-in tree [:comp ::done?])))
          (vec (cons :comp (frontier (u/get-in tree [:comp]))))

          :else [])]
    retval))

(defn- add-until-done [tree grammar index-fn syntax-tree]
  (if (u/get-in tree [:menard.generate/done?])
    ;; we are done: just return a list of the finished tree:
    [tree]

    ;; not done yet; keep going.
    (-> tree
        (add grammar index-fn syntax-tree)
        first
        (add-until-done grammar index-fn syntax-tree))))

(defn- add-until
  "to the tree _tree_, do (add) _n_ times."
  [tree grammar index-fn syntax-tree n]
  (cond

   (= true (u/get-in tree [:menard.generate/done?]))
   (do
     (log/warn "do-until: tree is done already with n=" n " steps still asked for: original caller should call add-until with a smaller _n_.")
     tree)

   ;; n=0: we're done; return:
   (= n 0) tree

   ;; main case: add and call again with n=n-1:
   :else
   (add-until (-> tree
                  (add grammar index-fn syntax-tree)
                  first)
              grammar
              index-fn
              syntax-tree
              (- n 1))))

(defn generate-seedlike
  "Return a lazy sequence of sentences, all of which are generated starting with a seed tree that itself
     is generated from _spec_. The seed tree's size is set by _seed-tree-size_, which means how big to make the seed tree:
   A bigger seed tree size means step 1 takes longer, but step 2 is shorter, and step 2's output sentences are more similar to each other.
   A smaller seed tree size means step 1 runs shorter, but step 2 is longer, and step 2's output sentences are more distinct from each other.
   So the tradeoff if between variety and speed: the sentences are more similar the more we pre-compute
   in the start, and the more different and slower the sentences are the less we pre-compute.
   For example, for expression 16: total size is 13, and current measurements are:
    - if seed-tree-size=13, then initial seed takes 1500 ms and each child tree takes    0 ms (because tree is already fully done).
    - if seed-tree-size=12, then initial seed takes 1375 ms and each child tree takes   35 ms.
    - if seed-tree-size=10, then initial seed takes 1200 ms and each child tree takes  100 ms.
    - if seed-tree-size=3,  then initial seed takes   73 ms and each child tree takes 1200 ms."
  [spec seed-tree-size grammar index-fn syntax-tree]
  (log/debug (str "doing step 1: generate seed tree of size " seed-tree-size " .."))
  (let [seed-tree (-> spec
                      ((fn [tree]
                         ;; add to the tree until it's reached the desired size:
                         (add-until tree grammar index-fn syntax-tree seed-tree-size))))]
    (log/debug (str "doing step 2: generate trees based on step 1's seed tree: " (syntax-tree seed-tree)))
    (repeatedly #(-> seed-tree
                     (add-until-done grammar index-fn syntax-tree)
                     first))))
