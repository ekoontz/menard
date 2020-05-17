(ns babylon.generate
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [babylon.exception :refer [exception]]
   [babylon.serialization :as ser]
   [babylon.treeops :as tr]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.diagnostics :as diag]))

;; See:
;; - english.cljc/generate
;; - nederlands.cljc/generate
;; for example usage.
;; for diagnostics; start with
;; changing the log/debug in (defn add) (below)
;; to log/info.

(declare add)
(declare add-lexeme)
(declare add-rule)
(declare frontier)
(declare generate-all)
(declare get-lexemes)
(declare reflexive-violations)

#?(:clj (def ^:dynamic fold? false))
#?(:clj (def ^:dynamic truncate? false))
#?(:cljs (def ^:dynamic fold? true))
#?(:cljs (def ^:dynamic truncate? true))

(def ^:dynamic allow-backtracking? true)
(def ^:dynamic max-depth 15)

(def ^:dynamic stop-generation-at
 "To use: in your own namespace, override this variable with the path
  before whose generation you want to stop.
  Generation will stop immediately
  when (frontier tree) is equal to this path."
  [])
(def ^:dynamic die-on-no-matching-lexemes? true)
(def ^:dynamic warn-on-no-matches?
  "warn in (add-rule) if no grammar rules matched the given spec."
  true)

(def count-adds (atom 0))
(def count-lexeme-fails (atom 0))
(def count-rule-fails (atom 0))

(defn generate [spec grammar lexicon-index-fn syntax-tree-fn]
  (reset! count-adds 0)
  (reset! count-lexeme-fails 0)
  (reset! count-rule-fails 0)
  (let [result
        (first (generate-all [spec] grammar lexicon-index-fn syntax-tree-fn))]
    (log/debug (str "generated: " (syntax-tree-fn result) " with "
                    @count-adds " add" (if (not (= @count-adds 1)) "s") ", "
                    @count-lexeme-fails " lexeme fail" (if (not (= @count-lexeme-fails 1)) "s") " and "
                    @count-rule-fails " rule fail" (if (not (= @count-rule-fails 1)) "s")
                    "."))
    result))

(defn generate-all
  "Recursively generate trees given input trees. continue recursively
   until no further expansion is possible."
  [trees grammar lexicon-index-fn syntax-tree-fn]
  (if (not (empty? trees))
    (let [tree (first trees)
          frontier (frontier tree)]
      (log/debug (str "generate-all: " frontier ": " (syntax-tree-fn tree)))
      (cond (= :fail tree)
            []

            (> (count frontier) max-depth)
            (do
              (log/debug (str "too deep: giving up on this tree: " (syntax-tree-fn tree) "."))
              [])
            
            (or (u/get-in tree [::done?])
                (and (not (empty? frontier)) (= frontier stop-generation-at)))
            (do
              (if (not (u/get-in tree [::done?]))
                (log/debug (str "early stop of generation: " (syntax-tree-fn tree) " at: " frontier)))
              (lazy-seq
               (cons tree
                     (generate-all (rest trees) grammar lexicon-index-fn syntax-tree-fn))))

            true
            (lazy-cat
             (generate-all
              (add tree grammar lexicon-index-fn syntax-tree-fn) grammar lexicon-index-fn syntax-tree-fn)
             (generate-all (rest trees) grammar lexicon-index-fn syntax-tree-fn))))))

(defn add [tree grammar lexicon-index-fn syntax-tree-fn]
  (swap! count-adds (fn [x] (+ 1 @count-adds)))
  (let [at (frontier tree)
        rule-at (u/get-in tree (concat at [:rule]) ::none)
        phrase-at (u/get-in tree (concat at [:phrase]) ::none)
        lexness (u/get-in tree (concat at [:canonical]) ::none)
        spec (u/get-in tree at)]
    (if (= :fail (u/get-in tree at))
      (exception (str "add: value at: " at " is fail.")))
    (if (not (= tree :fail))
      (log/debug (str "add: start: " (syntax-tree-fn tree) " at:" at
                      (if (u/get-in tree (concat at [:phrasal]))
                        (str "; looking for: " (syntax-tree-fn (u/get-in tree at)))))))
    (if (and (not (= tree :fail))
             (= [:comp] at))
      (log/debug (str (syntax-tree-fn tree) " COMP: add at:" at " with spec: " (diag/strip-refs spec))))
    (if (and (= false (u/get-in tree (concat at [:phrasal])))
             (not (= ::none (u/get-in tree (concat at [:rule]) ::none))))
      (exception (str "add: phrasal is false but rule is specified: "
                      (u/get-in tree (concat at [:rule])) " at: " at " within: " (syntax-tree-fn tree))))
    (->>
     (cond
       (u/get-in tree [::done?])
       (do
         (log/debug (str "add: condition 1."))
         [tree])
       (= tree :fail)
       (exception (str "add: tree is unexpectedly :fail."))

       (or
        (and (not (= ::none rule-at))
             (not (= :top rule-at)))
        (and (not (= ::none phrase-at))
             (not (= :top phrase-at)))
        (= true (u/get-in tree (concat at [:phrasal])))
        (u/get-in tree (concat at [:head]))
        (u/get-in tree (concat at [:comp])))
       (let [result
             (add-rule tree grammar syntax-tree-fn)]
         (log/debug (str "add: condition 2: only adding rules at: " at))
         (log/debug (str "  rule-at: " rule-at "; phrase-at:" phrase-at))
         (log/debug (str "  phrasal-at: " (u/get-in tree (concat at [:phrasal]))))
         (if (empty? result)
           (let [fail-paths
                 (vec 
                  (->> grammar
                       (filter #(= (u/get-in spec [:rule])
                                   (u/get-in % [:rule])))
                       (map (fn [rule]
                              (diag/fail-path spec rule)))))]
             (if (u/get-in spec [:rule])
               (exception (str (syntax-tree-fn tree) ": no rule: " (u/get-in spec [:rule]) " matched spec: " (syntax-tree-fn (u/get-in tree at))
                               (if (not (empty? fail-paths))
                                 fail-paths))))))
         (log/debug (str "add: condition 2: result emptiness:" (empty? result)))
         result)

       (or (= false (u/get-in tree (concat at [:phrasal])))
           (and (u/get-in tree (concat at [:canonical]))
                (= ::unspec (u/get-in tree (concat at [:head]) ::unspec))
                (= ::unspec (u/get-in tree (concat at [:comp]) ::unspec))
                (not (= :top
                        (u/get-in tree (concat at [:canonical]))))))
       (do
         (log/debug (str "add: condition 3: only adding lexemes at: " at "; spec: " (syntax-tree-fn
                                                                                    (u/get-in tree at))))
         (add-lexeme tree lexicon-index-fn syntax-tree-fn))

       true
       (let [debug (log/debug (str "add: adding both lexemes and rules."))
             both (lazy-cat (add-lexeme tree lexicon-index-fn syntax-tree-fn)
                            (add-rule tree grammar syntax-tree-fn))]
         (cond (and (empty? both)
                    allow-backtracking?)
               (do
                 (log/debug (str "backtracking: " (syntax-tree-fn tree) " at rule: "
                                 (u/get-in tree (concat (butlast at) [:rule])) " for child: "
                                 (last at))))
               (empty? both)
               (exception (str "dead end: " (syntax-tree-fn tree)
                               " at: " at))

               true both)))
     (filter #(reflexive-violations % syntax-tree-fn)))))

(defn get-lexemes
  "Get lexemes matching the spec. Use index, where the index 
   is a function that we call with _spec_ to get a set of lexemes
   that matches the given _spec_."
  [spec lexicon-index-fn syntax-tree]
  (log/debug (str "get-lexemes with spec: " (dag_unify.serialization/serialize spec)))
  (->> (lexicon-index-fn spec)
       (map (fn [lexeme]
              {:lexeme lexeme
               :unify (unify lexeme spec)}))
       (filter (fn [tuple]
                 (let [lexeme (:lexeme tuple)
                       unify (:unify tuple)]
                   (cond (not (= :fail unify))
                         true

                         true (do
                                (log/debug (str "lexeme candidate failed: " (dag_unify.diagnostics/fail-path spec lexeme)))
                                (swap! count-lexeme-fails inc)
                                false)))))
       (map :lexeme)))

(defn add-lexeme [tree lexicon-index-fn syntax-tree]
  (log/debug (str "add-lexeme: " (syntax-tree tree)))
  (let [at (frontier tree)
        done-at (concat (tr/remove-trailing-comps at) [:babylon.generate/done?])
        spec (u/get-in tree at)
        diagnose? false]
    (log/debug (str "add-lexeme: " (syntax-tree tree) " at: " at " with spec: "
                    (syntax-tree spec)))
    (if (= true (u/get-in spec [:phrasal]))
      (exception (str "don't call add-lexeme with phrasal=true! fix your code!"))
      (->> (get-lexemes spec lexicon-index-fn syntax-tree)

           (#(if (and (empty? %)
                      (= false allow-backtracking?)
                      (= false (u/get-in spec [:phrasal] ::none)))
               (exception (str "no lexemes matched spec: " (dag_unify.diagnostics/strip-refs spec)))
               %))

           ;; need this to prevent eagerly generating a tree for every matching lexeme:
           (#(take (count %) %))

           (map (fn [candidate-lexeme]
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
                      (tr/update-syntax-tree at syntax-tree)
                      (#(if truncate?
                          (tr/truncate-at % at syntax-tree)
                          %))
                      (#(if fold?
                          (tr/foldup % at syntax-tree)
                          %)))))

           (remove #(= :fail %))))))

(defn add-rule [tree grammar syntax-tree & [rule-name some-rule-must-match?]]
  (let [at (frontier tree)
        rule-name
        (cond rule-name rule-name
              (not (nil? (u/get-in tree (concat at [:rule])))) (u/get-in tree (concat at [:rule]))
              true nil)
        cat (u/get-in tree (concat at [:cat]))
        at-num (tr/numeric-frontier (:syntax-tree tree {}))]
    (log/debug (str "add-rule: @" at ": " (if rule-name (str "'" rule-name "'")) ": "
                    (syntax-tree tree) " at: " at))
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
     ;; path points here -> [] <- add candidate grammar rule here
     ;;
     (map (fn [rule]
            (u/assoc-in tree
                        at rule)))
     
     ;; some attempts to adjoin will have failed, so remove those:
     (filter #(or (not (= :fail %))
                  (do
                    (swap! count-rule-fails inc)
                    false)))
     (map
      #(u/unify! %
                 (assoc-in {} (concat [:syntax-tree] at-num)
                                   (let [one-is-head? (tr/headness? % (concat at [:1]))]
                                     {:head? (= :head (last at))
                                      :1 {:head? one-is-head?}
                                      :2 {:head? (not one-is-head?)}
                                      :variant (u/get-in % [:variant])
                                      :rule
                                      (or rule-name
                                          (u/get-in % (concat at [:rule])))}))))

     (remove #(= % :fail))

     (map (fn [tree]
            (log/debug (str "returning:  " (syntax-tree tree) "; added rule named: " rule-name))
            tree)))))

(defn frontier
  "get the next path to which to adjoin within _tree_, or empty path [], if tree is complete."
  [tree]
  
  (let [retval
        (cond
          (= :fail tree)
          []
          
          (= (u/get-in tree [::done?]) true)
          []
          
          (= (u/get-in tree [:phrasal]) false)
          []

          (empty? tree)
          []

          (= ::none (u/get-in tree [::started?] ::none))
          []
          
          (and (u/get-in tree [:head ::done?])
               (u/get-in tree [:comp ::done?]))
          []

          (and (= (u/get-in tree [:phrasal] true) true)
               (= (u/get-in tree [::started?] true) true)
               (not (u/get-in tree [:head ::done?])))
          (cons :head (frontier (u/get-in tree [:head])))

          (and (= (u/get-in tree [:phrasal] true) true)
               (= (u/get-in tree [::started?] true) true)
               (not (u/get-in tree [:comp ::done?])))
          (cons :comp (frontier (u/get-in tree [:comp])))

          true []
          
          true
          (exception (str "could not determine frontier for this tree: " (dag_unify.serialization/serialize tree))))]
    retval))

(defn reflexive-violations [expression syntax-tree-fn]
  (log/debug (str "filtering after adding..:" (syntax-tree-fn expression) "; reflexive: " (u/get-in expression [:reflexive] ::unset)))
  (log/debug (str "   subj/obj identity: " (= (:ref (u/get-in expression [:sem :subj]))
                                              (:ref (u/get-in expression [:sem :obj])))))
  (or (not (= :verb (u/get-in expression [:cat])))
      (and
       (or (= false (u/get-in expression [:reflexive] false))
           (= :top (u/get-in expression [:reflexive] :top)))
       (or
        (not (u/ref? (:ref (u/get-in expression [:sem :subj]) :top)))
        (not (u/ref? (:ref (u/get-in expression [:sem :obj]) :top)))
        (or
         (not (= (:ref (u/get-in expression [:sem :subj]))
                 (:ref (u/get-in expression [:sem :obj])))))))
      (and
        (= true (u/get-in expression [:reflexive] false))
        (= (:ref (u/get-in expression [:sem :subj]))
           (:ref (u/get-in expression [:sem :obj]))))))

(defn- add-until-done [tree grammar index-fn syntax-tree]
  (if (u/get-in tree [:babylon.generate/done?])
    ;; we are done: just return a list of the finished tree:
    [tree]

    ;; not done yet; keep going.
    (-> tree (add grammar index-fn syntax-tree) first (add-until-done grammar index-fn syntax-tree))))

(defn- add-until
  "to the tree _tree_, do (add) _n_ times."
  [tree grammar index-fn syntax-tree n]
  (cond

   (= true (u/get-in tree [:babylon.generate/done?]))
   (do
     (log/warn "do-until: tree is done already with n=" n " steps still asked for: original caller should call add-until with a smaller _n_.")
     tree)

   ;; n=0: we're done; return:
   (= n 0) tree

   ;; main case: add and call again with n=n-1:
   true
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
  (let [debug (log/debug (str "doing step 1: generate seed tree of size " seed-tree-size " .."))
        seed-tree (-> spec
                      ((fn [tree]
                         ;; add to the tree until it's reached the desired size:
                         (add-until tree grammar index-fn syntax-tree seed-tree-size))))]
    (log/debug (str "doing step 2: generate trees based on step 1's seed tree: " (syntax-tree seed-tree)))
    (repeatedly #(-> seed-tree
                     (add-until-done grammar index-fn syntax-tree)
                     first))))
