(ns babylon.generate
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [babylon.exception :refer [exception]]
   [babylon.serialization :as ser]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.diagnostics :as diag]
   [dag_unify.serialization :as s]
   [dag_unify.dissoc :as d]))

(declare add)
(declare add-lexeme)
(declare add-rule)
(declare frontier)
(declare generate-all)
(declare get-lexemes)
(declare headness?)
(declare make-word)
(declare reflexive-violations)
(declare remove-trailing-comps)
(declare summary-fn)

;; enable additional checks and logging that makes generation slower:
(def diagnostics? false)

(def ^:dynamic allow-backtracking? false)
(def ^:dynamic lexical-filter nil)
(def ^:dynamic log-generation? false)
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

(defn report [tree syntax-tree]
  (syntax-tree tree))

(def count-adds (atom 0))
(def count-lexeme-fails (atom 0))
(def count-rule-fails (atom 0))

(defn generate [spec grammar lexicon-index-fn syntax-tree-fn]
  (reset! count-adds 0)
  (reset! count-lexeme-fails 0)
  (reset! count-rule-fails 0)
  (let [result
        (first (generate-all [spec] grammar lexicon-index-fn syntax-tree-fn))]
    (log/debug (str "generated: " (report result syntax-tree-fn) " with "
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
      (log/debug (str "generate-all: " frontier ": " (report tree syntax-tree-fn)))
      (cond (= :fail tree)
            []

            (> (count frontier) max-depth)
            (do
              (log/warn (str "the depth got too deep on this tree: " (syntax-tree-fn tree) "; giving up on it."))
              [])
            
            (or (u/get-in tree [:babylon.generate/done?])
                (and (not (empty? frontier)) (= frontier stop-generation-at)))
            (do
              (if (not (u/get-in tree [:babylon.generate/done?]))
                (log/debug (str "early stop of generation: " (report tree syntax-tree-fn) " at: " frontier)))
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
      (log/debug (str "add: start: " (report tree syntax-tree-fn) " at:" at
                     (if (u/get-in tree (concat at [:phrasal]))
                       (str "; looking for: " (syntax-tree-fn (u/get-in tree at)))))))
    (log/debug (str "add: " (report tree syntax-tree-fn)))
    (if (and (not (= tree :fail))
             (= [:comp] at))
      (log/debug (str (report tree syntax-tree-fn) " COMP: add at:" at " with spec: " (diag/strip-refs spec))))
    (if (and (= false (u/get-in tree (concat at [:phrasal])))
             (not (= ::none (u/get-in tree (concat at [:rule]) ::none))))
      (exception (str "add: phrasal is false but rule is specified: "
                      (u/get-in tree (concat at [:rule])) " at: " at " within: " (syntax-tree-fn tree))))
    (->>
     (cond
       (u/get-in tree [:babylon.generate/done?])
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
               (log/warn (str (report tree syntax-tree-fn) ": no rule: " (u/get-in spec [:rule]) " matched spec: " (syntax-tree-fn (u/get-in tree at))
                              (if (not (empty? fail-paths))
                                fail-paths))))))
         (log/debug (str "add: condition 2: result emptiness:" (empty? result)))
         result)

       (or (= false (u/get-in tree (concat at [:phrasal])))
           (and (u/get-in tree (concat at [:canonical]))
                (= ::unspec (u/get-in tree (concat at [:head])))
                (= ::unspec (u/get-in tree (concat at [:comp])))
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
  (->> (lexicon-index-fn spec)
       (#(do (log/debug (str "get-lexeme: index-fn returned this many:" (count %)))
             %))
       (#(do (log/debug (str "get-lexeme: pre-unify: "
                             (vec (map (fn [matching-lexeme]
                                         (diag/fail-path matching-lexeme spec))
                                       %))))
             %))
       (map #(unify % spec))
       (#(do (log/debug (str "get-lexeme: post-spec unify returned this many:" (count %)))
             %))
       (filter #(or (not (= :fail %))
                    (do
                      (swap! count-lexeme-fails inc)
                      false)))
       (filter #(or (nil? lexical-filter) (lexical-filter %)))))

(defn add-lexeme [tree lexicon-index-fn syntax-tree]
  (log/debug (str "add-lexeme: " (report tree syntax-tree)))
  (let [at (frontier tree)
        done-at (concat (remove-trailing-comps at) [:babylon.generate/done?])
        spec (u/get-in tree at)
        diagnose? false]
    (log/debug (str "add-lexeme: " (report tree syntax-tree) " at: " at " with spec:"
                    (summary-fn spec) "; phrasal: " (u/get-in spec [:phrasal])))
    (if (= true (u/get-in spec [:phrasal]))
      (exception (str "don't call add-lexeme with phrasal=true! fix your code!"))
      (->> (get-lexemes spec lexicon-index-fn syntax-tree)

           ;; need this to prevent eagerly generating a tree for every matching lexeme:
           (#(take (count %) %))

           (map (fn [candidate-lexeme]
                  (log/debug (str "adding lex: '"  (u/get-in candidate-lexeme [:canonical]) "'"
                                  " with derivation: " (u/get-in candidate-lexeme [:derivation])
                                  " and subcat 2: " (u/get-in candidate-lexeme [:subcat :2])
                                  " at: " at " to: " (report tree syntax-tree)))
                  (-> tree
                      u/copy
                      (u/assoc-in! done-at true)
                      ((fn [tree]
                         (try (u/assoc-in! tree at candidate-lexeme)
                              (catch Exception e
                                :fail)))))))

           (remove #(= :fail %))))))

(defn add-rule [tree grammar syntax-tree & [rule-name some-rule-must-match?]]
  (let [at (frontier tree)
        rule-name
        (cond rule-name rule-name
              (not (nil? (u/get-in tree (concat at [:rule])))) (u/get-in tree (concat at [:rule]))
              true nil)
        cat (u/get-in tree (concat at [:cat]))]
    (log/debug (str "add-rule: @" at ": " (if rule-name (str "'" rule-name "'")) ": "
                    (report tree syntax-tree) " at: " at))
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
     ;;          tree->     /\
     ;;                     \ ..
     ;;                     /..
     ;;                    /
     ;; path points to -> [] <- add child here
     ;;
     (map (fn [rule]
            (u/assoc-in tree
                         at rule)))
     
     ;; some attempts to adjoin will have failed, so remove those:
     (filter #(or (not (= :fail %))
                  (do
                    (swap! count-rule-fails inc)
                    false))))))

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

(defn remove-trailing-comps [at]
  (cond (empty? at) at
        (= :comp
           (last at))
        (remove-trailing-comps (butlast at))
        true at))

;; TODO: move this to a ^:dynamic: variable so it can
;; be customized per-language.
(defn summary-fn [spec]
  (cond true
        (diag/strip-refs spec)

        ;; everything below is disabled (because of the 'cond true' above).
        (u/get-in spec [:rule])
        (u/get-in spec [:rule])

        (= :verb (u/get-in spec [:cat]))
        (str "subcat 1:" (u/get-in spec [:subcat :1 :cat]))
        true
        (or (u/get-in spec [:rule])
            (u/get-in spec [:canonical])
            (u/get-in spec [:sem :pred])
            (u/get-in spec [:cat]))))

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
