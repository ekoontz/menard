(ns babylon.generate
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [babylon.exception :refer [exception]]
   [babylon.serialization :as ser]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.serialization :as s]
   [dag_unify.dissoc :as d]))

(declare add)
(declare add-lexeme)
(declare add-rule)
(declare dissoc-in)
(declare foldup)
(declare frontier)
(declare generate-all)
(declare get-lexemes)
(declare headness?)
(declare make-word)
(declare numeric-frontier)
(declare numeric-path)
(declare reflexive-violations)
(declare remove-trailing-comps)
(declare summary-fn)
(declare truncate-at)
(declare update-syntax-tree)

;; enable additional checks and logging that makes generation slower:
(def diagnostics? false)
;; TODO: generation with allow-folding?=false doesn't work reliably:
;; either fix or might be time to not support allow-folding?=false anymore.
(def allow-folding? true)
(def allow-truncation? true)
(def ^:dynamic allow-backtracking? false)
(def ^:dynamic lexical-filter nil)
(def ^:dynamic log-generation? false)

(def ^:dynamic stop-generation-at
 "To use: in your own namespace, override this variable with the path
  before whose generation you want to stop.
  Generation will stop immediately
  when (frontier tree) is equal to this path.
  Note that the path must be given
  as where the generator is according to (frontier), which may
  differ from the path to the same node as in the logical syntax tree,
  because folding might have occurred."
  [])
(def ^:dynamic die-on-no-matching-lexemes? true)
(def ^:dynamic warn-on-no-matches?
  "warn in (add-rule) if no grammar rules matched the given spec."
  true)

(defn report [tree syntax-tree]
  (str "#" (count (str tree)) " " (syntax-tree tree)))

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

            (> (count frontier) 5)
            []
            
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
      (log/debug (str "add: " (report tree syntax-tree-fn) " at:" at
                     (if (u/get-in tree (concat at [:phrasal]))
                       (str "; looking for phrasal: " (u/get-in tree (concat at [:phrasal])))))))
    (if (and (not (= tree :fail))
             (= [:comp] at))
      (log/debug (str (report tree syntax-tree-fn) " COMP: add at:" at " with spec: " (u/strip-refs spec))))
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
           (if (u/get-in spec [:rule])
             (log/warn (str (report tree syntax-tree-fn) ": no rule: " (u/get-in spec [:rule]) " matched. fail-paths were: "
                            (vec
                             (->> grammar
                                  (filter #(= (u/get-in spec [:rule])
                                              (u/get-in % [:rule])))
                                  (map (fn [rule]
                                         (u/fail-path spec rule)))))))
             (if warn-on-no-matches? (log/debug (str (report tree syntax-tree-fn) ": no rules matched spec: " (u/strip-refs spec) ".")))))
         (log/debug (str "add: condition 2: result emptiness:" (empty? result)))
         result)

       (or (= false (u/get-in tree (concat at [:phrasal])))
           (and (u/get-in tree (concat at [:canonical]))
                (= ::unspec (u/get-in tree (concat at [:head])))
                (= ::unspec (u/get-in tree (concat at [:comp])))
                (not (= :top
                        (u/get-in tree (concat at [:canonical]))))))
       (do
         (log/debug (str "add: condition 3: only adding lexemes at: " at))
         (add-lexeme tree lexicon-index-fn syntax-tree-fn))

       true
       (let [both (lazy-cat (add-lexeme tree lexicon-index-fn syntax-tree-fn)
                            (add-rule tree grammar syntax-tree-fn))]
         (cond (and (empty? both)
                    allow-backtracking?)
               (do
                 (log/debug (str "backtracking: " (syntax-tree-fn tree) " at rule: "
                                 (u/get-in tree (concat (butlast at) [:rule])) " for child: "
                                 (last at))))
               (empty? both)
               (exception (str "dead end: " (syntax-tree-fn tree)
                               " at: " at "; looking for: "
                               (vec (s/serialize spec))))

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
                                         (u/fail-path matching-lexeme spec))
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

(declare update-syntax-tree)

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
                                :fail))))
                      (update-syntax-tree at syntax-tree)
                      (#(if allow-truncation?
                          (truncate-at % at syntax-tree)
                          %))
                      (foldup at syntax-tree))))

           (remove #(= :fail %))))))

(defn add-rule [tree grammar syntax-tree & [rule-name some-rule-must-match?]]
  (let [at (frontier tree)
        rule-name
        (cond rule-name rule-name
              (not (nil? (u/get-in tree (concat at [:rule])))) (u/get-in tree (concat at [:rule]))
              true nil)
        cat (u/get-in tree (concat at [:cat]))
        at-num (numeric-frontier (:syntax-tree tree {}))]
    (if log-generation? (log/info (str "add-rule: @" at ": " (if rule-name (str "'" rule-name "'")) ": "
                                       (report tree syntax-tree) " at: " at " (numerically): " at-num)))
    (->>
     ;; start with the whole grammar, shuffled:
     (shuffle grammar)
     
     ;; if a :rule is supplied, then filter out all rules that don't have this name:
     (filter #(or (nil? rule-name) (= (u/get-in % [:rule]) rule-name)))

     ;; if a :cat is supplied, then filter out all rules that specify a different :cat :
     (filter #(or (nil? cat) (= cat :top) (= :top (u/get-in % [:cat] :top)) (= (u/get-in % [:cat]) cat)))

     ;; do the actual adjoining of the child within the _tree_'s path _at_:
     (map (fn [rule]
            (if log-generation? (log/info (str "add-rule: " (report tree syntax-tree) " adding rule: " (u/get-in rule [:rule]) "; with variant: " (u/get-in rule [:variant]))))
            (binding [u/log-serializing? false]
              (u/assoc-in! (u/copy tree)
                           at (u/copy rule)))))

     ;; some attempts to adjoin will have failed, so remove those:
     (filter #(or (not (= :fail %))
                  (do
                    (swap! count-rule-fails inc)
                    false)))
     (map
      #(u/unify! %
                 (assoc-in {} (concat [:syntax-tree] at-num)
                                   (let [one-is-head? (headness? % (concat at [:1]))]
                                     {:head? (= :head (last at))
                                      :1 {:head? one-is-head?}
                                      :2 {:head? (not one-is-head?)}
                                      :variant (u/get-in % [:variant])
                                      :rule
                                      (or rule-name
                                          (u/get-in % (concat at [:rule])))}))))

     (remove #(= % :fail)))))

(defn update-syntax-tree [tree at syntax-tree]
  (log/debug (str "updating syntax-tree:" (report tree syntax-tree) " at: " at))
  (cond (= :fail tree)
        tree
        true
        (let [head? (headness? tree at)
              ;; ^ not sure if this works as expected, since _tree_ and (:syntax-tree _tree) will differ
              ;; if folding occurs.
              numerically-at (numeric-frontier (u/get-in tree [:syntax-tree]))
              word (merge (make-word)
                          {:head? head?})]
          (log/debug (str "update-syntax-tree: at: " at "; numerically-at:" numerically-at))
          (u/unify! tree
                    (merge (s/create-path-in (concat [:syntax-tree] numerically-at) word)
                           (s/create-path-in at word))))))

(defn make-word []
  {:agr (atom :top)
   :canonical (atom :top)
   :exceptions (atom :top)
   :cat (atom :top)
   :infl (atom :top)
   :sem (atom :top)
   :inflected? (atom :top)
   :root (atom :top)})

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

(defn truncate-at [tree at syntax-tree]
  (cond
    (= :fail tree)
    tree
    true
    (let [parent-at (-> at butlast)
          parent (u/get-in tree parent-at)
          grandparent-at (-> parent-at butlast vec)
          grandparent (u/get-in tree grandparent-at)
          uncle-head-at (-> grandparent-at (concat [:head]) vec)
          nephew-at (-> parent-at (concat [:head]))
          nephew (u/get-in tree nephew-at)]
      ;; TODO: also truncate :head at this point, too:
      (log/debug (str "truncate@: " at "(at) " (report tree syntax-tree)))
      (if (= :comp (last at))
        (let [compless-at (if (empty? (remove-trailing-comps at))
                            ;; in this case, we have just added the final :comp at the
                            ;; root of the tree, so simply truncate that:
                            [:comp]

                            ;; otherwise, ascend the tree as high as there are :comps
                            ;; trailing _at_.
                            (remove-trailing-comps at))]
          (log/debug (str "truncate@: " compless-at " (Compless at) " (report tree syntax-tree)))
          (log/debug (str "truncate@: " (numeric-path tree compless-at) " (Numeric-path at) " (report tree syntax-tree)))
          (-> tree
              (dissoc-in compless-at)
              (dissoc-in (numeric-path tree compless-at))
              (dissoc :dag_unify.serialization/serialized)
              (u/assoc-in! (concat compless-at [::done?]) true)
              (dissoc-in (concat (butlast compless-at) [:head :subcat]))
              (dissoc-in (concat (butlast compless-at) [:head :derivation]))
              (dissoc-in (concat (butlast compless-at) [:head :sem]))
              (dissoc-in (concat (butlast compless-at) [:head :exceptions]))
              (dissoc-in (concat (butlast compless-at) [:1]))
              (dissoc-in (concat (butlast compless-at) [:2]))
              ((fn [tree]
                 (log/debug (str "afterwards: " (report tree syntax-tree) "; keys of path: " (vec (concat (butlast compless-at) [:head])) ": "
                                 (keys (u/get-in tree (concat (butlast compless-at) [:head])))))
                 (cond true tree)))))
        tree))))

;; fold up a tree like this:
;;
;;       grandparent
;;      /   \ C
;;   H /    parent
;;   uncle  / \
;;         /   \
;;      H /     \
;;      nephew   _ complement-of-nephew
;;
;; into:
;;
;;      grandparent
;;      /         \ C
;;   H /           \
;;    uncle+nephew   _ complement-of-nephew
;;
(defn foldable?
  "determine whether the given _tree_ is foldable, if the given _at_ points to a nephew"
  [tree at syntax-tree]
  (let [st (syntax-tree tree)
        grandparent (u/get-in tree (-> at butlast butlast))
        parent (u/get-in tree (-> at butlast))
        uncle (u/get-in grandparent [:head])
        cond1 (not (empty? (-> at butlast butlast)))
        cond2 (= (get parent :head)
                 (get parent :1))
        cond3 (= (get grandparent :head)
                 (get grandparent :1))
        cond4 (not (nil? (u/get-in tree (-> at butlast (concat [:comp])))))
        cond5 (nil? (u/get-in tree (concat at [:subcat :3])))]
    (cond (and cond1 cond2 cond3 cond4 cond5)
          (do (log/debug (str "FOLD OK: " (syntax-tree tree) " at: " at))
              true)
          (false? cond1)
          (do (log/debug (str "cond1? " cond1 " " st " at: " at))
              false)
          (false? cond2)
          (do (log/debug (str "cond2? " cond3 " " st " at: " at))
              false)
          (false? cond3)
          (do (log/debug (str "cond3? " cond3 " " st " at: " at))
              false)
          (false? cond4)
          (do (log/debug (str "cond4? " cond4 " " st " at: " at))
              false)
          (false? cond5)
          (do (log/debug (str "cond5? " cond5 " " st " at: " at))
              false)
          
          true (exception (str "should never get here: did you miss adding a cond-check in foldable?")))))

;; fold up a tree like this:
;;
;;       grandparent
;;      /   \ C
;;   H /    parent
;;   uncle  / \
;;         /   \
;;      H /     \
;;      nephew   _ nephew complement
;;
;; into:
;;
;;      grandparent
;;      /         \ C
;;   H /           \
;;    uncle+nephew   _ nephew complement
;;
(defn foldup [tree at syntax-tree]
  (cond
    (u/get-in tree [::done?]) tree
    
    (and allow-folding? (foldable? tree at syntax-tree))
    (let [grandparent (u/get-in tree (-> at butlast butlast))
          nephew-complement (u/get-in tree (-> at butlast (concat [:comp])))]
      (log/debug (str "folding    " at " " (report tree syntax-tree)))
      (log/debug (str "nephew-complement: " (report nephew-complement syntax-tree)))
      (swap! (get grandparent :comp)
             (fn [old] nephew-complement))
      (dissoc tree :dag_unify.serialization/serialized))
    true
    tree))

(defn numeric-frontier [syntax-tree]
  (cond
    (and (map? syntax-tree)
         (:syntax-tree syntax-tree))
    (numeric-frontier (:syntax-tree syntax-tree))

    (and (map? syntax-tree)
         (-> syntax-tree :canonical))
    :done

    (and (map? syntax-tree)
         (nil? (-> syntax-tree :1))
         (nil? (-> syntax-tree :2)))
    []

    (and (map? syntax-tree)
         (= :done (numeric-frontier (-> syntax-tree :2)))
         (not (= :done (numeric-frontier (-> syntax-tree :1)))))
    (cons :1 (numeric-frontier (-> syntax-tree :1)))
    
    (and (map? syntax-tree)
         (= :done (numeric-frontier (-> syntax-tree :1)))
         (not (= :done (numeric-frontier (-> syntax-tree :2)))))
    (cons :2 (numeric-frontier (-> syntax-tree :2)))

    (and (map? syntax-tree)
         (= (-> syntax-tree :1 numeric-frontier) :done)
         (= (-> syntax-tree :2 numeric-frontier) :done))
    :done

    (nil? syntax-tree) []

    (and (map? syntax-tree)
         (-> syntax-tree :1 :head?))
    (cons :1 (numeric-frontier (-> syntax-tree :1)))

    (and (map? syntax-tree)
         (-> syntax-tree :2 :head?))
    (cons :2 (numeric-frontier (-> syntax-tree :2)))
    
    true (exception (str "unhandled: " (u/strip-refs syntax-tree)))))

(defn numeric-path
  "convert a path made of [:head,:comp]s into one made of [:1,:2]s."
  [tree at]
  (cond
    (empty? at) []

    (or (and (= (first at) :head)
             (= (get tree :head)
                (get tree :1)))
        (and (= (first at) :comp)
             (= (get tree :comp)
                (get tree :1))))
    (cons :1 (numeric-path (u/get-in tree [(first at)]) (rest at)))

    true
    (cons :2 (numeric-path (u/get-in tree [(first at)]) (rest at)))))

(defn headness? [tree at]
  (or
   (= (last at) :head)
   (and
    (= (last at) :1)
    (= (get (u/get-in tree (butlast at)) :1)
       (get (u/get-in tree (butlast at)) :head)))
   (and
    (= (last at) :1)
    (= (get (u/get-in tree (butlast at)) :1)
       (get (u/get-in tree (butlast at)) :head)))))

(defn remove-trailing-comps [at]
  (cond (empty? at) at
        (= :comp
           (last at))
        (remove-trailing-comps (butlast at))
        true at))

;; TODO: consider using dag_unify.dissoc/dissoc-in.
;; https://github.com/weavejester/medley/blob/1.1.0/src/medley/core.cljc#L20
(defn dissoc-in
  "Dissociate a value in a nested associative structure, identified by a sequence
  of keys. Any collections left empty by the operation will be dissociated from
  their containing structures."
  [m ks]
  (if-let [[head & tail] ks]
    (do
      (log/debug (str "ks: " ks))
      (log/debug (str "HEAD: " head))
      (log/debug (str "TAIL: " tail))      
      (cond
        tail
        (let [v (dissoc-in (u/get-in m [head]) tail)]
          (cond
            (empty? v)
            (dissoc m head)
            true
            (do
              (log/debug (str "type of head: " (get m head)))
              (cond
                (u/ref? (get m head))
                (do
                  (log/debug (str "doing swap!"))
                  (swap! (get m head)
                         (fn [x] v))
                  m)
                true
                (do
                  (log/debug (str "doing default: " assoc))
                  (assoc m head v))))))
        true
        (do
          (log/debug (str "HEAD(alone):" head))
          (dissoc m head))))
    m))

;; TODO: move this to a ^:dynamic: variable so it can
;; be customized per-language.
(defn summary-fn [spec]
  (cond true
        (u/strip-refs spec)

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
  (let [debug (log/info (str "doing step 1: generate seed tree of size " seed-tree-size " .."))
        seed-tree (-> spec
                      ((fn [tree]
                         ;; add to the tree until it's reached the desired size:
                         (add-until tree grammar index-fn syntax-tree seed-tree-size))))]
    (log/debug (str "doing step 2: generate trees based on step 1's seed tree: " (syntax-tree seed-tree)))
    (repeatedly #(-> seed-tree
                     (add-until-done grammar index-fn syntax-tree)
                     first))))
