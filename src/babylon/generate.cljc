(ns babylon.generate
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [babylon.exception :refer [exception]]
   [babylon.serialization :as ser]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.serialization :as s]))

(declare add)
(declare add-lexeme)
(declare add-rule)
(declare dissoc-in)
(declare foldup)
(declare frontier)
(declare generate-all)
(declare get-lexemes)
(declare headness?)
(declare lazy-map)
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
;; TODO: add allow-truncation?
(def allow-folding? true)
(def ^:dynamic generate-only-one? true)
(def ^:dynamic allow-backtracking? false)
(def ^:dynamic lexical-filter nil)

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

(defn report [tree syntax-tree]
  (str "#" (count (str tree)) " " (syntax-tree tree)))

(def count-adds (atom 0))

(defn generate [spec grammar lexicon-index-fn syntax-tree-fn]
  (reset! count-adds 0)
  (let [result
        (first (generate-all [spec] grammar lexicon-index-fn syntax-tree-fn))]
    (log/debug (str "generated: " (report result syntax-tree-fn) " with: "
                   @count-adds " add" (if (not (= @count-adds 1)) "s" "")))
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
             (generate-all (add tree grammar lexicon-index-fn syntax-tree-fn) grammar lexicon-index-fn syntax-tree-fn)
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
      (log/debug (str (report tree syntax-tree-fn) " add at:" at " with spec: "
                      (summary-fn spec) " with phrasal: " (u/get-in tree (concat at [:phrasal]) ::none))))
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
         (log/debug (str "condition 1."))
         [tree])
       (= tree :fail)
       (exception (str "add: tree is unexpectedly :fail."))

       (or
        (and (not (= ::none rule-at))
             (not (= :top rule-at)))
        (and (not (= ::none phrase-at))
             (not (= :top phrase-at))))
       (do
         (log/debug (str "add: rule is set to: " rule-at))
         (add-rule tree grammar syntax-tree-fn (:rule rule-at) (= true phrase-at)))

       (= true (u/get-in tree (concat at [:phrasal])))
       (let [result
             (add-rule tree grammar syntax-tree-fn)]
         (if (and diagnostics? (empty? result))
           (log/warn (str "no rules matched spec: " (u/strip-refs spec) ": dead end.")))
         result)

       (or (= false (u/get-in tree (concat at [:phrasal])))
           (and (u/get-in tree (concat at [:canonical]))
                (not (= :top
                        (u/get-in tree (concat at [:canonical]))))))

       (do
         (log/debug (str "add: only adding lexemes at: " at))
         (let [result (add-lexeme tree lexicon-index-fn syntax-tree-fn)]
           (log/debug (str "add: added lexeme; result: " (vec (map syntax-tree-fn result))))
           (if (and (= false (u/get-in tree (concat at [:phrasal])))
                    (empty? result)
                    diagnostics?)
             (let [message
                   (str "no lexemes match for tree: " (syntax-tree-fn tree)
                        " at: " (frontier tree)
                        "; lexeme spec: " (u/strip-refs (u/get-in tree (frontier tree))))]
               (when die-on-no-matching-lexemes?
                 (log/error message)
                 (exception message))
               (log/warn message)))
           result))

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
               (exception (str "dead end: " (syntax-tree-fn tree) " at: " at))

               true both)))
     (filter #(reflexive-violations % syntax-tree-fn)))))

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

(defn get-lexemes
  "Get lexemes matching the spec. Use index, where the index 
   is a function that we call with _spec_ to get a set of lexemes
   that matches the given _spec_."
  [spec lexicon-index-fn syntax-tree]
  ;; this initial copying and strip-refs of _spec_ improves performance of (unify) below; since _spec_ will
  ;; now: have no reentrances, and have its serialized form stored within it, so the u/copy of it that is
  ;; done as part of the unify below will be O(1).
  (let [spec (u/copy (u/strip-refs spec))]
    (->> (lexicon-index-fn spec)
         (map #(unify % spec))
         (filter #(not (= :fail %)))
         (filter #(or (nil? lexical-filter) (lexical-filter %)))
         (#(do
             (log/debug (str "get-lexeme: found this many lexemes:" (count %)))
             (if (not (empty? %))
               (log/debug (str "get-lexeme: " 
                               (cond (= 1 (count %))
                                     "only one "
                                     true "first of " (count %) " lexemes ")
                               "found: '" (syntax-tree (first %)) "'")))
             %)))))

(defn add-lexeme [tree lexicon-index-fn syntax-tree]
  (log/debug (str "add-lexeme: " (report tree syntax-tree)))
  (let [at (frontier tree)
        done-at (concat (remove-trailing-comps at) [:babylon.generate/done?])
        spec (u/get-in tree at)
        diagnose? false]
    (log/debug (str "add-lexeme: " (report tree syntax-tree) " at: " at " with spec:" (summary-fn spec) "; generate-only-one? " generate-only-one?))
    (if (= true (u/get-in spec [:phrasal]))
      (exception (str "don't call add-lexeme with phrasal=true! fix your code!"))
      (->> (get-lexemes spec lexicon-index-fn syntax-tree)

           ((fn [lexemes]
              (cond
                generate-only-one? (take 1 lexemes)
                true lexemes)))
           (lazy-map (fn [candidate-lexeme]
                       (log/debug (str "adding lex: '"  (u/get-in candidate-lexeme [:canonical]) "'"
                                      " at: " at " to: " (report tree syntax-tree)))
                       (-> tree
                           ((fn [tree]
                              (cond generate-only-one? tree
                                    true (u/copy tree))))
                           (u/assoc-in! done-at true)
                           (u/assoc-in! at candidate-lexeme)
                           (update-syntax-tree at syntax-tree)
                           (truncate-at at syntax-tree)
                           (foldup at syntax-tree)
                           (#(do
                               (if (= :fail %)
                                 (log/warn (str "failed to add '" (u/get-in candidate-lexeme [:canonical]) "'"))
                                 (log/debug (str "successfully added lexeme: '" (u/get-in candidate-lexeme [:canonical]) "': " (syntax-tree %))))
                               %)))))))))

(defn add-rule [tree grammar syntax-tree & [rule-name some-rule-must-match?]]
  (log/debug (str "add-rule: " (report tree syntax-tree)))
  (let [at (frontier tree)
        rule-name
        (cond rule-name rule-name
              (not (nil? (u/get-in tree (concat at [:rule])))) (u/get-in tree (concat at [:rule]))
              true nil)
        cat (u/get-in tree (concat at [:cat]))
        at-num (numeric-frontier (:syntax-tree tree {}))]
    (log/debug (str "add-rule: @" at ": " (if rule-name (str "'" rule-name "'")) ": " (report tree syntax-tree)
                   " at: " at " (numerically): " at-num))
    (->>
     ;; start with the whole grammar:
     grammar

     ;; if a :rule is supplied, then filter out all rules that don't have this name:
     (filter #(or (nil? rule-name) (= (u/get-in % [:rule]) rule-name)))

     ;; if a :cat is supplied, then filter out all rules that specify a different :cat :
     (filter #(or (nil? cat) (= cat :top) (= :top (u/get-in % [:cat] :top)) (= (u/get-in % [:cat]) cat)))
     
     ;; do the actual adjoining of the child within the _tree_'s path _at_:
     (map #(u/assoc-in! (u/copy tree)
                        at %))

     ;; some attempts to adjoin will have failed, so remove those:
     (remove #(= :fail %))
     
     (map
      #(u/unify! %
                 (s/create-path-in (concat [:syntax-tree] at-num)
                                   (let [one-is-head? (headness? % (concat at [:1]))]
                                     {:head? (= :head (last at))
                                      :1 {:head? one-is-head?}
                                      :2 {:head? (not one-is-head?)}
                                      :variant (u/get-in % [:variant])
                                      :rule
                                      (or rule-name
                                          (u/get-in % (concat at [:rule])))})))))))

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
          
          true
          (exception (str "could not determine frontier for this tree: " tree)))]
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

(defn lazy-map [f items]
  (when (not (empty? items))
    (cons (f (first items))
          (lazy-seq (lazy-map f (rest items))))))


;; TODO: move this to a ^:dynamic: variable so it can
;; be customized per-language.
(defn summary-fn [spec]
  (cond (u/get-in spec [:rule])
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
             (= :top (u/get-in expression [:reflexive])))
         (or
             (= ::unspec (:ref (u/get-in expression [:sem :subj]) ::unspec))
             (= ::unspec (:ref (u/get-in expression [:sem :obj]) ::unspec))                         
             (not (= (:ref (u/get-in expression [:sem :subj]))
                     (:ref (u/get-in expression [:sem :obj]))))))
      (and
        (= true (u/get-in expression [:reflexive] false))
        (= (:ref (u/get-in expression [:sem :subj]))
           (:ref (u/get-in expression [:sem :obj]))))))
