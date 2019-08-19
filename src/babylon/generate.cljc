(ns babylon.generate
  (:require
   [clojure.tools.logging :as log]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.serialization :as s]))

(declare add)
(declare add-lexeme)
(declare add-rule)
(declare dissoc-in)
(declare foldup)
(declare frontier)
(declare get-lexemes)
(declare headness?)
(declare lazy-map)
(declare lazy-mapcat)
(declare make-word)
(declare numeric-frontier)
(declare numeric-path)
(declare remove-trailing-comps)
(declare truncate-at)
(declare update-syntax-tree)

;; enable additional checks and logging that makes generation slower:
(def diagnostics? false)

(def ^:dynamic grammar (delay (throw (Exception. (str "no grammar supplied.")))))
(def ^:dynamic index-fn (fn [spec]
                          (throw (Exception. (str "no index-fn supplied.")))))
(def ^:dynamic lexical-filter nil)
(def ^:dynamic lexicon (delay (throw (Exception. (str "no lexicon supplied.")))))
(def ^:dynamic syntax-tree (fn [tree]
                             (log/warn (str "using default syntax-tree function for tree "
                                            " rooted at: " (u/get-in tree [:rule])))))

(defn report [tree]
  (str (syntax-tree tree) " #:" (count (str tree))))
  
(defn generate-all
  "Recursively generate trees given input trees. continue recursively
   until no further expansion is possible."
  [trees]
  (if (not (empty? trees))
    (let [tree (first trees)]
      (cond (= :fail tree)
            (throw (Exception. (str "generate-all: tree is unexpectedly :fail.")))

            (u/get-in tree [:babylon.generate/done?])
            (lazy-seq
              (cons tree
                    (generate-all (rest trees))))

            true
            (lazy-cat
             (generate-all (add tree))
             (generate-all (rest trees)))))))

(defn generate [^clojure.lang.PersistentArrayMap spec]
  (-> [spec] generate-all first))

(defn add [tree]
  (log/debug (str "add: " (report tree)))
  (let [at (frontier tree)
        rule-at (u/get-in tree (concat at [:rule]) ::none)
        phrase-at (u/get-in tree (concat at [:phrase]) ::none)
        lexness (u/get-in tree (concat at [:canonical]) ::none)]
    (log/debug (str "add: spec:" (u/strip-refs (u/get-in tree at))))
    (if (= :fail (u/get-in tree at))
      (throw (Exception. (str "add: value at: " at " is fail."))))
    (if (not (= tree :fail))
      (log/debug (str "add: tree=" (syntax-tree tree) (str "; at:" at " with spec: " (u/strip-refs (u/get-in tree at))))))
    (if (and (= false (u/get-in tree (concat at [:phrasal])))
             (not (= ::none (u/get-in tree (concat at [:rule]) ::none))))
      (throw (Exception. (str "add: phrasal is false but rule is specified: " (u/get-in tree (concat at [:rule]))))))
    (cond
      (u/get-in tree [:babylon.generate/done?])
      (do
        (log/debug (str "condition 1."))
        [tree])
      (= tree :fail)
      (throw (Exception. (str "add: tree is unexpectedly :fail.")))

      (or
       (and (not (= ::none rule-at))
            (not (= :top rule-at)))
       (and (not (= ::none phrase-at))
            (not (= :top phrase-at))))
      (do
        (log/debug (str "add: rule is set to: " rule-at))
        (add-rule tree (:rule rule-at) (= true phrase-at)))

      (or (= false (u/get-in tree (concat at [:phrasal])))
          (and (u/get-in tree (concat at [:canonical]))
               (not (= :top
                       (u/get-in tree (concat at [:canonical]))))))
      
      (do
        (log/debug (str "add: adding lexeme: " (syntax-tree tree) (str "; at:" at)))
        (let [result (add-lexeme tree)]
          (log/debug (str "add: added lexeme; result: " (syntax-tree tree)))
          (if (and (= false (u/get-in tree (concat at [:phrasal])))
                   (empty? result)
                   diagnostics?)
            (let [message
                  (str "no lexemes match for tree: " (syntax-tree tree)
                       " at: " (frontier tree)
                       "; lexeme spec: " (u/strip-refs (u/get-in tree (frontier tree))))]
              (if false
                (throw (Exception. (str "Unexpected: " message)))
                (log/warn message))))
          result))
      
      true
      (do (log/debug (str "slowness:" (syntax-tree tree) " at rule: "
                          (u/get-in tree (concat (butlast at) [:rule])) " for child: "
                          (last at) ", due to need to generate for both rules *and* lexemes."))
          (let [both
                (lazy-cat (add-lexeme tree)
                          (add-rule tree))]
            (if (empty? both)
              (throw (Exception. (str "dead end: " (syntax-tree tree) " at: " at))))
            both)))))

(defn add-lexeme [tree & [spec]]
  (log/debug (str "add-lexeme: " (report tree)))
  (let [at (frontier tree)
        done-at (concat (remove-trailing-comps at) [:babylon.generate/done?])
        spec (or spec :top)
        tree (u/assoc-in tree done-at true)
        spec (u/unify spec (u/get-in tree at))]
    (log/debug (str "add-lexeme: calculated spec."))
    (when (and (not (= spec :fail))
               (not (empty? at)))
      (log/debug (str "add-lexeme: " (syntax-tree tree) " at: " at " with spec:" (u/strip-refs spec)))
      (->> (get-lexemes (u/strip-refs spec))
           shuffle
           (remove #(when (and diagnostics? (= :fail (u/assoc-in tree at %)))
                      (log/warn (str (syntax-tree tree) " failed to add lexeme: " (u/get-in % [:canonical])
                                     " at: " at "; failed path:" (u/fail-path (u/get-in tree at) %)))
                      true))
           (lazy-map (fn [candidate-lexeme]
                       (log/debug (str "adding lexeme: " (u/get-in candidate-lexeme [:canonical])))
                       (u/assoc-in! (u/copy tree) at candidate-lexeme)))
           (remove #(= :fail %))
           (lazy-map #(update-syntax-tree % at))
           (lazy-map #(truncate-at % at))
           (lazy-map #(foldup % at))))))

(defn update-syntax-tree [tree at]
  (log/debug (str "updating syntax-tree:" (syntax-tree tree) " at: " at))
  (let [head? (headness? tree at)
        ;; ^ not sure if this works as expected, since _tree_ and (:syntax-tree _tree) will differ
        ;; if folding occurs.
        numerically-at (numeric-frontier (u/get-in tree [:syntax-tree]))
        word (merge (make-word)
                    {:head? head?})]
    (log/debug (str "update-syntax-tree: at: " at "; numerically-at:" numerically-at))
    (u/unify! tree
              (merge (s/create-path-in (concat [:syntax-tree] numerically-at) word)
                     (s/create-path-in at word)))))

(defn get-lexemes
  "Get lexemes matching the spec. Use index, where the index 
   is a function that we call with _spec_ to get a set of lexemes
   that matches the given _spec_."
  [spec]
  (if (= true (u/get-in spec [:phrasal]))
    (lazy-seq [])
    (do
      (let [retval
            (->> (index-fn spec)
                 lazy-seq
                 (filter #(and (or (nil? lexical-filter) (lexical-filter %))))
                 (map #(unify % spec))
                 (filter #(not (= :fail %)))
                 (map #(u/assoc-in! % [::done?] true)))]
        ;; This empty?-check is a workaround because without it, retval will be empty
        ;; if (get-lexemes) is called from outside this namespace with bindings on lexicon and index-fn.
        ;; Not sure where the bug is (might be my code somewhere or even in Clojure itself).
        (if (empty? retval) retval retval)))))

(defn add-rule [tree & [rule-name some-rule-must-match?]]
  (log/debug (str "add-rule: " (report tree)))
  (let [at (frontier tree)
        rule-name
        (cond rule-name rule-name
              (not (nil? (u/get-in tree (concat at [:rule])))) (u/get-in tree (concat at [:rule]))
              true nil)
        cat (u/get-in tree (concat at [:cat]))
        matching-rules (->> grammar
                            (filter #(or (nil? rule-name) (= (u/get-in % [:rule]) rule-name)))
                            (filter #(or (nil? cat) (= (u/get-in % [:cat]) cat))))
        at-num (numeric-frontier (:syntax-tree tree {}))]
    (log/debug (str "add-rule: " (syntax-tree tree) "; " (if rule-name (str "adding rule: " rule-name ";")) " at: " at "; numerically: " at-num))
    (log/debug (str "add-rule: tree: " (syntax-tree tree) " at:" (frontier tree)
                    "; number of matching rules: " (count matching-rules) ": ("
                    (clojure.string/join ","
                                         (map #(u/get-in % [:rule])
                                              (take 5 matching-rules)))
                    (if (< 5 (count matching-rules)) ",..")
                    ")"))
    (if (and (empty? matching-rules)
             (not (nil? rule-name)))
      (throw (Exception. (str "add-rule: no rules matched rule-name '" rule-name "'."))))
    (if (and (empty? matching-rules)
             some-rule-must-match?)
      (throw (Exception. (str "add-rule: no rules matched but we were given {:phrasal true} in the spec."))))
    (->> matching-rules
         shuffle
         (lazy-map #(u/assoc-in % [:babylon.generate/started?] true))
         (remove #(when (and diagnostics?
                             (= :fail (u/assoc-in tree at %)))
                    (log/warn (str (syntax-tree tree) " failed to add rule:" (u/get-in % [:rule])
                                   " at: " at "; failed path(tree/rule):" (u/fail-path (u/get-in tree at) %)))
                    true))
         (remove #(= :fail %))
         (lazy-map #(u/assoc-in! (u/copy tree) at %))
         (remove #(= :fail %))
         (lazy-map
          #(u/unify! %
                     (s/create-path-in (concat [:syntax-tree] at-num)
                                       (let [one-is-head? (headness? % (concat at [:1]))] 
                                         {:head? (= :head (last at))
                                          :1 {:head? one-is-head?}
                                          :2 {:head? (not one-is-head?)}
                                          :variant (u/get-in % [:variant])
                                          :rule
                                          (do (log/debug (str "getting rule for: " (syntax-tree %) "; rule-name is: " rule-name))
                                              (or rule-name
                                                  (u/get-in % (concat at [:rule]))))})))))))

(defn make-word []
  {:agr (atom :top)
   :canonical (atom :top)
   :exceptions (atom :top)
   :cat (atom :top)
   :infl (atom :top)
   :inflected? (atom :top)
   :root (atom :top)})

(defn frontier
  "get the next path to which to adjoin within _tree_, or empty path [], if tree is complete."
  [tree]
  
  (let [retval
        (cond
          (= :fail tree)
          (throw (Exception. (str "input tree was :fail.")))
          
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
          (throw (Exception. (str "could not determine frontier for this tree: " tree))))]
    retval))

(defn truncate-at [tree at]
  (let [parent-at (-> at butlast)
        parent (u/get-in tree parent-at)
        grandparent-at (-> parent-at butlast vec)
        grandparent (u/get-in tree grandparent-at)
        uncle-head-at (-> grandparent-at (concat [:head]) vec)
        nephew-at (-> parent-at (concat [:head]))
        nephew (u/get-in tree nephew-at)]
    (if (= :comp (last at))
      (let [compless-at (if (empty? (remove-trailing-comps at))
                          ;; in this case, we have just added the final :comp at the
                          ;; root of the tree, so simply truncate that.
                          [:comp]
                          ;; otherwise, ascend the tree as high as there are :comps
                          ;; trailing _at_.
                          (remove-trailing-comps at))]
        (log/debug (str "truncating: " (syntax-tree tree) " at: " compless-at))
        (-> tree
            (dissoc-in compless-at)
            (dissoc-in (numeric-path tree compless-at))
            (dissoc :dag_unify.serialization/serialized)
            (u/assoc-in! (concat compless-at [:babylon.generate/done?]) true)))
      tree)))

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
  [tree at]
  (let [st (syntax-tree tree)
        grandparent (u/get-in tree (-> at butlast butlast))
        parent (u/get-in tree (-> at butlast))
        uncle (u/get-in grandparent [:head])
        cond1 (not (empty? (-> at butlast butlast)))
        cond2 (= (get parent :head)
                 (get parent :1))
        cond3 (= (get grandparent :head)
                 (get grandparent :1))]
    (cond (and cond1 cond2 cond3)
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
          true (throw (Exception. (str "should never get here: did you miss adding a cond-check in foldable?"))))))

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
(defn foldup [tree at]
  (cond
    (u/get-in tree [::done?]) tree
    
    (foldable? tree at)
    (let [grandparent (u/get-in tree (-> at butlast butlast))
          nephew-complement (u/get-in tree (-> at butlast (concat [:comp])))]
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

    (nil? syntax-tree) :done

    (and (map? syntax-tree)
         (-> syntax-tree :1 :head?))
    (cons :1 (numeric-frontier (-> syntax-tree :1)))

    (and (map? syntax-tree)
         (-> syntax-tree :2 :head?))
    (cons :2 (numeric-frontier (-> syntax-tree :2)))
    
    true (throw (Exception. (str "unhandled: " (u/strip-refs syntax-tree))))))

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
    (cond
      tail
      (let [v (dissoc-in (u/get-in m [head]) tail)]
        (cond
          (empty? v)
          (dissoc m head)
          true
          (assoc m head v)))
      true
      (dissoc m head))
    m))

(defn lazy-map [f items]
  (when (not (empty? items))
    (cons (f (first items))
          (lazy-seq (lazy-map f (rest items))))))

