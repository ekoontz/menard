(ns babylon.generate
  (:require
   [babylon.generate.truncate :as trunc]
   [clojure.tools.logging :as log]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.serialization :as s]))

;; the higher the constant below,
;; the more likely we'll first generate leaves
;; (terminal nodes) before trees.
;; set to 0 to always put trees before leaves.
;; set to a large X (e.g. 10 to (virtually) always put leaves before trees.
(def ^:const branching-factor 1000)

(def ^:dynamic morph (fn [x] "-no-morph-function-"))
(def ^:dynamic syntax-tree (fn [x] "[-no-syntax-tree-function-]"))
(def ^:dynamic truncate? true)

(defn phrasal-children-first? [depth]
  (let [result (= 0 (rand-int (+ depth branching-factor)))]
    (log/debug (str "branch at: " depth "? => " result))
    result))

;; you can experiment by modifying branching-factor and then run branching-samples
;; to see how many times out of 100 you'd branching trees before leaves.
(defn branching-samples []
  (count (filter #(= % true) (take 100 (repeatedly #(fn [] (phrasal-children-first?)))))))

(def ^:const max-depth 7)

(def ^:dynamic lexicon nil)
(def ^:dynamic index-fn (fn [spec]
                          (flatten (vals lexicon))))
(def ^:dynamic lexical-filter nil)

(declare fold-up)
(declare frontier)
(declare get-lexemes)
(declare grow)
(declare match-against-rules)
(declare generate-all)
(declare lazy-mapcat)
(declare terminate-up)
(declare truncate-in)

;; <new implementation>
(declare add-lexeme)
(declare add-rule)
(declare foldup)
(declare headness?)
(declare numeric-frontier)
(declare remove-trailing-comps)
(declare truncate-at)
(declare update-syntax-tree)
(def optimize? true)
(def ^:dynamic grammar (delay (throw (Exception. (str "no grammar supplied.")))))
(def ^:dynamic index-fn (fn [spec]
                          (throw (Exception. (str "no index-fn supplied.")))))
(def ^:dynamic lexicon (delay (throw (Exception. (str "no lexicon supplied.")))))
(def ^:dynamic syntax-tree (fn [tree]
                             (log/warn (str "using default syntax-tree function for tree "
                                            " rooted at: " (u/get-in tree [:rule])))))
;; </new implementation>


(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  [spec grammar]
  (first (generate-all spec grammar)))

(defn generate-all
  "Return the subset of _grammar_ that unfies with spec _spec_, and return the unified result for each member of that subset."
  [spec grammar]
  (->> (match-against-rules spec grammar)
       (lazy-mapcat (fn [tree]
                      (grow tree grammar)))))

(defn match-against-rules [spec grammar]
  (->> grammar
       (map (fn [grammar-rule]
              (unify grammar-rule spec)))
       (remove #(= % :fail))
       (map #(u/assoc-in! % [::started?] true))))

(declare handle-generation-failure)
(defn lazy-map [f items]
  (when (not (empty? items))
    (cons (f (first items))
          (lazy-seq (lazy-map f (rest items))))))

;; TODO: (grow) isn't really the greatest name. Rather than "growing" a
;; a tree you grow an entire forest using a single tree as input.
;; each member of the forest differs from the input tree by having a single
;; thing (either a phrase or a lexeme) added to it.
;; maybe 'tree-to-forest-by-addition' or something.
(defn grow
  "Recursively generate trees given input trees. continue recursively
   until no further expansion is possible."
  [^clojure.lang.PersistentArrayMap tree grammar]
  (let [frontier-path (frontier tree)
        depth (count frontier-path)]
    (log/debug (str "grow: " (syntax-tree tree) " at: " (vec frontier-path) "; #:" (count (str tree))))
    (let [retval
          (cond
            (empty? frontier-path) [tree]
            true
            (let [child-spec (u/get-in tree frontier-path :top)
                  child-lexemes (if (not (= true (u/get-in child-spec [:phrasal])))
                                  (get-lexemes child-spec))
                  child-trees (if (not (= false (u/get-in child-spec [:phrasal])))
                                (match-against-rules child-spec grammar))]
              (log/debug (str "lexical children: " (count child-lexemes)))
              (log/debug (str "phrasal children: " (count child-trees)))
              (log/debug (str "total children: " (count (concat child-trees child-lexemes))))

              (handle-generation-failure (syntax-tree tree) frontier-path child-spec
                                         (empty? child-lexemes) (empty? child-trees)
                                         (u/get-in child-spec [:phrasal])
                                         (u/get-in tree (concat (butlast frontier-path) [:rule])))
              (->>
               (cond
                 (>= depth max-depth) child-lexemes ;; max-depth has been reached: return only lexemes.
                 (phrasal-children-first? depth)
                 (lazy-cat child-trees child-lexemes) ;; order children which are trees before children which are leaves.
                 true
                 (lazy-cat child-lexemes child-trees)) ;; order children which are leaves before children which are trees.
               (lazy-map
                (fn [child]
                  (-> tree
                      (u/assoc-in frontier-path child)
                      (terminate-up frontier-path child))))
               (lazy-mapcat (fn [tree]
                              (grow tree grammar))))))]
      (log/debug (str "done growing; first result: " (if (not (empty? retval))
                                                       (syntax-tree (first retval)))))
      retval)))

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

(defn find-tail-path [tree path]
  (if (nil? (u/get-in tree path))
    path
    (find-tail-path tree (concat path [:rest]))))

(defn make-word []
   {:agr (atom :top)
    :canonical (atom :top)
    :exceptions (atom :top)
    :cat (atom :top)
    :infl (atom :top)
    :inflected? (atom :top)
    :root (atom :top)})

(def unify-morphology-leaf-leaf
  (let [one (make-word)
        two (make-word)]
    {:1 one
     :2 two
     :words {:first one
             :rest {:first two}}}))

(def unify-morphology-leaf-tree
  (let [one (make-word)
        two (atom :top)]
    {:1 one
     :2 {:words two}
     :words {:first one
             :rest two}}))

(def unify-morphology-fold
  (let [one (make-word)
        two (make-word)]
    {:1 one
     :2 {:1 two}
     :words {:first one
             :rest {:first two}}}))

(def unify-morphology-tree-leaf
  (make-word))

(defn create-words [tree frontier-path]
  (log/debug (str "creating words: " (syntax-tree tree) " at: " frontier-path))
  (let [retval
        (cond (and
               (= false (u/get-in tree (concat frontier-path [:1 :phrasal]) false))
               (= false (u/get-in tree (concat frontier-path [:2 :phrasal]) false)))
              ;; (1): both children at _frontier-path_ are leaves.
              (do
                (-> tree
                    (u/assoc-in frontier-path unify-morphology-leaf-leaf)))

              ;; (2) child :1 at _frontier-path_ is a leaf, child :2 is a tree.
              (and (= false (u/get-in tree (concat frontier-path [:1 :phrasal]) false))
                   (= true (u/get-in tree (concat frontier-path [:2 :phrasal]) false)))
              (do
                (log/debug (str "create-words(1)"))
                (-> tree
                    (u/assoc-in frontier-path unify-morphology-leaf-tree)))

              ;; (3): child :1 at _frontier-path_ is a tree; child :2 is a leaf.
              (and (= true (u/get-in tree (concat frontier-path [:1 :phrasal]) false))
                   (= false (u/get-in tree (concat frontier-path [:2 :phrasal]) false)))
              (let [tail-path (find-tail-path (u/get-in tree (concat frontier-path [:1 :words]))
                                              [])]
                (log/debug (str "create-words(3); tree is at: " (vec (concat frontier-path [:2]))
                                "; tail-path is: " (vec tail-path)))
                (->
                 tree
                 (u/assoc-in (concat frontier-path [:words])
                             (u/get-in tree (concat frontier-path [:1 :words])))
                 (u/assoc-in frontier-path
                             (unify {:2 unify-morphology-tree-leaf
                                     :words
                                     (s/create-path-in (concat tail-path [:first])
                                                       unify-morphology-tree-leaf)}))))

              ;; (4) both children at _frontier-path_ are trees.
              (and (= true (u/get-in tree (concat frontier-path [:1 :phrasal]) false)))
              (let [tail-path (find-tail-path (u/get-in tree (concat frontier-path [:1 :words]))
                                              [])]
                (log/debug (str "create-words(4)"))
                (-> tree
                    (u/assoc-in
                     (concat frontier-path [:words])
                     (u/assoc-in (u/get-in tree (concat frontier-path [:1 :words]))
                                 tail-path
                                 (u/get-in tree (concat frontier-path [:2 :words]))))))

              true
              (throw (ex-info
                      (str "create-words: each child must be either a lexeme or a tree."))))]
    (log/debug (str "done creating words."))
    retval))

(defn truncate [m]
  (-> (reduce (fn [m path]
                 (dissoc-in m path))
              m
              [[:comp] [:1]
               [:head] [:2]])
      (assoc :syntax-tree (syntax-tree m))))

(defn fold-up [tree path]
  ;; 
  ;; If tree T looks like:
  ;;                  T
  ;;                 ...
  ;;                  P
  ;;                 / \
  ;;        done -> H   C
  ;;                   / \ 
  ;;  @path: done -> H2   C2 <- not started yet
  ;;
  ;; then fold this up to:
  ;;                  T
  ;;                 ...
  ;;                  P'
  ;;                 / \
  ;;               H'   C2
  ;;
  (let [h-path (-> path butlast butlast (concat [:head]))
        h (u/get-in tree h-path)
        h2 (u/get-in tree path)]
    (log/info (str "h-path: " (vec h-path)))
    (log/info (str "fold up:"
                   "  H: " (syntax-tree (u/get-in tree h-path))
                   "; H2: " (syntax-tree (u/get-in tree path))
                   "; T: " (syntax-tree tree)))
    (if false
      (do
        (swap! (get (u/get-in tree h-path)
                    :subcat)
               (fn [x] (u/get-in tree (concat path [:subcat]))))
        (swap! (get (u/get-in tree h-path)
                    :comp)
               (fn [x] (u/get-in tree (concat path [:comp]))))
        tree)
      tree)))

(defn truncate-in
  "Truncate the value at path _path_ within _m_. if path is not empty, then 
  truncate-in expects: (get (u/get-in m (butlast path)) (last path)) to be an atom;
  otherwise it will throw an exception when trying to swap! that value."
  [m path]
  (if (not (empty? path))
    (do
      (swap! (get (u/get-in m (butlast path))
                  (last path))
             (fn [x] (truncate (u/get-in m path))))
      m)
    (truncate m)))

(defn terminate-up [tree frontier-path child]
  (log/debug (str "terminate-up: " (vec frontier-path) ": " (syntax-tree tree)))
  (cond
    ;;
    ;; in this case, H:word is a fold-up.
    ;;                   T
    ;;                  / \
    ;; (done) -> H:words   C <- @frontier-path (done)
    (and (= :comp (last frontier-path))
         (u/get-in tree (concat frontier-path [::done?]))
         (map? (u/get-in tree (concat (butlast frontier-path) [:words]))))
    (throw (Exception. (str "TODO.")))
    
    (and (= :comp (last frontier-path))
         (u/get-in tree (concat frontier-path [::done?])))
    ;;
    ;; If tree T looks like:
    ;; 
    ;;             T
    ;;            / \
    ;; (done) -> H   C <- @frontier-path (done)
    ;;
    ;; 
    ;; then truncate-in this to:
    ;;
    ;;             T'
    ;;
    (->
      tree
      (create-words (butlast frontier-path))
      ((fn [tree]
         (if truncate?
           (truncate-in tree (butlast frontier-path))
           tree)))
      (u/assoc-in! (concat (butlast frontier-path) [::done?]) true)
      (terminate-up (butlast frontier-path) child))

    (and (= (last frontier-path) :head)
         (not (empty? (butlast frontier-path)))
         (= :comp (last (butlast frontier-path))))
    ;; 
    ;; If tree T looks like:
    ;; 
    ;;                           T
    ;;                          / \
    ;;                 done -> H   C
    ;;                            / \ 
    ;;  @frontier-path: done -> H2   C2 <- not started yet
    ;;
    ;; then fold this up to:
    ;; 
    ;;                           T'
    ;;                          / \
    ;;                         H'  C2
    ;;
    (fold-up tree frontier-path)

    true
    (do
      (log/debug (str "done terminating: at:" (vec frontier-path)))
      (log/debug (str "done terminating: with done?:" (u/get-in tree [::done?] ::none)))
      tree)))

(defn frontier
  "get the next path to which to adjoin within _tree_, or empty path [], if tree is complete."
  [tree]
  (let [retval
         (cond
           (= (u/get-in tree [::done?]) true)
           []

           (= (u/get-in tree [::started?] false) false)
           []
           
           (= (u/get-in tree [:phrasal]) false)
           []
           
           (= ::none (u/get-in tree [:comp] ::none))
           []
           
           (= ::none (u/get-in tree [:head] ::none))
           (cons :comp (frontier (u/get-in tree [:comp])))
    
           (and (= (u/get-in tree [:phrasal] true) true)
                (= true (u/get-in tree [::started?]))
                (not (u/get-in tree [:head ::done?])))
           (cons :head (frontier (u/get-in tree [:head])))

           (and (= (u/get-in tree [:phrasal] true) true)
                (= true (u/get-in tree [::started?])))
           (cons :comp (frontier (u/get-in tree [:comp])))

           (and (= (u/get-in tree [:phrasal] true) true))
           [:head]
    
           true
           (throw (Exception. (str "could not determine frontier for this tree: " tree))))]
    retval))

(defn lazy-mapcat [f seqs]
  (if (not (empty? seqs))
     (lazy-cat
       (f (first seqs))
       (lazy-mapcat f (rest seqs)))
     []))

(defn handle-generation-failure [syntax-tree frontier-path child-spec child-lexemes-empty?
                                 child-trees-empty? child-spec-phrasal? immediate-parent]
  (cond (and (= false (u/get-in child-spec [:phrasal] true)) child-lexemes-empty?)
        (throw (ex-info
                (str "cannot grow this tree: " syntax-tree " at: " frontier-path " (no lexemes match)")
                {:tree syntax-tree
                 :immediate-parent immediate-parent
                 :why :no-lexemes-match
                 :frontier-path frontier-path
                 :max-depth max-depth
                 :child-spec child-spec}))

        (and (= true (u/get-in child-spec [:phrasal] false)) child-trees-empty?)
        (throw (ex-info
                (str "cannot grow this tree: " syntax-tree " at: " frontier-path " (no rules match)")
                {:tree syntax-tree
                 :immediate-parent immediate-parent
                 :why :no-rules-match
                 :frontier-path frontier-path
                 :max-depth max-depth
                 :child-spec child-spec}))
              
        (and child-lexemes-empty? child-trees-empty?)
        (throw (ex-info
                (str "cannot grow this tree: " syntax-tree " at: " frontier-path "; (no lexemes or rules match)")
                {:tree syntax-tree
                 :immediate-parent immediate-parent
                 :why :no-lexemes-or-rules-match
                 :frontier-path frontier-path
                 :max-depth max-depth
                 :child-spec child-spec}))
        
        (and child-lexemes-empty? child-trees-empty?)
        (throw (ex-info
                (str "cannot grow this tree: " syntax-tree " at: " frontier-path ". (max depth reached)")
                {:tree syntax-tree
                 :immediate-parent immediate-parent
                 :why :too-deep
                 :frontier-path frontier-path
                 :max-depth max-depth
                 :child-spec child-spec}))))



;; BEGIN new generation implementation:

(defn add [tree]
  (let [at (frontier tree)]
    (if (not (= tree :fail))
      (log/debug (str "adding to: " (syntax-tree tree) (str "; at:" at))))
    (cond
      (u/get-in tree [:babylon.generate/done?])
      [tree]
      (= tree :fail)
      []

      (or (u/get-in tree (concat at [:rule]))
          (= true (u/get-in tree (concat at [:phrasal]))))
      (do
        (log/debug (str "adding rule: " (syntax-tree tree) (str "; at:" at)))
        (add-rule tree))

      (or (= false (u/get-in tree (concat at [:phrasal])))
          (u/get-in tree (concat at [:canonical])))
      (do
        (log/debug (str "adding lexeme: " (syntax-tree tree) (str "; at:" at)))
        (add-lexeme tree))
    
      true
      (do (log/warn (str "slowness:" (syntax-tree tree) " at rule: " (u/get-in tree (concat (butlast at) [:rule])) " for child: " (last at) " due to need to generate for both rules *and* lexemes.."))
          (lazy-cat (add-lexeme tree)
                    (add-rule tree))))))

(defn add-lexeme [tree & [spec]]
  (let [at (frontier tree)
        done-at (concat (remove-trailing-comps at) [:babylon.generate/done?])
        spec (or spec :top)
        tree (u/assoc-in! tree done-at true)
        spec (u/unify! spec (u/get-in tree at))]
    (when (not (= spec :fail))
      (log/debug (str "add-lexeme: " (syntax-tree tree) " at: " at))
      (->> (get-lexemes (u/strip-refs spec))
           shuffle
           (remove #(when (and (not optimize?) (= :fail (u/assoc-in tree at %)))
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

(defn add-rule [tree & [rule-name]]
  (let [at (frontier tree)
        rule-name
        (cond rule-name rule-name
              (not (nil? (u/get-in tree (concat at [:rule])))) (u/get-in tree (concat at [:rule]))
              true nil)
        at-num (numeric-frontier (:syntax-tree tree {}))]
    (log/debug (str "add-rule: " (syntax-tree tree) "; " (if rule-name (str "adding rule: " rule-name ";")) " at: " at "; numerically: " at-num))
    (->> grammar
         (filter #(or (nil? rule-name) (= (:rule %) rule-name)))
         shuffle
         (lazy-map #(u/assoc-in % [:babylon.generate/started?] true))
         (remove #(when (and (not optimize?) (= :fail (u/assoc-in tree at %)))
                    (log/warn (str (syntax-tree tree) " failed to add rule:" (u/get-in % [:rule])
                                   " at: " at "; failed path(tree/rule):" (u/fail-path (u/get-in tree at) %)))
                    true))
         (remove #(= :fail %))
         (lazy-map #(u/assoc-in! (u/copy tree) at %))
         (lazy-map
          #(u/unify! %
                     (s/create-path-in (concat [:syntax-tree] at-num)
                                       (let [one-is-head? (headness? % (concat at [:1]))] 
                                         {:head? (= :head (last at))
                                          :1 {:head? one-is-head?}
                                          :2 {:head? (not one-is-head?)}
                                          :rule
                                          (do (log/debug (str "getting rule for: " (syntax-tree %) "; rule-name is: " rule-name))
                                              (or rule-name
                                                  (u/get-in % (concat at [:rule]))))})))))))

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

(defn remove-trailing-comps [at]
  (cond (empty? at) at
        (= :comp
           (last at))
        (remove-trailing-comps (butlast at))
        true at))

(defn truncate-at [tree at]
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
    tree))
   
;; fold up a tree like this:
;;
;;       grandparent
;;      /   \ C
;;   H /    parent
;;   uncle  / \
;;         /   \
;;      H /     \
;;      nephew   _ nephew's complement
;;
;; into:
;;
;;      grandparent
;;      /         \ C
;;   H /           \
;;    uncle+nephew   _
;;
(defn foldup [tree at]
  (let [parent-at (-> at butlast)
        parent (u/get-in tree parent-at)
        grandparent-at (-> parent-at butlast vec)
        grandparent (u/get-in tree grandparent-at)
        uncle-head-at (-> grandparent-at (concat [:head]) vec)
        nephew-at (-> parent-at (concat [:head]))
        nephew (u/get-in tree nephew-at)]
    (log/debug (str "checking for foldability: " (syntax-tree tree) " at: " (vec at)))
    (cond
      (and
       (not (nil? parent))
       (not (nil? grandparent))
       (not (empty? (u/get-in tree (concat uncle-head-at [:subcat :2]))))
       (not (empty? parent-at))
       (= (get parent :head)
          (get parent :1))
       (= (get grandparent :head)
          (get grandparent :1))
       (or (= (get (u/get-in nephew [:subcat]) :1)
              (get parent :comp))
           (= (get (u/get-in nephew [:subcat]) :2)
              (get parent :comp))))
      (let [raised-comp (u/get-in tree (concat parent-at [:comp]))]
        (log/debug (str "doing fold: " (syntax-tree tree) "; uncle at: " uncle-head-at " (" (u/get-in tree (concat uncle-head-at [:canonical]))
                       "); nephew at:" (vec nephew-at) " (" (u/get-in tree (concat nephew-at [:canonical])) ")"))
        (swap! (get (u/get-in tree (concat uncle-head-at [:subcat])) :2) (fn [old] raised-comp))
        (swap! (get (u/get-in tree grandparent-at) :comp) (fn [old] raised-comp))
        (log/debug (str "=== done folding: " (count (str tree)) "  ==="))
        (dissoc tree :dag_unify.serialization/serialized))
      true tree)))

