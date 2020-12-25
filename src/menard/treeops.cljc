(ns menard.treeops
  (:require
   [menard.logging :as log]
   [menard.exception :refer [exception]]
   [dag_unify.core :as u]
   [dag_unify.diagnostics :as diag]
   [dag_unify.serialization :as s]))

(defn foldable?
  "determine whether the given _tree_ is foldable, if the given _at_ points to a nephew"
  [tree at syntax-tree]
  (let [st (syntax-tree tree)
        grandparent (u/get-in tree (-> at butlast butlast))
        parent (u/get-in tree (-> at butlast))
        cond1 (seq (-> at butlast butlast))
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
          
          :else (exception (str "should never get here: did you miss adding a cond-check in foldable?")))))

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
    (u/get-in tree [:menard.generate/done?]) tree
    
    (foldable? tree at syntax-tree)
    (let [grandparent (u/get-in tree (-> at butlast butlast))
          nephew-complement (u/get-in tree (-> at butlast (concat [:comp])))]
      (log/debug (str "folding    " at " " (syntax-tree tree)))
      (log/debug (str "nephew-complement: " (syntax-tree nephew-complement)))
      (swap! (get grandparent :comp)
             (fn [_] nephew-complement))
      tree)
    :else
    tree))

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

(defn make-word []
  {:agr (atom :top)
   :canonical (atom :top)
   :cat (atom :top)
   :definite? (atom :top)
   :exceptions (atom :top)
   :infl (atom :top)
   :inflected? (atom :top)
   :null? (atom :top)
   :root (atom :top)
   :sem (atom :top)})

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
    
    :else (exception (str "unhandled: " (diag/strip-refs syntax-tree)))))

(defn update-syntax-tree [tree at syntax-tree]
  (log/debug (str "updating syntax-tree:" (syntax-tree tree) " at: " at))
  (cond (= :fail tree)
        tree
        :else
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

    :else
    (cons :2 (numeric-path (u/get-in tree [(first at)]) (rest at)))))

(defn remove-trailing-comps [at]
  (cond (empty? at) at
        (= :comp
           (last at))
        (remove-trailing-comps (butlast at))
        :else at))

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
            :else
            (do
              (log/debug (str "type of head: " (get m head)))
              (cond
                (u/ref? (get m head))
                (do
                  (log/debug (str "doing swap!"))
                  (swap! (get m head)
                         (fn [_] v))
                  m)
                :else
                (do
                  (log/debug (str "doing default: " assoc))
                  (assoc m head v))))))
        :else
        (do
          (log/debug (str "HEAD(alone):" head))
          (dissoc m head))))
    m))

(defn truncate-at [tree at syntax-tree]
  (cond
    (= :fail tree)
    tree
    :else
    ;; TODO: also truncate :head at this point, too:
    (if (= :comp (last at))
      (let [compless-at (if (empty? (remove-trailing-comps at))
                          ;; in this case, we have just added the final :comp at the
                          ;; root of the tree, so simply truncate that:
                          [:comp]
                          
                          ;; otherwise, ascend the tree as high as there are :comps
                          ;; trailing _at_.
                          (remove-trailing-comps at))]
        (log/debug (str "truncate@: " compless-at " (Compless at) " (syntax-tree tree)))
        (log/debug (str "truncate@: " (numeric-path tree compless-at) " (Numeric-path at) " (syntax-tree tree)))
        (-> tree
            (dissoc-in compless-at)
            (dissoc-in (numeric-path tree compless-at))
            (u/assoc-in! (concat compless-at [:menard.generate/done?]) true)
            (dissoc-in (concat (butlast compless-at) [:head :subcat]))
            (dissoc-in (concat (butlast compless-at) [:head :derivation]))
            (dissoc-in (concat (butlast compless-at) [:head :sem]))
            (dissoc-in (concat (butlast compless-at) [:head :exceptions]))
            (dissoc-in (concat (butlast compless-at) [:1]))
            (dissoc-in (concat (butlast compless-at) [:2]))
            ((fn [tree]
               (log/debug (str "afterwards: " (syntax-tree tree) "; keys of path: " (vec (concat (butlast compless-at) [:head])) ": "
                               (keys (u/get-in tree (concat (butlast compless-at) [:head])))))
               true))))
      tree)))

