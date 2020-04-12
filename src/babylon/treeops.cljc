(ns babylon.treeops
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [babylon.exception :refer [exception]]
   [babylon.serialization :as ser]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.diagnostics :as diag]
   [dag_unify.serialization :as s]
   [dag_unify.dissoc :as d]))

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
    (u/get-in tree [:babylon.generate/done?]) tree
    
    (foldable? tree at syntax-tree)
    (let [grandparent (u/get-in tree (-> at butlast butlast))
          nephew-complement (u/get-in tree (-> at butlast (concat [:comp])))]
      (log/debug (str "folding    " at " " (syntax-tree tree)))
      (log/debug (str "nephew-complement: " (syntax-tree nephew-complement)))
      (swap! (get grandparent :comp)
             (fn [old] nephew-complement))
      (dissoc tree :dag_unify.serialization/serialized))
    true
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
   :exceptions (atom :top)
   :cat (atom :top)
   :infl (atom :top)
   :sem (atom :top)
   :inflected? (atom :top)
   :root (atom :top)})

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
    
    true (exception (str "unhandled: " (diag/strip-refs syntax-tree)))))

(defn update-syntax-tree [tree at syntax-tree]
  (log/debug (str "updating syntax-tree:" (syntax-tree tree) " at: " at))
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

