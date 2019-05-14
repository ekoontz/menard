(ns babylon.english.generate
  (:require
   [babylon.english :as en :refer [grammar]]
   [babylon.english.serialization :refer [syntax-tree]]
   [babylon.generate :as g]
   [dag_unify.serialization :as s]
   [dag_unify.core :as u]
   [clojure.tools.logging :as log]))

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

(declare generate)

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

(def optimize? true)

(defn add-rule [tree & [rule-name]]
  (let [at (g/frontier tree)
        rule-name
        (cond rule-name rule-name
              (not (nil? (u/get-in tree (concat at [:rule])))) (u/get-in tree (concat at [:rule]))
              true nil)
        at-num (numeric-frontier (:syntax-tree tree {}))]
    (log/debug (str "add-rule: " (syntax-tree tree) "; " (if rule-name (str "adding rule: " rule-name ";")) " at: " at "; numerically: " at-num))
    (->> grammar
         (filter #(or (nil? rule-name) (= (:rule %) rule-name)))
         shuffle
         (g/lazy-map #(u/assoc-in % [:babylon.generate/started?] true))
         (remove #(when (and (not optimize?) (= :fail (u/assoc-in tree at %)))
                    (log/warn (str (syntax-tree tree) " failed to add rule:" (u/get-in % [:rule])
                                   " at: " at "; failed path(tree/rule):" (u/fail-path (u/get-in tree at) %)))
                    true))
         (g/lazy-map #(u/assoc-in tree at %))
         (g/lazy-map
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

(defn update-syntax-tree [tree at]
  (log/debug (str "updating syntax-tree:" (syntax-tree tree) " at: " at))
  (let [head? (headness? tree at)
        ;; ^ not sure if this works as expected, since _tree_ and (:syntax-tree _tree) will differ
        ;; if folding occurs.
        numerically-at (numeric-frontier (u/get-in tree [:syntax-tree]))
        word (merge (g/make-word)
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
           (g/dissoc-in compless-at)
           (g/dissoc-in (numeric-path tree compless-at))
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

(defn add-lexeme [tree & [spec]]
  (let [at (g/frontier tree)
        done-at (concat (remove-trailing-comps at) [:babylon.generate/done?])
        spec (or spec :top)
        tree (u/assoc-in! tree done-at true)
        spec (u/unify! spec (u/get-in tree at))]
    (when (not (= spec :fail))
      (log/debug (str "add-lexeme: " (syntax-tree tree) " at: " at))
      (->> (binding [g/lexicon babylon.english/lexicon
                     g/index-fn
                     (fn [spec]
                       (cond (= (u/get-in spec [:cat]) :verb)
                             (shuffle babylon.english/verb-lexicon)
                             true
                             (shuffle babylon.english/non-verb-lexicon)))]
             (g/get-lexemes (u/strip-refs spec)))
           shuffle
           (remove #(when (and (not optimize?) (= :fail (u/assoc-in tree at %)))
                      (log/warn (str (syntax-tree tree) " failed to add lexeme: " (u/get-in % [:canonical])
                                     " at: " at "; failed path:" (u/fail-path (u/get-in tree at) %)))
                      true))
           (g/lazy-map (fn [candidate-lexeme]
                         (log/debug (str "adding lexeme: " (u/get-in candidate-lexeme [:canonical])))
                         (u/assoc-in! (u/copy tree) at (u/copy candidate-lexeme))))
           (remove #(= :fail %))
           (g/lazy-map #(update-syntax-tree % at))
           (g/lazy-map #(truncate-at % at))
           (g/lazy-map #(foldup % at))))))

(defn add [tree]
  (let [at (g/frontier tree)]
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
      (do (log/warn (str "slowness at rule: " (u/get-in tree (concat (butlast at) [:rule])) " for child " (last at) " due to need to generate for both rules *and* lexemes.."))
          (lazy-cat (add-lexeme tree)
                    (add-rule tree))))))

(defn generate-all [trees]
  (if (not (empty? trees))
    (let [tree (first trees)]
      (if (u/get-in tree [:babylon.generate/done?])
        (cons tree
              (generate-all (rest trees)))
        (lazy-cat
         (generate-all (add tree))
         (generate-all (rest trees)))))))

(defn generate [spec]
   (-> [spec] generate-all first))

