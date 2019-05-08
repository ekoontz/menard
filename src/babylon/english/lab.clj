(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze grammar parse]]
   [babylon.generate :as g :refer [lazy-mapcat]]
   [dag_unify.core :as u :refer [unify fail? ref? simplify-ref]]
   [dag_unify.serialization :as s :refer [all-refs]]
   [clojure.tools.logging :as log]))

(def specs
  [{:phrasal true
    :rule "np"
    :head {:rule "nbar4"
           :phrasal true
           :comp {:phrasal true
                  :comp {:phrasal true
                         :comp {:phrasal true}}}}}
   {:phrasal true
    :rule "s"
    :comp {:phrasal true
           :agr {:number :sing}
           :sem {:pred :dog}}
    :canonical "be"}])

(declare morph-1)
(declare syntax-tree-1)
(defn morph [tree]
  (morph-1 (u/get-in tree [:syntax-tree])))

(defn syntax-tree [tree]
  (cond (= :fail tree)
        tree
        true
        (str (syntax-tree-1 (u/get-in tree [:syntax-tree])) " (#" (count (str tree)) ")")))

(defn syntax-tree-1 [syntax-tree]
  (cond
    (nil? syntax-tree) "_"
    (u/get-in syntax-tree [:1])
    (str "["
         (:rule syntax-tree "?") " "
         (if (= true (u/get-in syntax-tree [:1 :head?]))
           "*" ".")
         (syntax-tree-1 (u/get-in syntax-tree [:1])) " "
         (if (= true (u/get-in syntax-tree [:2 :head?]))
           "*" ".")
         (syntax-tree-1 (u/get-in syntax-tree [:2]))
         "]")
    (u/get-in syntax-tree [:2])
    (str "["
         (:rule syntax-tree "?") " "
         (if (= true (u/get-in syntax-tree [:1 :head?]))
           "*" ".")
         "_ "
         (if (= true (u/get-in syntax-tree [:2 :head?]))
           "*" ".")
         (syntax-tree-1 (u/get-in syntax-tree [:2])) "]")
    true
    (en/morph syntax-tree)))


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

(defn gen
  "how to generate a phrase with particular constraints."
  [i]
  (let [expression (generate (nth specs i))]
      {:st (syntax-tree expression)
       :morph (morph expression)
       :agr (u/strip-refs (u/get-in expression [:head :agr]))
       ;;       :parses (map syntax-tree (parse (morph expression)))
       :phrase? (u/get-in expression [:head :comp :phrasal])}))

(def specific-sentence-spec
  [{:cat :verb
    :subcat []
    :pred :top
    :comp {:phrasal true
           :rule "np"
           :agr {:number :sing
                 :person :3rd}
           :head {:phrasal true
                  :rule "nbar2"
                  :head {:canonical "dog"}}}
    :head {:phrasal false
           :canonical "be"}}])

(def poetry-specs
  [
   {:cat :verb
    :phrasal true
    :subcat []
    :comp {:phrasal false}
    :head {:phrasal true
           :comp {:phrasal true}}}])

(defn poetry-line []
  (try
    (->
     poetry-specs
     shuffle
     first
     generate)
    (catch Exception e
      (log/warn (str "fail:(" (-> e ex-data :why) ":)"
                     (syntax-tree (:tree (ex-data e))) " with spec:"
                     (u/strip-refs (:child-spec (ex-data e))) "; at path:"
                     (:frontier-path (ex-data e)) "; immediate-parent: "
                     (-> e ex-data :immediate-parent))))))

(defn benchmark []
  (repeatedly
   #(time (->
           (or (poetry-line) "(failed)")
           morph
           println))))

(defn poetry []
  (loop []
    (println (morph (or (poetry-line) "(failed)") :sentence-punctuation? true))
    (recur)))

(defn long-s []
  (count
   (take 1
    (repeatedly
      #(println
         (morph
          (generate
           {:cat :verb
            :subcat []
            :pred :top
            :comp {:phrasal true
                   :head {:phrasal true}}
            :head {:phrasal true
                   :comp {:phrasal true
                          :head {:phrasal true}}}})))))))

(defn set-started [tree path]
  (if (not (empty? path))
    (set-started
     (u/assoc-in tree (concat path [:babylon.generate/started?]) true)
     (rest path))
    (u/assoc-in tree [:babylon.generate/started?] true)))

(defn set-done [tree path]
  (if (and (not (empty? path))
           (= :comp (last path)))
      (set-done
       (u/assoc-in tree (concat path [:babylon.generate/done?]) true)
       (butlast path))
    (u/assoc-in tree (concat path [:babylon.generate/done?]) true)))

                                                                         
(def flattened-lexicon
  (->>
   babylon.english/lexicon
   vals
   flatten
   (filter #(not (u/get-in % [:exception])))))

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
         (g/lazy-map #(u/assoc-in tree at %))
         (g/lazy-map #(set-started % at))
         (remove #(= :fail %))
         (g/lazy-map
          #(unify %
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
    (unify tree
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
      (log/info (str "truncating: " (syntax-tree tree) " at: " compless-at))
      (-> tree
           (g/dissoc-in compless-at)
           (g/dissoc-in (numeric-path tree compless-at))
           (dissoc :dag_unify.serialization/serialized)
           (u/assoc-in (concat compless-at [:babylon.generate/done?]) true)))
    tree))
   
;; fold up a tree like this:
;;
;;    s
;;   / \ 
;;  /   \ H
;;  _    vp-aux
;;      /   \ C
;;   H /    vp
;;   would  / \
;;         /   \
;;      H /     \
;;      see      _
;;
;; into:
;;
;;    s
;;   / \
;;  /   \ H
;;  _    vp-aux
;;      /      \
;;     / H      \
;;    would see  _
;;
(defn fold-at [tree at]
  (cond
    (and
     (= at [:head :comp :head])
     (= (get tree :head)
        (get tree :2))
     (= (get (u/get-in tree (butlast at)) :head)
        (get (u/get-in tree (butlast at)) :1)))
    (let [at [:head]
          raised-comp (u/get-in tree (concat at [:comp :comp]))]
      (log/info (str "doing fold: " (syntax-tree tree)))
      (swap! (get (u/get-in tree (concat at [:head :subcat])) :2) (fn [old] raised-comp))
      (swap! (get (u/get-in tree at) :comp) (fn [old] raised-comp))
      (dissoc tree :dag_unify.serialization/serialized))
    true tree))

(defn add-lexeme [tree & [spec]]
  (let [at (g/frontier tree)
        spec (or spec :top)
        spec (unify spec (u/get-in tree at))]
    (if (= spec :fail)
      []
      (do
        (log/debug (str "add-lexeme: spec: " (u/strip-refs spec)))
        (->> flattened-lexicon
             (remove #(= :fail (unify % spec)))
             shuffle
             (g/lazy-map #(u/assoc-in tree at %))
             (g/lazy-map #(set-done % at))
             (g/lazy-map #(update-syntax-tree % at))
             (g/lazy-map #(truncate-at % at))
             (g/lazy-map #(fold-at % at)))))))

(defn morph-1 [syntax-tree]
  (cond
    (nil? syntax-tree) "_"
    (u/get-in syntax-tree [:1])
    (str (morph-1 (u/get-in syntax-tree [:1])) " "
         (morph-1 (u/get-in syntax-tree [:2])))
    (u/get-in syntax-tree [:2])
    (str "_ "
         (morph-1 (u/get-in syntax-tree [:2])))
    true
    (en/morph syntax-tree)))

(defn add [tree]
  (let [frontier (g/frontier tree)]
    (cond
      (u/get-in tree [:babylon.generate/done?])
      [tree]
      (= tree :fail)
      []

      (or (u/get-in tree (concat frontier [:rule]))
          (u/get-in tree (concat frontier [:phrasal])))
      (do
        (log/info (str "adding rule: " (syntax-tree tree) (str "; frontier:" frontier)))
        (add-rule tree))

      (or (= false (u/get-in tree (concat frontier [:phrasal])))
          (u/get-in tree (concat frontier [:canonical])))
      (do
        (log/info (str "adding lexeme: " (syntax-tree tree) (str "; frontier:" frontier)))
        (add-lexeme tree))
    
      true
      (do
        (log/info (str "adding lexemes and rules: " (syntax-tree tree) (str "; frontier:" frontier)))
        (lazy-cat
         (add-lexeme tree)
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

(def quick-demo-spec
  {:rule "s" :comp {:phrasal false} :head {:phrasal false}})

(def demo-spec
  (-> {:rule "s"
       :comp {:rule "np"}
       :head {:rule "vp-aux"
              :comp {:rule "vp"
                     :comp {:rule "np"}}}}))
(defn demo []
  (repeatedly
   #(println
     (-> demo-spec
         generate
         morph))))

(defn timed-demo []
  (repeatedly
   #(println
     (-> demo-spec
         generate
         time
         syntax-tree))))

(defn quick-demo []
  (repeatedly #(println (morph (time (generate quick-demo-spec))))))

(defn partial-generate-test []
  (first
    (->> [demo-spec]
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)         
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)
         (lazy-mapcat add)
         (take 1))))
