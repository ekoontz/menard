(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze generate grammar morph parse]]
   [babylon.generate :as g]
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

(defn syntax-tree-2 [syntax-tree]
  (cond
    (nil? syntax-tree) "_"
    (u/get-in syntax-tree [:1])
    (str "["
         (:rule syntax-tree "?") " "
         (if (= true (u/get-in syntax-tree [:1 :head?]))
           "*" ".")
         (syntax-tree-2 (u/get-in syntax-tree [:1])) " "
         (if (= true (u/get-in syntax-tree [:2 :head?]))
           "*" ".")
         (syntax-tree-2 (u/get-in syntax-tree [:2]))
         "]")
    (u/get-in syntax-tree [:2])
    (str "["
         (:rule syntax-tree "?") " "
         (if (= true (u/get-in syntax-tree [:1 :head?]))
           "*" ".")
         "_ "
         (if (= true (u/get-in syntax-tree [:2 :head?]))
           "*" ".")
         (syntax-tree-2 (u/get-in syntax-tree [:2])) "]")
    true
    (morph syntax-tree)))

(defn syntax-tree [tree]
  (cond (= :fail tree)
        tree
        true
        (str (syntax-tree-2 (u/get-in tree [:syntax-tree])) " (#" (count (str tree)) ")")))

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
           (morph :sentence-punctuation? true)
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

(defn add-rule [tree rule-name]
  (let [at (g/frontier tree)
        at-num (numeric-frontier (:syntax-tree tree {}))]
    (log/info (str "add-rule: " (syntax-tree tree) "; adding rule: " rule-name "; at: " at "; numerically: " at-num))
    (->> grammar
         (filter #(= (:rule %) rule-name))
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
                                       :rule rule-name})))))))


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
  (cond
    (not (= (last at) :comp))
    tree
    
    true
    (let [at (cond (empty? (butlast at))
                   [:comp]
                   true (butlast at))
          numeric-path (numeric-path tree at)]
      (log/info (str "truncate-at: " (syntax-tree tree) " at: " (vec at) "; numerically: " numeric-path))
      (-> tree
          (g/dissoc-in at)
          (g/dissoc-in numeric-path)
          (dissoc :dag_unify.serialization/serialized)))))

(defn add-lexeme [tree & [spec]]
  (let [at (g/frontier tree)
        spec (or spec :top)
        spec (unify spec (u/get-in tree at))]
    (if (not (= tree :fail))
      (log/debug (str "add-lexeme: adding to: " (syntax-tree tree) "(#" (count (str tree)) ") at:" at)))
    (if (= spec :fail)
      []
      (do
        (log/debug (str "add-lexeme: spec: " (u/strip-refs spec)))
        (->> flattened-lexicon
             (remove #(= :fail (unify % spec)))
             shuffle
             (g/lazy-map #(u/assoc-in tree at %))
             (g/lazy-map #(set-done % at))
             (g/lazy-map #(update-syntax-tree % at)))))))
;;             (g/lazy-map #(truncate-at % at)))))))

(defn morph-2 [syntax-tree]
  (cond
    (nil? syntax-tree) "_"
    (u/get-in syntax-tree [:1])
    (str (morph-2 (u/get-in syntax-tree [:1])) " "
         (morph-2 (u/get-in syntax-tree [:2])))
    (u/get-in syntax-tree [:2])
    (str "_ "
         (morph-2 (u/get-in syntax-tree [:2])))
    true
    (morph syntax-tree)))

(defn do-fold [tree at]
  (let [raised-comp (u/get-in tree (concat at [:comp :comp]))]
    (swap! (get (u/get-in tree (concat at [:head :subcat])) :2) (fn [old] raised-comp))
    (swap! (get (u/get-in tree at) :comp) (fn [old] raised-comp))
    (-> tree
        (dissoc :dag_unify.serialization/serialized))))

(defn morph-new [tree]
  (morph-2 (u/get-in tree [:syntax-tree])))

(defn generate-new []
  (->>
   ;; 1. start with a list containing a single empty tree:
   [{}]

   ;; 2. add an s:
   (g/lazy-mapcat #(add-rule % "s"))

   ;; 3. add auxiliary verb:
   ;;
   ;;    s
   ;;   / \ 
   ;;  /   \ H
   ;;  _    vp-aux
   ;;      /   
   ;;     / H   
   ;;   would  
   (g/lazy-mapcat #(add-rule % "vp-aux"))
   (g/lazy-mapcat add-lexeme)
   
   ;; 4. add vp->verb:
   ;;
   ;;    s
   ;;   / \ 
   ;;  /   \ H
   ;;  _    vp-aux
   ;;      /   \
   ;;     / H   vp(new)
   ;;   would  / \
   ;;         /   \
   ;;      H /     \
   ;;      see      _
   ;;
   (g/lazy-mapcat #(add-rule % "vp"))
   (g/lazy-mapcat add-lexeme)
   
   ;; 5. fold up tree from the above representation to:
   ;;    s
   ;;   / \
   ;;  /   \ H
   ;;  _    vp-aux
   ;;      /      \
   ;;     / H      \
   ;;    would see  _
   ;;
   (g/lazy-map #(do-fold % [:head]))
   
   ;; 6. add lower complement:
   (g/lazy-mapcat #(add-rule % "np"))
   (g/lazy-mapcat add-lexeme)))
;;   (g/lazy-mapcat add-lexeme)))

   ;; 7. add upper complement:
;;   (g/lazy-mapcat #(add-rule % "np"))
;;   (g/lazy-mapcat add-lexeme)
;;   (g/lazy-mapcat add-lexeme)))

(defn demo []
  (repeatedly #(println (morph-new (time (-> (generate-new) first))))))

