(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze generate grammar morph parse syntax-tree]]
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
  (let [at (g/frontier tree)]
    (->> grammar
         (filter #(= (:rule %) rule-name))
         shuffle
         (g/lazy-map #(u/assoc-in tree at %))
         (g/lazy-map #(set-started % at))
         (remove #(= :fail %))
         (g/lazy-map
          #(unify %
                  (s/create-path-in (concat [:syntax-tree] (numeric-path tree at))
                                    (let [one-is-head? (headness? % (concat at [:1]))] 
                                      {:head? (= :head (last at))
                                       :1 {:head? one-is-head?}
                                       :2 {:head? (not one-is-head?)}
                                       :rule rule-name})))))))

(defn numeric-frontier [syntax-tree]
  ;; TODO: this is a mess; was written by trial-and-error: rewrite cleanly.
  (cond
    (nil? syntax-tree)
    []
    (:canonical syntax-tree) ;; we hit a leaf.
    []

    (-> syntax-tree :2 :done?)
    (cons :1 (-> syntax-tree :1 numeric-frontier))

    (and (nil? (-> syntax-tree :1 :canonical))
         (nil? (-> syntax-tree :1 :rule))
         (-> syntax-tree :1 :head?))
    (cons :1 (-> syntax-tree :2 numeric-frontier))

    (and (-> syntax-tree :1 :canonical)
         (-> syntax-tree :1 :head?))
    (cons :2 (-> syntax-tree :2 numeric-frontier))

    (and (nil? (-> syntax-tree :1 :canonical))
         (-> syntax-tree :2 :rule))
    (cons :2 (-> syntax-tree :2 numeric-frontier))

    (and (nil? (-> syntax-tree :1 :canonical))
         (-> syntax-tree :1 :head?))
    (cons :1 (-> syntax-tree :1 numeric-frontier))
    
    true nil))

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

(defn syntax-tree-new [tree]
  (syntax-tree-2 (u/get-in tree [:syntax-tree])))

(defn add-word-at [tree at]
  (let [head? (headness? tree at)
        numerically-at (numeric-frontier (u/get-in tree [:syntax-tree]))
        word (merge (g/make-word)
                    {:head? head?})]
    (unify tree
           (merge (s/create-path-in (concat [:syntax-tree] numerically-at) word)
                  (s/create-path-in at word)))))

(defn add-lexeme [tree & [spec]]
  (let [at (g/frontier tree)
        spec (or spec :top)
        spec (unify spec (u/get-in tree at))]
    (if (not (= tree :fail))
      (log/debug (str "adding to: " (syntax-tree-new tree) "(#" (count (str tree)) ") at:" at)))
    (if (= spec :fail)
      []
      (do
        (log/debug (str "spec: " (u/strip-refs spec)))
        (->> flattened-lexicon
             (remove #(= :fail (unify % spec)))
             shuffle
             (g/lazy-map #(u/assoc-in tree at %))
             (g/lazy-map #(set-done % at))
             (g/lazy-map #(add-word-at % at)))))))

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
  (let [raised-comp (u/get-in tree (concat at [:comp :comp]))
        upper-head (u/get-in tree (concat at [:head]))
        raised-head (u/get-in tree (concat at [:comp :head]))]
    (swap! (get (u/get-in tree (concat at [:head :subcat])) :2) (fn [old] raised-comp))
    (swap! (get (u/get-in tree at) :comp) (fn [old] raised-comp))
    (-> tree
        (dissoc :dag_unify.serialization/serialized))))

(defn morph-new [tree]
  (morph-2 (u/get-in tree [:syntax-tree])))

(defn truncate-at [tree at]
  (let [numeric-path (numeric-path tree at)]
    (-> tree
        (g/dissoc-in at)
        (g/dissoc-in numeric-path)
        (dissoc :dag_unify.serialization/serialized))))

(defn terminate-at [tree at]
  (-> tree
      (u/assoc-in (concat [:syntax-tree] (numeric-path tree at) [:done?])
                  true)))
(defn demo []
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
   (g/lazy-mapcat #(add-lexeme %))
   
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
   (g/lazy-mapcat add-lexeme)
   (g/lazy-map #(terminate-at % [:head]))
   (g/lazy-map #(truncate-at % [:head]))

   ;; 7. add upper complement:
   (g/lazy-mapcat add-lexeme)
   (g/lazy-map #(truncate-at % [:comp]))))
