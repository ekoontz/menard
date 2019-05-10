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
         (g/lazy-map #(u/assoc-in % [:babylon.generate/started?] true))
         (g/lazy-map #(u/assoc-in tree at %))
         (remove #(= :fail %))
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
;;      vp-aux
;;      /   \ C
;;   H /    vp
;;   would  / \
;;         /   \
;;      H /     \
;;      see      _
;;
;; into:
;;
;;      vp-aux
;;      /     \ C
;;     / H     \
;;    would see  _
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
       (not (empty? parent-at))
       (get grandparent :head)
       (= (get parent :head)
          (get parent :1))
       (= (get grandparent :head)
          (get grandparent :1)))
      (let [raised-comp (u/get-in tree (concat parent-at [:comp]))]
        (log/debug (str "doing fold: " (syntax-tree tree) " uncle-head-at:" uncle-head-at "; at:" (vec at)))
        (log/debug (str "parent-at:" parent-at))
        (log/debug (str "grandparent-at:" grandparent-at))
        (log/debug (str "subcatness(1): " (= (get (u/get-in nephew [:subcat]) :1)
                                            (get parent :comp))))
        (log/debug (str "subcatness(2): " (= (get (u/get-in nephew [:subcat]) :2)
                                            (get parent :comp))))
        (cond (= (get (u/get-in nephew [:subcat]) :1)
                 (get parent :comp))
              (swap! (get (u/get-in tree (concat uncle-head-at [:subcat])) :1) (fn [old] raised-comp))
              (= (get (u/get-in nephew [:subcat]) :2)
                 (get parent :comp))
              (swap! (get (u/get-in tree (concat uncle-head-at [:subcat])) :2) (fn [old] raised-comp))
              true (throw (Exception. (str "unhandled subcat scenario between nephew head and its complement sibling."))))
        (swap! (get (u/get-in tree grandparent-at) :comp) (fn [old] raised-comp))
        (log/debug (str "=== done folding: " (count (str tree)) "  ==="))
        (dissoc tree :dag_unify.serialization/serialized))
      true tree)))

(def cat-verbs
  (->> flattened-lexicon
       (filter #(= :verb (u/get-in % [:cat])))))

(def cat-noun
  (->> flattened-lexicon
       (filter #(= :noun (u/get-in % [:cat])))))

(def cat-det
  (->> flattened-lexicon
       (filter #(= :det (u/get-in % [:cat])))))

(def cat-adj
  (->> flattened-lexicon
       (filter #(= :adjective (u/get-in % [:cat])))))

(defn filter-lexemes [spec]
  (let [lexemes flattened-lexicon]
    (log/debug (str "start filtering; spec:" spec))
    (let [retval
          (filter
            (fn [lexeme]
               (if (not (= :fail (unify spec lexeme)))
                 (do
                    (log/debug (str "found a match: " (u/get-in lexeme [:canonical])))
                    true)
                 false))
            (cond
              (= (u/get-in spec [:cat]) :verb)
              cat-verbs
              (= (u/get-in spec [:cat]) :noun)
              cat-noun
              (= (u/get-in spec [:cat]) :det)
              cat-det
              (= (u/get-in spec [:cat]) :adjective)
              cat-adj
              true
              lexemes))]
      (log/debug (str "done filtering."))
      retval)))

(defn add-lexeme [tree & [spec]]
  (let [at (g/frontier tree)
        done-at (concat (remove-trailing-comps at) [:babylon.generate/done?])
        spec (or spec :top)
        tree (u/assoc-in! tree done-at true)
        spec (u/unify! spec (u/get-in tree at))]
    (if (= spec :fail)
      []
      (do
        (log/debug (str "add-lexeme: " (syntax-tree tree) " at: " at))
        (->> (filter-lexemes (u/strip-refs spec))
             shuffle
             (g/lazy-map (fn [candidate-lexeme]
                           (log/debug (str "adding lexeme: " (u/get-in candidate-lexeme [:canonical])))
                           (u/assoc-in! (u/copy tree) at (u/copy candidate-lexeme))))
             (filter #(do
                        (if (= :fail %)
                          (log/warn (str "lexeme with canonical form:" (u/get-in % [:canonical]) " failed to be added to: " (syntax-tree tree)
                                         "; failed path:" (u/fail-path (u/get-in tree at) %))))
                        (not (= :fail %))))
             (g/lazy-map #(update-syntax-tree % at))
             (g/lazy-map #(truncate-at % at))
             (g/lazy-map #(foldup % at)))))))

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
          (= true (u/get-in tree (concat frontier [:phrasal]))))
      (do
        (log/debug (str "adding rule: " (syntax-tree tree) (str "; frontier:" frontier)))
        (add-rule tree))

      (or (= false (u/get-in tree (concat frontier [:phrasal])))
          (u/get-in tree (concat frontier [:canonical])))
      (do
        (log/debug (str "adding lexeme: " (syntax-tree tree) (str "; frontier:" frontier)))
        (add-lexeme tree))
    
      true
      (do (log/warn (str "slowness at rule: " (u/get-in tree (concat (butlast frontier) [:rule])) " for child " (last frontier) " due to need to generate for both rules *and* lexemes.."))
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

(def quick-demo-spec
  {:rule "s" :comp {:phrasal false} :head {:phrasal false}})

(def medium-demo-spec
  (-> {:rule "s"
       :comp {:rule "np"
              :head {:rule "nbar"}}
       :head {:rule "vp"
              :comp {:rule "np"
                     :head {:phrasal false}}}}))

(def long-demo-spec
  (-> {:rule "s"
       :comp {:rule "np"
              :head {:rule "nbar"}}
       :head {:rule "vp-aux"
              :comp {:rule "vp"
                     :comp {:rule "np"
                            :head {:rule "nbar"}}}}}))
(def modal-spec
  (-> {:rule "s"
       :comp {:rule "np" :head {:rule "nbar"}}
       :head {:rule "vp-modal-1"}}))

(defn demo []
  (repeatedly
   #(println
     (-> long-demo-spec
         generate
         morph))))

(defn modal-demo []
  (repeatedly
   #(println
     (-> modal-spec
         generate
         time
         morph))))

(defn timed-demo []
  (repeatedly
   #(println
     (-> long-demo-spec
         generate
         time
         syntax-tree))))

(defn medium-timed-demo []
  (repeatedly
   #(println
     (-> medium-demo-spec
         generate
         time
         syntax-tree))))

(defn quick-demo []
  (repeatedly #(println (morph (time (generate quick-demo-spec))))))

(defn partial-generate-test []
  (first
    (->> [long-demo-spec]
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
