(ns babylon.english.serialization
  (:require
   [babylon.english :as en]
   [dag_unify.core :as u]))

(declare morph-1)
(declare syntax-tree-1)
(defn morph
  ([tree]
   (morph-1 (u/get-in tree [:syntax-tree])))
  ([tree & {:keys [sentence-punctuation?]}]
   (if sentence-punctuation?
     (-> tree
         morph
         en/an
         (en/sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

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

(defn syntax-tree [tree]
  (cond (or (nil? tree) (= :fail tree))
        tree
        (= ::unspec (u/get-in tree [:syntax-tree] ::unspec))
        (babylon.english/syntax-tree tree)
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
