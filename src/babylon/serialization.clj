(ns babylon.serialization
  (:require
   [babylon.grammar :refer [default-morph-fn]]
   [dag_unify.core :as u]))

(defn morph-1 [syntax-tree morphology]
  (cond
    (nil? syntax-tree) "_"
    (u/get-in syntax-tree [:1])
    (str (morph-1 (u/get-in syntax-tree [:1]) morphology) " "
         (morph-1 (u/get-in syntax-tree [:2]) morphology))
    (u/get-in syntax-tree [:2])
    (str "_ "
         (morph-1 (u/get-in syntax-tree [:2]) morphology))
    true
    (default-morph-fn syntax-tree morphology)))

(defn syntax-tree-1 [syntax-tree morphology]
  (cond
    (nil? syntax-tree) "_"
    (u/get-in syntax-tree [:1])
    (str "["
         (:rule syntax-tree "?") " "
         (if (= true (u/get-in syntax-tree [:1 :head?]))
           "*" ".")
         (syntax-tree-1 (u/get-in syntax-tree [:1]) morphology) " "
         (if (= true (u/get-in syntax-tree [:2 :head?]))
           "*" ".")
         (syntax-tree-1 (u/get-in syntax-tree [:2]) morphology)
         "]")
    (u/get-in syntax-tree [:2])
    (str "["
         (:rule syntax-tree "?") " "
         (if (= true (u/get-in syntax-tree [:1 :head?]))
           "*" ".")
         "_ "
         (if (= true (u/get-in syntax-tree [:2 :head?]))
           "*" ".")
         (syntax-tree-1 (u/get-in syntax-tree [:2]) morphology) "]")
    true
    (default-morph-fn syntax-tree morphology)))
