(ns menard.serialization
  (:require
   [menard.grammar :refer [default-morph-fn]]
   [dag_unify.core :as u]))

(defn morph [tree morphology]
  (cond
    (nil? tree) "_"
    (u/get-in tree [:1])
    (str (morph (u/get-in tree [:1]) morphology) " "
         (morph (u/get-in tree [:2]) morphology))
    (u/get-in tree [:2])
    (str "_ "
         (morph (u/get-in tree [:2]) morphology))
    :else
    (default-morph-fn tree morphology)))

(defn syntax-tree [tree morphology]
  (cond
    (nil? tree) "_"
    (u/get-in tree [:syntax-tree])
    (syntax-tree (u/get-in tree [:syntax-tree]) morphology)
    (u/get-in tree [:1])
    (str "["
         (:rule tree "?")
         (when (:variant tree) (str "(" (:variant tree) ")" ""))
         " "
         (if (or (= true (u/get-in tree [:1 :head?]))
                 (= (u/get-in tree [:1]) (u/get-in tree [:head])))
           "+" ".")
         (syntax-tree (u/get-in tree [:1]) morphology) " "
         (if (or (= true (u/get-in tree [:2 :head?]))
                 (= (u/get-in tree [:2]) (u/get-in tree [:head])))
           "+" ".")
         (syntax-tree (u/get-in tree [:2]) morphology)
         "]")
    (u/get-in tree [:2])
    (str "["
         (:rule tree "?") " "
         (if (= true (u/get-in tree [:1 :head?]))
           "+" ".")
         "_ "
         (if (= true (u/get-in tree [:2 :head?]))
           "+" ".")
         (syntax-tree (u/get-in tree [:2]) morphology) "]")
    :else
    (default-morph-fn tree morphology)))
