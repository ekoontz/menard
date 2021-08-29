(ns menard.serialization
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [menard.morphology :as m]
   [dag_unify.core :as u]))

(defn morph [tree morphology]
  (cond
    (nil? tree) "_"
    (string? tree) tree    
    (string? (u/get-in tree [:syntax-tree]))
    (u/get-in tree [:syntax-tree])
    (u/get-in tree [:1])
    (str (morph (u/get-in tree [:1]) morphology) " "
         (morph (u/get-in tree [:2]) morphology))
    (u/get-in tree [:2])
    (str "_ "
         (morph (u/get-in tree [:2]) morphology))
    :else
    (m/morph-leaf tree morphology)))

(defn syntax-tree [tree morphology]
  (cond
    (nil? tree) "_"
    (string? tree) tree    
    (string? (u/get-in tree [:syntax-tree])) (u/get-in tree [:syntax-tree])
    (u/get-in tree [:1])
    (str "["
         (:rule tree "?")
         (when (:variant tree) (str "(" (:variant tree) ")" ""))
         (when (let [defined? (u/get-in tree [:reflexive])]
                 (and (not (= defined? ::none))
                      (= :verb (u/get-in tree [:cat]))
                      (not (= defined? :top))))
           (let [value (u/get-in tree [:reflexive])]
             (str "{" (cond (= value true)
                            "+"
                            (= value false)
                            "-"
                            :else value) "}")))
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
    (do
      (log/info (str "GOT INTO THE MORPH_LEAF"))
      (m/morph-leaf tree morphology))))

