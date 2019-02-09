(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze generate morph parse syntax-tree]]
   [dag_unify.core :as u :refer [unify]]))

(def specs
  [{:phrasal true
    :rule "np"
    :head {:rule "nbar4"
           :phrasal true
           :comp {:phrasal true
                  :comp {:phrasal true
                         :comp {:phrasal true}}}}}

   {:phrasal true
    :rule "np"
    :head {:rule "nbar3"
           :phrasal true
            :comp {:phrasal true
                   :rule "comp1"
                   :comp {:phrasal true
                          :rule "s-slash"}}}}])

(defn gen
  "how to generate a noun phrase with particular constraints."
  [i]
  (let [expression (generate (nth specs i))]
      {:st (syntax-tree expression)
       :morph (morph expression)
;;       :parses (map syntax-tree (parse (morph expression)))
       :phrase? (u/get-in expression [:head :comp :phrasal])}))
