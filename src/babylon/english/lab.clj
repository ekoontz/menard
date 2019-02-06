(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [generate morph syntax-tree]]
   [dag_unify.core :as u :refer [unify]]))

(defn a-specific-noun-phrase
  "how to generate a noun phrase with particular constraints."
  []
  (let [my-bespoke-noun-phrase
        (generate {:phrasal true
                   :rule "np"
                   :head {:rule "nbar4" :phrasal true
                          :comp {:phrasal true}}})]
    (let [some-structural-info
            {:st (syntax-tree my-bespoke-noun-phrase)
             :morph (morph my-bespoke-noun-phrase)
             :phrase? (u/get-in my-bespoke-noun-phrase [:head :comp :phrasal])}]
      (:morph some-structural-info))))
