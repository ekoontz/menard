(ns babel.francais.morphology.nouns
  (:refer-clojure :exclude [get-in merge resolve])
  (:require
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in merge ref? strip-refs unifyc)]))

(def replace-patterns
  [
   ;; pronouns: e.g.: "t'" => "te". The apostrophe (')
   ;; is already removed by the tokenizer.
   {:i [#"^([jlmst])" "$1e"]}
   {:i [#"^(l)" "$1a"]}])

(defn agreement [lexical-entry]
  (let [agr (atom :top)
        cat (atom :top)]
    (unifyc lexical-entry
            {:français {:agr agr
                        :cat cat}
             :synsem {:agr agr
                      :cat cat}})))

(def plural-to-singular-noun-fem-1
  {#"e$"
   {:replace-with "a"
    :unify-with {:synsem {:cat :noun
                          :agr {:gender :fem
                                :number :plur}}}}})

(def plural-to-singular-noun-masc-1
  {#"i$"
   {:replace-with "o"
    :unify-with {:synsem {:cat :noun
                          :agr {:number :plur}}}}})

(def plural-to-singular-noun-masc-2 ;; e.g. "cani" -> "cane"
  {#"i$"
   {:replace-with "e"
    :unify-with {:synsem {:cat :noun
                          :agr {:number :plur}}}}})


(def plural-to-singular-adj-masc
  {#"i$"
   {:replace-with "o"
    :unify-with {:synsem {:cat :adjective
                          :agr {:gender :masc
                                :number :plur}}}}})

(def plural-to-singular-adj-fem-sing
  {#"a$"
   {:replace-with "o"
    :unify-with {:synsem {:cat :adjective
                          :agr {:gender :fem
                                :number :sing}}}}})

(def plural-to-singular-adj-fem-plur
  {#"e$"
   {:replace-with "o"
    :unify-with {:synsem {:cat :adjective
                          :agr {:gender :fem
                                :number :plur}}}}})

(def lexical-noun-to-singular
  {:identity
   {:unify-with {:synsem {:cat :noun
                          :agr {:number :sing}}}}})
