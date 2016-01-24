(ns babel.italiano.morphology.nouns
  (:refer-clojure :exclude [get-in merge resolve])
  (:require
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in merge ref? strip-refs unifyc)]))

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
