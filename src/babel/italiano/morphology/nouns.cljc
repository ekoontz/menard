(ns babel.italiano.morphology.nouns
  (:refer-clojure :exclude [get-in merge resolve])
  (:require
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in merge ref? strip-refs unifyc)]))

(def replace-patterns
  [

   {:p [#"(.*)e$" "$1o"] ;; nere -> nero
    :u {:synsem {:cat :adjective
                 :agr {:gender :fem
                       :number :plur}}}}

   {:p [#"(.*)e$" "$1e"] ;; difficile (fem plural) -> difficile (lexical form, which happens to be spelled the same)
    :u {:synsem {:cat :adjective
                 :agr {:gender :fem
                       :number :plur}}}}

   {:p [#"(.*)a$" "$1o"] ;; nera -> nero
    :u {:synsem {:cat :adjective
                 :agr {:gender :fem
                       :number :sing}}}}

   {:p [#"(.*)i$" "$1e"] ;; difficili -> difficile
    ;; note that we do not specify the [:agr :gender], because
    ;; the surface form could be either masculine or feminine.
    :u {:synsem {:cat :adjective
                 :agr {:number :plur}}}}

   {:p [#"(.*)i$" "$1o"] ;; neri -> nero
    :u {:synsem {:cat :adjective
                 :agr {:gender :masc
                       :number :plur}}}}

   {:p [#"(.*)i$" "$1io"] ;; "bracci" -> "braccio"
    :u {:synsem {:cat :noun
                 :agr {:number :plur}}}}

   {:p [#"([^i])i$" "$1e"] ;; "cani" -> "cane"
    :u {:synsem {:cat :noun
                 :agr {:gender :masc
                       :number :plur}}}}
   
   ])

(def plural-to-singular-noun-fem-1
  {#"e$"
   {:replace-with "a"
    :unify-with {:synsem {:cat :noun
                          :agr {:gender :fem
                                :number :plur}}}}})

(def plural-to-singular-noun-masc-1
  {
   #"ii$" ;; e.g. "braccio" => "bracci"
   {:replace-with "io"
    :unify-with {:synsem {:cat :noun
                          :agr {:number :plur}}}}
   #"([^i])i$"
   {:replace-with "$1o"
    :unify-with {:synsem {:cat :noun
                          :agr {:number :plur}}}}})

(def plural-to-singular-noun-masc-2 ;; e.g. "cani" -> "cane"
  {#"i$"
   {:replace-with "e"
    :unify-with {:synsem {:cat :noun
                          :agr {:number :plur}}}}})
