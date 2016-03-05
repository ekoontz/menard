(ns babel.italiano.morphology.adjectives
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
   ])

(def plural-to-singular-adj-masc
  {#"i$"
   {:replace-with "o"
    :unify-with {:italiano {:masc {:plur :not-string}}
                 :synsem {:cat :adjective
                          :agr {:gender :masc
                                :number :plur}}}}
   #"i$"
   {:replace-with "e"
    :unify-with {:italiano {:masc {:plur :not-string}}
                 :synsem {:cat :adjective
                          :agr {:gender :masc
                                :number :plur}}}}})

(def plural-to-singular-adj-fem-sing
  {#"a$"
   {:replace-with "o"
    :unify-with {:italiano {:fem {:sing :not-string}}
                 :synsem {:cat :adjective
                          :agr {:gender :fem
                                :number :sing}}}}
   #"a$"
   {:replace-with "e"
    :unify-with {:italiano {:fem {:sing :not-string}}
                 :synsem {:cat :adjective
                          :agr {:gender :fem
                                :number :sing}}}}})

(def plural-to-singular-adj-fem-plur
  {#"e$"
   {:replace-with "o"
    :unify-with {:italiano {:fem {:plur :not-string}}
                 :synsem {:cat :adjective
                          :agr {:gender :fem
                                :number :plur}}}}
   #"i$"
   {:replace-with "e"
    :unify-with {:italiano {:fem {:plur :not-string}}
                 :synsem {:cat :adjective
                          :agr {:gender :fem
                                :number :plur}}}}})
