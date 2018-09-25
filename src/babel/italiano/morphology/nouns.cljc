(ns babel.italiano.morphology.nouns
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unifyc)]))

;; TODO: add :g as with babel.italiano.morphology.verbs/patterns.
(defonce patterns
  [

   {:p [#"^c$" "ci"] ;; "c'" -> "ce"
    :u {:synsem {:cat :noun
                 :case :nom}}}

   {:p [#"^l$" "la"] ;; "l'" -> "la"
    :u {:synsem {:cat :noun
                 :pronoun true
                 :case :acc
                 :agr {:number :sing
                       :gender :fem}}}}

   {:p [#"^l$" "lo"] ;; "l'" -> "lo"
    :u {:synsem {:cat :noun
                 :pronoun true
                 :case :acc
                 :agr {:number :sing
                       :gender :masc}}}}

   {:p [#"(.*)e$" "$1a"] ;; "donna" -> "donne"
    :u {:synsem {:cat :noun
                 :agr {:gender :fem
                       :number :plur}}}}

   {:g [#"(.*)ca"   "$1che" ] ;; "mucca" -> "mucche"
    :p [#"(.*)che$" "$1ca"] ;; "mucche" -> "mucca"
    :u {:synsem {:cat :noun
                 :agr {:gender :fem
                       :number :plur}}}}

   {:g [#"(.*)à"   "$1à" ]
    :p [#"(.*)à$" "$1à"] ;; "città" -> "città"
    :u {:synsem {:cat :noun
                 :agr {:gender :fem
                       :number :plur}}}}

   {:g [#"(.*)io" "$1i"] ;; figlio -> figli
    :p [#"(.*)i$" "$1io"] ;; figli -> figlio
    :u {:synsem {:cat :noun
                 :agr {:number :plur}}}}

   {:g [#"(.*)e"   "$1i" ] ;; cane --> cani
    :p [#"(.*)i$" "$1e"] ;; "cani" -> "cane"; "madri" -> "madre"
    ;; (note that no gender is specified in :u because it could be either.
    :u {:synsem {:cat :noun
                 :agr {:number :plur}}}}

   {:g [#"(.*)o"   "$1i" ]
    :p [#"(.*)i$" "$1o"] ;; "compiti" -> "compito"
    :u {:synsem {:cat :noun
                 :agr {:number :plur}}}}
   ])

(def plural-to-singular-noun-fem-1
  {#"e$"
   {:replace-with "a"
    :unify-with {:synsem {:cat :noun
                          :agr {:gender :fem
                                :number :plur}}}}})

(def plural-to-singular-noun-masc-1
  {
   #"ii$" ;; e.g. "braccio" => "braccia"
   {:replace-with "a"
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

(def exceptions-rules
  [
   {:path [:italiano :plur]
    :merge-fn
    (fn [val]
      {:synsem {:cat :noun}
       :italiano {:agr {:number :plur}}})}])
