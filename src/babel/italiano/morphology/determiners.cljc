(ns babel.italiano.morphology.determiners
  (:refer-clojure :exclude [get-in merge resolve])
  (:require
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in merge ref? strip-refs unifyc)]))

(def replace-patterns
  [

   {:p [#"^dell$" "del"]
    :u {:cat :det
        :agr {:number :sing
              :gender :masc}}}

   {:p [#"^dell$" "della"]
    :u {:cat :det
        :agr {:number :sing
              :gender :fem}}}

   {:p [#"^gli$" "i"] ;; "gli" -> "i"
    :u {:synsem {:cat :det
                 :agr {:number :plur
                       :gender :masc}}}}

   {:p [#"^l$" "la"] ;; "l'" -> "la"
    :u {:synsem {:cat :det
                 :agr {:number :sing
                       :gender :fem}}}}

   {:p [#"^l$" "il"] ;; "l'" -> "il"
    :u {:synsem {:cat :det
                 :agr {:number :sing
                       :gender :masc}}}}
   ])
