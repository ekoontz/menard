(ns babel.italiano.vocabcoach
  (:refer-clojure :exclude [get-in merge])
  (:require
   #?(:clj [clojure.tools.logging :as log])
   [babel.cache :refer [create-index]]
   #?(:cljs [babel.logjs :as log]) 
   [babel.italiano.grammar :refer [medium]]
   [babel.italiano.lexicon :refer [lexicon]]
   [babel.italiano.morphology :as morph :refer [fo]]
   [babel.italiano.workbook :refer [analyze generate lightning-bolt parse]]
   [babel.lexiconfn :refer [filter-keys filter-vals]]
   [babel.ug :refer [head-principle]]
   [dag_unify.core :refer [fail? get-in merge strip-refs unifyc unify]]))

(def vc-lex
  (-> lexicon
      (filter-keys
       #(or
         (= % "a")
         (= % "a prossima")
         (= % "casa")
         (= % "essere")
         (= % "io")
         (= % "sono")))))

(def vc-model
  (merge medium
         {:lexicon vc-lex}
         {:index (create-index (:grammar medium)
                               (flatten (vals vc-lex))
                               head-principle)}))

  

