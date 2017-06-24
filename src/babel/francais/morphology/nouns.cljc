(ns babel.francais.morphology.nouns
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unifyc)]))

(def regular-patterns
  [
   ;; pronouns: e.g.: "t'" => "te". The apostrophe (')
   ;; is already removed by the tokenizer.
   {:p [#"^([jlmst])" "$1e"]}
   {:p [#"^(l)" "$1a"]}])

(defn agreement [lexical-entry]
  (let [agr (atom :top)
        cat (atom :top)]
    (unifyc lexical-entry
            {:français {:agr agr
                        :cat cat}
             :synsem {:agr agr
                      :cat cat}})))
