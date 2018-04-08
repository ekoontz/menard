(ns babel.italiano.morphology.determiners
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unifyc)]))

;; TODO: add :g as with babel.italiano.morphology.verbs/patterns.
(def patterns
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

   ;; <generation rules>: turn lexicon-derived strings into surface strings.
   ;; il -> lo
   {:g [#"\bil ((gn)|(io)|(jo)|(pn)|(s[^aeiou])|([xyz]))"  "lo $1"]}

   ;; i -> gli
   {:g [ #"\bi ((gn)|(io)|(jo)|(pn)|(s[^aeiou])|([xyz]))"  "gli $1"]}

   ;; la -> l'
   {:g [ #"\bla ([aeiou])"                                 "l'$1"]}
   
   ;; un -> uno
   {:g [ #"\bun ((gn)|(io)|(jo)|(pn)|(s[^aeiou])|([xyz]))" "uno $1"]}

   ;; una -> un'
   {:g [ #"\bun ([aeiou])"                                 "un'$1"]}
   
   ;; </generation rules>
   
   ])

(defn apply-determiner-rules [left right]
  (let [input (string/join " " [left right])
        results
        (remove nil?
                (map (fn [[from to]]
                       (if (re-find from input)
                         (clojure.string/replace 
                          input
                    from to)))
                     (map #(get % :g)
                          (filter (fn [pattern-map]
                                    (some #(= :g %) (keys pattern-map)))
                                  patterns))))]
    (if (not (empty? results))
      [true (first results)]
      [false input])))
