(ns babel.latin
  (:refer-clojure :exclude [get-in])
  (:require [babel.latin.morphology :as morph]
            [babel.lexiconfn :refer [default listify map-function-on-map-vals]]
            [clojure.java.io :refer [reader resource]]
            [clojure.repl :refer [doc]]
            [dag_unify.core :refer [fail? get-in unifyc]]))

(defn edn2lexicon [resource]
  (-> (read-string (slurp resource)) ;; read .edn file into a Clojure map.
      listify
      (map-function-on-map-vals
       (fn [lexicon-string lexemes]
         (map (fn [lexeme]
                (merge lexeme
                       {:root lexicon-string}))
              lexemes)))
      (default
       {:phrasal false})))

(def lexicon (edn2lexicon "src/babel/latin/lexicon.edn"))

(defn generate [spec]
  (first (shuffle
          (filter #(not (fail? %))
                  (map (fn [val]
                         (unifyc spec val))
                       (flatten (vals lexicon)))))))

(defn parse [surface]
  [{:parses (morph/analyze surface lexicon)}])

(defn fo [structure]
  (morph/conjugate (get-in structure [:root]) structure))

(def model {:lexicon lexicon
            :fo fo
            :generate-fn generate})


