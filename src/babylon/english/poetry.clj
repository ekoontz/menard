(ns babylon.english.poetry
  (:require
   [babylon.english :as en :refer [analyze expressions generate grammar
                                   index-fn lexicon morph parse syntax-tree]]
   [babylon.generate :as g :refer [add lazy-map]]
   [dag_unify.core :as u]
   [clojure.tools.logging :as log]))

(declare poetry-line)

(defn poetry []
  (loop []
    (println (if true
               (morph (or (poetry-line) "(failed)") :sentence-punctuation? true)
               (syntax-tree (poetry-line))))
    (recur)))

(defn poetry-line []
  (try
    (->
     expressions
     shuffle
     first
     generate)
    (catch Exception e
      (log/warn (str "fail: " e)))))
