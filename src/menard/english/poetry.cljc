(ns menard.english.poetry
  (:require
   [menard.english :as en :refer [analyze expressions generate grammar
                                   index-fn lexicon morph parse syntax-tree]]
   [menard.generate :as g :refer [add lazy-map]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])))

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
