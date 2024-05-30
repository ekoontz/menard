(ns menard.español
  (:require [menard.model :refer [load-model]]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn analyze [^String surface-form]
  {:cat :noun
   :sem {:pred :I}})

(defn generate [spec]
  "yo quiero")

(defn morph [tree]
  "yo quiero")

(defn parse [^String expression]
  "[s .yo +quiero]")

(defn syntax-tree [tree]
  "[s .yo +quiero]")
