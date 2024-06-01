(ns menard.español 
  (:require [menard.español.compile :refer [compile-lexicon]]
            [menard.model :refer [create load-model]]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def model
  (ref (create "español/models/basic"
               "basic"
               compile-lexicon
               true)))

(defn generate [spec]
  {:a 42})

(defn morph [tree]
  "yo quiero")
