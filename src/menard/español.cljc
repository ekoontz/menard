(ns menard.español
  (:require [menard.model :refer [load-model]]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn generate [spec]
  "yo quiero")

(defn morph [tree]
  "yo quiero")
