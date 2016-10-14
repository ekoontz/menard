(ns ^{:doc "babel.writer testing code"}
    babel.test.writer
  (:refer-clojure :exclude [test])
  (:require [babel.writer :as writer]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]))

(deftest simple
  (is (= true true)))
