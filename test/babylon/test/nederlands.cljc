(ns babylon.test.nederlands
  (:require [babylon.english :as nl :refer [analyze expressions generate morph parse syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]))

(deftest first-test
  (is true))
