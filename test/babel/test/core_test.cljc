(ns babel.test.core_test
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            [dag_unify.core :refer [get-in unify unifyc]]
            [babel.pos :refer [noun]])
  (:refer-clojure :exclude [get-in]))

(deftest eugene-test
  (is (= 1 1)))

(deftest eugene-test2
  (is (= 2 2)))

(deftest eugene-test3
  (is (= 3 3)))

(deftest eugene-test4
  (is (= 4 4)))

