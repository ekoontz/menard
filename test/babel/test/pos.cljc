(ns babel.test.pos
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            [dag_unify.core :refer [unifyc get-in]]
            [babel.pos :refer [common-noun proper-noun]])
  
  (:refer-clojure :exclude [get-in]))

(deftest noun-test
  (is (not (nil? noun-test)))
  (is (= (get-in common-noun [:synsem :cat]) :noun)))

(deftest proper-noun-test
  (is (not (nil? noun-test)))
  (is (= (get-in proper-noun [:synsem :cat]) :noun))
  (is (= (get-in proper-noun [:synsem :propernoun]) true)))


