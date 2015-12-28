(ns babel.test.pos
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            [dag_unify.core :refer [unifyc get-in]]
            [babel.pos :refer [common-noun proper-noun verb-subjective]])
  
  (:refer-clojure :exclude [get-in]))

(deftest noun-test
  (is (not (nil? common-noun)))
  (is (= (get-in common-noun [:synsem :cat]) :noun)))

(deftest proper-noun-test
  (is (not (nil? proper-noun)))
  (is (= (get-in proper-noun [:synsem :cat]) :noun))
  (is (= (get-in proper-noun [:synsem :propernoun]) true)))

(deftest verb-subjective-test
  (is (not (nil? verb-subjective)))
  (is (empty? (get-in verb-subjective [:synsem :subcat :2])))
  (is (= (get-in verb-subjective [:synsem :sem :subj])
         (get-in verb-subjective [:synsem :subcat :1 :sem]))))

         

