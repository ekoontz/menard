(ns menard.test.español
  (:require [menard.español :as es
             :refer [generate morph]]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def spec {:cat :verb
           :rule "s"
           :subcat []
           :root "querer"
           :sem {:pred :want
                 :subj {:pred :i}}})

(def spec2 {:cat :verb
            :rule "s"
            :subcat []
            :sem {:pred :want}})

(deftest subject-agreement
  (is (= "yo quiero"
         (-> {:cat :verb
              :rule "s"
              :subcat []
              :root "querer"
              :sem {:pred :want
                    :subj {:pred :i}}}
             generate
             morph)))
  (is (= "ellas quieren"
         (-> {:cat :verb
              :rule "s"
              :subcat []
              :root "querer"
              :agr {:number :plur
                    :gender :fem}
              :sem {:pred :want
                    :subj {:pred :they}}}
             generate
             morph))))

(deftest find-root-verb
  (is (= "querer"
         (-> spec generate (u/get-in [:root]))))
  (is (= "querer"
         (-> spec2 generate (u/get-in [:root])))))

