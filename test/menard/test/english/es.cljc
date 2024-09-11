(ns menard.test.english.es
  (:require [menard.english.es :as en
             :refer [analyze generate morph parse
                     syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [menard.lexiconfn :as l]))

(def spec {:sem {:iobj :none,
                 :subj
                 {:gender :masc,
                  :existential? false,
                  :mod [],
                  :ref {:human? true},
                  :pred :he},
                 :mod [],
                 :pred :get-angry,
                 :aspect :simple,
                 :tense :present},
           :agr {:gender :masc, :number :sing, :person :3rd},
           :subcat [],
           :cat :verb})

(deftest reflexive-generate
  (let [generated (-> spec generate)]
    (is (not (empty? generated)))
    (is (= (-> generated morph) "he gets angry"))))

