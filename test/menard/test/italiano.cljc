(ns menard.test.italiano
  (:require [menard.italiano :as it
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; These are for convenience so you don't
;; have to reload these language-model files every time you make a change:
;; you can just reload this file (menard.test.italiano.clj)
;; file and it will reload the changed language-model files.
(load "../../../src/menard/subcat")
(load "../../../src/menard/italiano")

(def spec3 {:cat :verb
            :rule "s"
            :subcat []
            :root "abbandonare"
            :sem {:pred :abandon
                  :tense :present
                  :subj {:pred :i}}})

(deftest subject-agreement
  (count
   (take 10
         (repeatedly #(is (= "io abbandono"
                             (-> spec3
                                 generate
                                 ((fn [x]
                                    (log/info (str "syntax-tree: " (syntax-tree x)))
                                    x))
                                 morph
                                 ((fn [x]
                                    (log/info (str "morph: " x))
                                    x)))))))))
