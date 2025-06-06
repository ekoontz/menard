(ns menard.test.español
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; These are for convenience so you don't
;; have to reload these language-model files every time you make a change:
;; you can just reload this file (menard.test.español.clj)
;; file and it will reload the changed language-model files.
(load "../../../src/menard/subcat")
(load "../../../src/menard/español")

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

(def spec3 {:cat :verb
            :rule "s"
            :subcat []
            :root "querer"
            :sem {:pred :want
                  :tense :present
                  :subj {:pred :i}}})

(def spec4 {:cat :verb
            :rule "s"
            :subcat []
            :root "querer"
            :agr {:number :plur
                  :gender :fem}
            :sem {:pred :want
                  :tense :present
                  :subj {:pred :they}}})

(deftest subject-agreement
  (count
   (take 10
         (repeatedly #(is (= "yo quiero"
                             (-> spec3
                                 generate
                                 ((fn [x]
                                    (log/info (str "syntax-tree: " (syntax-tree x)))
                                    x))
                                 morph
                                 ((fn [x]
                                    (log/info (str "morph: " x))
                                    x))))))))
  (count
   (take 10
         (repeatedly #(is (= "ellas quieren"
                             (-> spec4
                                 generate
                                 morph)))))))

(deftest find-root-verb
  (is (= "querer"
         (-> spec generate (u/get-in [:root]))))
  (is (= "querer"
         (-> spec2 generate (u/get-in [:root])))))

(deftest analyze-1
  (let [analysis (analyze "quiero")]
    (is (seq analysis))
    (is (= "querer" (->> analysis (map #(u/get-in % [:canonical])) set first)))
    (is (= :present (->> analysis (map #(u/get-in % [:infl])) set first)))
    (is (= {:person :1st :number :sing} (->> analysis (map #(u/get-in % [:agr])) set first)))))

(deftest parse-1
  (let [parses (parse "yo quiero")]
    (is (= "[s(:present-simple) .yo +quiero]" (-> parses first syntax-tree)))))

(deftest analyze-present-tense
  ;; irregular
  (let [quiero (->> "quiero" analyze first)]
    (is (= (u/get-in quiero [:agr]) {:person :1st
                                     :number :sing}))
    (is (or (= (u/get-in quiero [:sem :pred]) :want)
            (= (u/get-in quiero [:sem :pred]) :like)
            (= (u/get-in quiero [:sem :pred]) :love)))
    (is (= (u/get-in quiero [:infl]) :present)))
  ;; regular
  (let [hablan (->> "hablan" analyze first)]
    (is (= (u/get-in hablan [:agr :number]) :plur))
    (is (= (u/get-in hablan [:infl]) :present))))

(deftest analyze-future-tense
  ;; regular
  (let [hablaré (->> "hablaré" analyze first)]
    (is (= (u/get-in hablaré [:agr :number]) :sing))
    (is (= (u/get-in hablaré [:agr :person]) :1st))    
    (is (= (u/get-in hablaré [:infl]) :future))
    (is (or (= (u/get-in hablaré [:sem :pred]) :speak)
            (= (u/get-in hablaré [:sem :pred]) :talk))))    
  (let [hablaréis (->> "hablaréis" analyze first)]
    (is (= (u/get-in hablaréis [:agr :number]) :plur))
    (is (= (u/get-in hablaréis [:agr :person]) :2nd))    
    (is (= (u/get-in hablaréis [:infl]) :future))
    (is (or (= (u/get-in hablaréis [:sem :pred]) :speak)
            (= (u/get-in hablaréis [:sem :pred]) :talk)))))

