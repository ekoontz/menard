(ns menard.test.español.preterito-perfecto
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def spec
  {:cat :verb
   :subcat []
   :sem {:pred :eat
         :subj {:pred :i}
         :tense :past
         :aspect :perfect}})

;; https://es.wiktionary.org/wiki/comer#Conjugaci%C3%B3n

(deftest analyze-test
  (let [analysis (analyze "comido")]
    (is (seq analysis))
    (is (= 1 (count analysis)))
    (is (map? (-> analysis first)))
    (is (= :participio (-> analysis first (u/get-in [:infl]))))))

(deftest parse-test
  (let [yo-he-comido (-> "yo he comido" parse first)]
    (is (= (-> yo-he-comido syntax-tree)
           "[s-aux(:preterito-perfecto) .yo +[vp-aux-non-reflexive(:preterito-perfecto) +he .comido]]"))
    (is (= (-> yo-he-comido (u/get-in [:sem :aspect])) :perfect))
    (is (= (-> yo-he-comido (u/get-in [:sem :tense])) :past))))

(deftest generate-test
  (is (= (-> spec generate syntax-tree)
         "[s-aux(:preterito-perfecto) .yo +[vp-aux-non-reflexive(:preterito-perfecto) +he .comido]]")))

