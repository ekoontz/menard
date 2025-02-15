(ns menard.test.español.preterito-perfecto
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def spec {:cat :verb
           :rule "s"
           :subcat []
           :root "comer"
           :sem {:pred :want
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

    


