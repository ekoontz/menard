(ns menard.test.español.reflexives
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; https://es.wiktionary.org/wiki/quedarse#Conjugaci%C3%B3n

(deftest analyze-test
  (let [analysis (analyze "quedarse")]
    (is (seq analysis)))
  (let [analysis (analyze "queda")]
    (is (seq analysis))))

(deftest parse-test
  (let [one-parse (-> "Juan se queda" parse first)]
    (is (= (-> one-parse syntax-tree)
           "[s(:present-simple){+} .Juan +[vp-pronoun(:present-simple){+} .se(2) +queda]]"))))
