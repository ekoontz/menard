(ns menard.test.english.reflexives
  (:require [menard.english :as en :refer [analyze expressions generate morph parse syntax-tree get-grammar get-lexicon]]
            [dag_unify.core :as u :refer [unify]]
            [menard.lexiconfn :as l]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def developer-mode? false)
(when developer-mode? 
  (load "../../../../src/menard/english/complete"))

(deftest reflexive-parsing
  (let [parses (parse "he hurt himself")]
    (is (seq parses))))

;;(deftest reflexive-generation
;;  (let [spec {:obj {:reflexive? :none},
;;              :sem {:obj :none, :iobj :none,
;;                    :subj {:existential? false, :mod [], :ref {:human? true, :number [[1] :plur]}, :pred :we}, :mod [], :pred :hurt-oneself, :aspect :simple, :tense :present}, :agr {:number [1], :person :1st}, :subcat [], :phrasal? true, :cat :verb, :pronoun? nil}
;;    (is (seq parses))))
