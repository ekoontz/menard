(ns menard.test.español.round-trip
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; https://es.wiktionary.org/wiki/quedarse#Conjugaci%C3%B3n

;; to reload after fixing something, do:
;; uncomment these:
(load "../../../../src/menard/subcat")
(load "../../../../src/menard/español")

(deftest reflexive-roundtrip
  (let [have-fun-spec {:comp {:agr {:gender :fem
                                 :number :plur
                                 :person :3rd}}
                       :sem {:tense :present
                             :aspect :simple
                             :pred :have-fun}
                       :subcat []}]
    (->> (-> have-fun-spec es/generate
             es/morph es/parse)
         (map #(is "s" (u/get-in % [:rule])))
         vec)))

(deftest ustedes-se-duermen
  (is (= "[s(:present-simple){+} .ustedes +[vp-pronoun(:present-simple){+} .se(6) +duermen(:explicit-subj)]]"
         (-> {:rule "s"
              :comp {:root "ustedes"
                     :agr {:number :plur,
                           :person :2nd
                           :formal? true}},
              :sem {:tense :present,
                    :aspect :simple},
              :head {:rule "vp-pronoun"
                     :head {:canonical "dormirse"}}}
             es/generate es/syntax-tree))))

(deftest ellos-cierran
  (is (= "[s(:present-simple) .ellos +cierran]"
         (-> {:rule "s"
              :comp {:root "ellos"
                     :agr {:number :plur
                           :person :3rd}}
              :sem {:tense :present
                    :aspect :simple}
              :head {:canonical "cerrar"}}
             es/generate es/syntax-tree))))


