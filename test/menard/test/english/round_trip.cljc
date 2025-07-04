(ns menard.test.english.round-trip
  (:require [menard.english :as en :refer [analyze expressions generate morph parse syntax-tree get-grammar get-lexicon]]
            [dag_unify.core :as u :refer [unify]]
            [menard.lexiconfn :as l]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))


(load "../../../../src/menard/english/complete")


(deftest parse-english
  (is
   (= ["[s(:past-progressive) .he +[vp-aux +used(3) .[vp-aux +to(:v2) .[vp-aux +be(4) .[adj-p +able(3) .[vp-aux +to(:v1) .[vp +see .it]]]]]]]"]
      (->> "he used to be able to see it" en/parse (map en/syntax-tree) set vec))))

(defn get-spec-for-generation [parsed-expression]
  {:phrasal? (u/get-in parsed-expression [:phrasal?])
   :cat (u/get-in parsed-expression [:cat])
   :subcat (u/get-in parsed-expression [:subcat])
   :reflexive? (u/get-in parsed-expression [:reflexive?])
   :sem (u/get-in parsed-expression [:sem])})

(deftest roundtrip-english-past-progressive
  (let [input "he used to be able to sleep"]
    (is (= input
           (->> (en/parse input)
                (map #(u/get-in % [:sem]))
                (map (fn [sem] {:sem sem :cat :verb :subcat []}))
                (map en/generate)
                (map en/morph)
                first)))))

(deftest roundtrip-english-future
  (let [input "he will be able to sleep"]
    (is (= input
           (->> (en/parse input)
                (map #(u/get-in % [:sem]))
                (map (fn [sem] {:sem sem :cat :verb :subcat []}))
                (map en/generate)
                (map en/morph)
                first)))))

(deftest round-trip-direct-object-pronouns
  (is (= ["she sees him"]
         (->> "she sees him"
              parse
              (map get-spec-for-generation)
              (map generate)
              (map morph)
              vec)))
  (is (= ["he sees her"]
         (->> "he sees her"
              parse
              (map get-spec-for-generation)
              (map generate)
              (map morph)
              vec))))
