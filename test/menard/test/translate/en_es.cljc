(ns menard.test.translate.en-es
  (:require [dag_unify.core :as u]
            [menard.english :as en]
            [menard.español :as es]
            [menard.lexiconfn :as l]
            [menard.translate.es-en :as translate]
            [clojure.test :refer [deftest is]]))

(deftest transfer-1
  (is (or  (= "I want" (translate/es-to-en "yo quiero"))
           (= "I like" (translate/es-to-en "yo quiero"))
           (= "I love" (translate/es-to-en "yo quiero")))))
  
(deftest parse-english
  (is
   (= ["[s(:past-progressive) .he +[vp +used(2) .[vp +to(:v2) .[vp +be(4) .[adj-p +able(3) .[vp +to(:v1) .[vp +see .it]]]]]]]"]
      (->> "he used to be able to see it" en/parse (map en/syntax-tree)))))

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

