(ns menard.test.translate.en-es
  (:require [dag_unify.core :as u :refer [unify]]
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
   (= ["[s(:past-progressive) .he +[vp +used(3) .[vp +to(:v2) .[vp +be(4) .[adj-p +able(3) .[vp +to(:v1) .[vp +see .it]]]]]]]"]
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

(def have-fun-spec {:comp {:agr {:gender :fem
                                 :number :plur
                                 :person :3rd}}
                    :sem {:tense :present
                          :aspect :simple
                          :pred :have-fun}
                    :subcat []})

(def sentence-specs
  [{:rule "s" :head {:rule "vp"
                     :head {:phrasal? false :canonical "divertirse"}
                     :comp {:phrasal? false
                            :sem {:pred :top}}}}
                     
   {:rule "s" :head {:phrasal? false :canonical "divertirse"}}])

(deftest reflexive-roundtrip
  (let [input-sem (u/get-in have-fun-spec [:sem])]
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

(def have-fun-spec {:comp {:agr {:gender :fem
                                 :number :plur
                                 :person :3rd}}
                    :sem {:tense :present
                          :aspect :simple
                          :pred :have-fun}
                    :subcat []})

(def preguntar-present-spec {:comp {:agr {:gender :fem
                                          :number :plur
                                          :person :3rd}}
                             :sem {:tense :present
                                   :aspect :simple
                                   :pred :ask-for}
                             :root "preguntar"
                             :subcat []})

(def preguntar-preterito-spec {:comp {:agr {:gender :fem
                                            :number :plur
                                            :person :3rd}}
                               :sem {:tense :past
                                     :aspect :perfect
                                     :pred :ask-for}
                               :root "preguntar"
                               :subcat []})
  
(deftest noun-phrases
  (is (= "the black cats"
         (translate/es-to-en "los gatos negros"))))
