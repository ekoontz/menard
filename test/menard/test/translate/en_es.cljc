(ns menard.test.translate.en-es
  (:require [dag_unify.core :as u :refer [unify]]
            [menard.english :as en]
            [menard.español :as es]
            [menard.lexiconfn :as l]
            [menard.translate.es-en :as translate]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

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

(deftest translate-test
  (let [es-spec {:reflexive? false
                 :root  "llenar"
                 :cat :verb
                 :sem {:subj {:pred :they
                              :gender :fem}}
                 :subcat []
                 :rule "s-aux"
                 :phrasal? true}
        es-generated (-> es-spec es/generate)
        en-spec (-> es-generated translate/es-structure-to-en-structure)
        en-generated (-> en-spec en/generate)]
    (log/debug (str "translate-test-1: " (es/syntax-tree es-generated)))
    (is (= (es/syntax-tree es-generated)
           "[s-aux(:preterito-perfecto) .ellas +[vp-aux-non-reflexive(:preterito-perfecto) +han(:explicit-subj-non-reflexive) .llenado]]"))
    (is (= (binding [menard.morphology/show-notes? false]
             (en/syntax-tree en-generated))
           "[s(:perfect) .they +[vp +have(2) .filled]]")))
  (let [es-spec {:rule "s"
                 :sem {:subj
                       {:existential? false
                        :mod []
                        :pred :Juana}
                       :mod []
                       :pred :wake-up
                       :aspect :simple
                       :tense :present}
                 :subcat [],
                 :phrasal? true,
                 :cat :verb,
                 :pronoun? nil}
        es-generated (-> es-spec es/generate)
        debug (log/debug (str "es-generated is: " (es/syntax-tree es-generated)))
        en-spec (translate/es-structure-to-en-structure es-generated)
        en-generated (-> en-spec en/generate)]
    (is (= (es/syntax-tree es-generated)
           "[s(:present-simple){+} .Juana +[vp-pronoun(:present-simple){+} .se(3) +despierta(:explicit-subj)]]"))
    (is (= (en/syntax-tree en-generated)
           "[s(:present-simple) .Juana +[vp +wakes .up]]"))))

(defn timings []
  (let [es-spec {:head {:reflexive? false
                        :canonical "llenar"
                        :rule "vp-aux-non-reflexive"
                        :cat :verb}
                 :subcat []
                 :rule "s-aux"
                 :phrasal? true
                 :cat :verb}
        es-generated (-> es-spec es/generate)
        en-spec (translate/es-structure-to-en-structure es-generated)
        en-generated (-> en-spec en/generate)]
    {:es (-> es-generated es/morph)
     :en (-> en-generated en/morph)}))

(defn do-timing []
  (take 10 (repeatedly #(time (println (timings))))))
