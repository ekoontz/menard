(ns menard.test.translate.it-en
  (:require [dag_unify.core :as u]

            ;; TODO: load a subset of the default "complete" model
            ;; rather than the whole thing:
            [menard.english :as en]
            [menard.english.complete]

            [menard.italiano :as it]
            [menard.morphology :refer [morph-leaf]]
            [menard.translate.it-en :as translate]
            [clojure.test :refer [deftest is testing]]))

;; ---------------------------------------------------------------------------
;; italiano side
;; ---------------------------------------------------------------------------

(deftest italian-parse
  (testing "simple italiano words, phrases and clauses parse"
    (is (= 1 (count (it/parse "il cane"))))
    (is (seq (it/parse "parlo")))
    (is (seq (it/parse "il cane dorme")))))

(deftest italian-parse-tree
  (testing "a parsed noun phrase renders as expected"
    (is (= "[noun-phrase2 .il +cane]"
           (it/syntax-tree (first (it/parse "il cane")))))))

(deftest italian-morphology
  (testing "italiano verbs inflect via the ported morphology"
    (let [morphology (:morphology @it/model)]
      (is (= "parlo"
             (morph-leaf {:canonical "parlare"
                          :cat :verb
                          :infl :present
                          :agr {:number :sing :person :1st}}
                         morphology)))
      (is (= "parlano"
             (morph-leaf {:canonical "parlare"
                          :cat :verb
                          :infl :present
                          :agr {:number :plur :person :3rd}}
                         morphology))))))

;; ---------------------------------------------------------------------------
;; english side: the same checks as menard.test.translate.es-en
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; it -> en
;; ---------------------------------------------------------------------------

;; TODO: it->en translation is not asserted yet, because the spec conversion
;; (menard.translate.it-en/it-structure-to-en-spec, copied from the español
;; module) does not fit the italiano structures produced by the ported
;; grammar/lexicon yet:
;;
;;   * it-structure-to-en-spec unifies {:pronoun? ...} with {:sem {:mod ...}}
;;     and returns :fail for structures whose :sem has no :mod (e.g. a verb),
;;     so it-to-en falls through to the "_" placeholder.
;;   * where the spec *is* produced (e.g. "il cane"), english generation
;;     rejects it, because the italian noun's :sem (:pred nil, determiner
;;     semantics under :mod) does not line up with what menard's english
;;     lexicon/grammar expect.
;;
;; Until the italian semantics are aligned with the english ones, these are
;; kept here as documentation:
(comment
  (deftest it-to-en
    (is (or (= "I want" (translate/it-to-en "voglio"))
            (= "I like" (translate/it-to-en "voglio"))
            (= "I love" (translate/it-to-en "voglio")))))

  (deftest it-to-en-noun-phrase
    (is (string? (translate/it-to-en "il cane")))))
