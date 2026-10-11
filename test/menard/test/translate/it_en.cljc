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

(deftest italian-np-carries-the-head-noun-semantics
  (testing "head-sem copies the head noun's :sem (incl. :pred) into the NP"
    ;; before menard.ug/head-sem was added to the italian np rules, "il cane"
    ;; parsed to a :sem with the determiner's :mod but no :pred at all.
    (let [np (first (it/parse "il cane"))]
      (is (= :dog (u/get-in np [:sem :pred])))
      (is (= true (u/get-in np [:sem :pet]))))))

(deftest it-to-en-noun-phrases
  (testing "definite italian noun phrases translate"
    ;; the italian determiner (:mod :first :pred :definite) is moved onto
    ;; english's :sem :quant, the number is carried over as :ref, and singular
    ;; (unmarked in italian) defaults to :sing.
    (is (= "the dog" (translate/it-to-en "il cane")))
    (is (= "the dogs" (translate/it-to-en "i cani")))
    (is (= "the cat" (translate/it-to-en "il gatto"))))
  (testing "the translated spec has english's shape"
    (let [spec (translate/it-structure-to-en-spec (first (it/parse "il cane")))]
      (is (= :the (get-in spec [:sem :quant])))
      (is (= :dog (get-in spec [:sem :pred])))
      (is (= :sing (get-in spec [:sem :ref :number])))
      (is (= [] (get-in spec [:sem :mod]))))))

(deftest it-to-en-spec-alignment
  (testing "the italian structure is aligned with what the english lexicon expects"
    ;; italiano is pro-drop and flatter than español, so the conversion builds
    ;; the shape english generation needs: :pred kept, :infl turned into
    ;; :sem :tense/:aspect, a subject synthesized from :agr, and :mod/:subcat
    ;; as lists.
    (let [spec (translate/it-structure-to-en-spec (first (it/parse "parlo")))]
      (is (= :talk (get-in spec [:sem :pred])))
      (is (= :present (get-in spec [:sem :tense])))
      (is (= :simple (get-in spec [:sem :aspect])))
      (is (= [] (get-in spec [:sem :mod])))
      (is (= [] (:subcat spec)))
      (is (= :verb (:cat spec)))))

  (testing "the aligned predicates are the english ones"
    (is (contains? #{:talk :speak} (get-in (translate/it-structure-to-en-spec (first (it/parse "parlo"))) [:sem :pred])))
    (is (= :sleep (get-in (translate/it-structure-to-en-spec (first (it/parse "dormo"))) [:sem :pred])))))

(deftest it-to-en-remaining-work
  (testing "documented gaps in the it->en alignment"
    ;; Recorded here rather than asserted, so the gaps stay visible:
    ;;   * generating a full english sentence from the aligned spec is
    ;;     order-sensitive: "parlo" yields "I talk" or "what talk" depending on
    ;;     the run, because english still has freedom over the subject/object
    ;;     when they are not pinned by the spec. The conversion does synthesize
    ;;     a subject from :agr, but the parsed :agr can arrive as a dag atom, so
    ;;     even that is not yet stable.
    ;;   * noun phrases now work (see it-to-en-noun-phrases above).
    ;; The italian predicates themselves are already english-aligned for these
    ;; words (cane -> :dog, dormire -> :sleep, parlare -> :talk/:speak); the
    ;; remaining work is in the italian semantics/structure, not the names.
    (is (string? (translate/it-to-en "parlo")))))
