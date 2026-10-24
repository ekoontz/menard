(ns menard.test.translate.it-en
  (:require [clojure.string :as str]
            [dag_unify.core :as u]

            ;; TODO: load a subset of the default "complete" model
            ;; rather than the whole thing:
            [menard.english :as en]
            [menard.english.complete]

            [menard.italiano :as it]
            [menard.morphology :as m :refer [morph-leaf]]
            [menard.translate.it-en :as translate]
            [clojure.test :refer [deftest is testing]]))

(defn- has-emoji-of?
  "menard appends a formality emoji to a 2nd-person pronoun; true if _surface_
   carries one of the emojis in _emojis_."
  [surface emojis]
  (some #(str/includes? surface %) emojis))

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
    ;; noun-phrase1 is the determiner + bare noun rule; noun-phrase2 takes the
    ;; :phrasal head, so it is the rule for "il cane nero".
    (is (= "[noun-phrase1 .il +cane]"
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

(deftest it-to-en-verbs
  (testing "a pro-drop subject is realized from :agr"
    ;; third-person-plural subjects come out with menard's own person-emoji
    ;; markers, so only the forms without them are asserted here.
    (is (= "I talk" (translate/it-to-en "parlo")))
    (is (= "I sleep" (translate/it-to-en "dormo")))
    (is (= "he sleeps" (translate/it-to-en "dorme"))))
  (testing "an explicit pronoun subject is realized the same way"
    ;; english spells its pronouns itself, so italiano's own pronoun
    ;; predicates (:I, :we, ...) are not carried across.
    (is (= "I talk" (translate/it-to-en "io parlo")))
    (is (= "I sleep" (translate/it-to-en "io dormo"))))
  (testing "formal lei keeps its second-person meaning with third-person agreement"
    (let [specs (map #(translate/it-structure-to-en-spec %)
                     (it/parse "lei parla"))]
      (is (some #(= {:pred :you
                     :ref {:human? true, :context :formal, :number :sing}
                     :existential? false
                     :mod []}
                    (get-in % [:sem :subj]))
                specs))))
  (testing "the verb reaches the sentence (menard.ug/head-sem on the s rules)"
    (is (= :sleep (u/get-in (first (it/parse "il cane dorme")) [:sem :pred])))))

(deftest lei-scrive
  (testing "'lei scrive' translates as both third person 'she' and formal
            second person 'you'"
    ;; 'lei' is ambiguous between 'she' and the formal 'you', so both readings
    ;; have to survive translation. menard spells second person with a
    ;; formality emoji (cf. menard.test.translate.es-en), and which emoji is
    ;; picked varies, so match the formal set rather than pinning one.
    (let [translations (set (translate/it-to-en-surface-alternatives "lei scrive"))]
      (is (contains? translations "she writes"))
      (is (some #(contains? translations (str "you " % " write")) m/formal)
          (str "no formal 'you ... write' in: " (pr-str translations)))))
  (testing "the third-person reading keeps the feminine gender that the verb's
            :agr does not carry"
    (is (some #(= :she (get-in % [:sem :subj :pred]))
              (map translate/it-structure-to-en-spec (it/parse "lei scrive")))))
  (testing "the formal reading generates with english's second-person agreement"
    (let [formal-spec (first (filter #(= :you (get-in % [:sem :subj :pred]))
                                     (map translate/it-structure-to-en-spec
                                          (it/parse "lei scrive"))))]
      (is (= :2nd (get-in formal-spec [:agr :person])))
      (is (= :sing (get-in formal-spec [:agr :number])))
      (is (= :formal (get-in formal-spec [:sem :subj :ref :context]))))))

(deftest second-person-formality-emojis
  (testing "the formal 'Lei' only ever gets a formal (older person) emoji"
    ;; english has one 'you' entry per formality, and its :note is what menard
    ;; turns into the emoji, so a subject whose :ref :context is :formal must
    ;; not be spelled with the informal entries.
    (let [translations (->> (range 5)
                            (mapcat (fn [_] (translate/it-to-en-surface-alternatives "Lei trova")))
                            distinct
                            (filter #(str/includes? % "you")))]
      (is (seq translations) "expected at least one 'you ...' translation")
      (doseq [t translations]
        (is (has-emoji-of? t m/formal) (str "no formal emoji in: " t))
        (is (not (has-emoji-of? t m/informal)) (str "informal emoji in: " t)))))
  (testing "the informal 'tu', and an unmarked 2nd person, only ever get an
            informal (child) emoji"
    (doseq [input ["tu balli" "balli"]]
      (let [translations (->> (range 5)
                              (mapcat (fn [_] (translate/it-to-en-surface-alternatives input)))
                              distinct
                              (filter #(str/includes? % "you")))]
        (is (seq translations) (str "expected at least one 'you ...' translation for " input))
        (doseq [t translations]
          (is (has-emoji-of? t m/informal) (str input ", no informal emoji in: " t))
          (is (not (has-emoji-of? t m/formal)) (str input ", formal emoji in: " t)))))))

(deftest second-person-formality-defaulting
  (testing "a 2nd-person subject that does not say whether it is formal is
            english's informal 'you'"
    ;; italiano marks the formal addressee explicitly ("Lei", :ref {:context
    ;; :formal}); without the :context english cannot pick between its "you"
    ;; entries and the emoji menard appends would be arbitrary.
    (doseq [[subj expected] [[{:pred :you} :informal]
                             [{:pred :you :ref {:number :sing :human? true}} :informal]
                             [{:pred :you :ref {:context :informal}} :informal]
                             [{:pred :you :ref {:context :formal}} :formal]]]
      (let [spec (translate/it-structure-to-en-spec
                  {:cat :verb :subcat [] :phrasal? true
                   :agr {:number :sing :person :2nd}
                   :sem {:pred :eat :mod [] :tense :present :aspect :simple
                         :subj subj}})]
        (is (= expected (get-in spec [:sem :subj :ref :context])) (pr-str subj)))))
  (testing "a spec that carries the formality in :agr instead (as a
            conjugation chart does) is understood too"
    (doseq [[agr expected] [[{:person :2nd :number :sing :formal? true} :formal]
                            [{:person :2nd :number :sing :formal? false} :informal]
                            [{:person :2nd :number :sing} :informal]]]
      (let [spec (translate/it-structure-to-en-spec
                  {:cat :verb :subcat [] :phrasal? true
                   :agr agr
                   :sem {:pred :eat :mod [] :tense :present :aspect :simple}})]
        (is (= :you (get-in spec [:sem :subj :pred])))
        (is (= expected (get-in spec [:sem :subj :ref :context])) (pr-str agr))))))

(deftest chart-formal-you-gets-a-formal-emoji
  (testing "a conjugation chart's polite 2nd person generates 'you' plus an
            older-person emoji"
    ;; This is the shape an application generates from when it builds the
    ;; english spec itself instead of translating a parse (verbcoach does this
    ;; for its italian conjugation chart): :agr :formal? true is the polite
    ;; second person, i.e. "Lei".
    (let [spec (translate/it-structure-to-en-spec
                {:cat :verb :subcat [] :phrasal? true
                 :agr {:person :2nd :number :sing :formal? true}
                 :sem {:pred :read :mod [] :tense :conditional :aspect :simple}})
          translations (->> (range 5) (map (fn [_] (-> spec en/generate en/morph))) distinct)]
      (is (seq translations))
      (doseq [t translations]
        (is (str/starts-with? t "you ") (str "expected 'you ...': " t))
        (is (has-emoji-of? t m/formal) (str "no formal emoji in: " t))
        (is (not (has-emoji-of? t m/informal)) (str "informal emoji in: " t))))))

(deftest formal-lei-emojis-per-parse
  (testing "each parse whose subject is the formal 'you' generates an
            older-person emoji"
    ;; The formal reading of "Lei" is the parse whose subject carries
    ;; :ref {:context :formal} (the other readings are "she"). english picks
    ;; one of its "you" entries -- and so the emoji menard appends -- from
    ;; that :context, so every such parse has to come out with an older-person
    ;; emoji, never a child one.
    (let [formal-parses (->> (it/parse "Lei mangia")
                             (filter #(= :formal (u/get-in % [:sem :subj :ref :context]))))]
      (is (seq formal-parses) "expected the formal reading of 'Lei' among the parses")
      (doseq [parse formal-parses]
        (let [translations (->> (range 4)
                                (map (fn [_] (translate/it-structure-to-en-string parse)))
                                (remove nil?)
                                distinct)]
          (is (seq translations) "expected at least one translation")
          (doseq [t translations]
            (is (str/starts-with? t "you ") (str "expected 'you ...': " t))
            (is (has-emoji-of? t m/formal) (str "no formal emoji in: " t))
            (is (not (has-emoji-of? t m/informal)) (str "informal emoji in: " t))))))))

(deftest it-to-en-sentences
  (testing "an explicit subject noun reaches the verb
            (menard.ug/subject-is-comp), rather than being defaulted to a
            pronoun from :agr"
    (is (= :dog (u/get-in (first (it/parse "il cane dorme")) [:sem :subj :pred])))
    (is (= "the dog sleeps" (translate/it-to-en "il cane dorme")))
    (is (= "the cat sleeps" (translate/it-to-en "il gatto dorme")))
    (is (= "the dogs sleep" (translate/it-to-en "i cani dormono"))))
  (testing "the subject is a noun phrase for english (cf. it-to-en-noun-phrases)"
    (let [spec (translate/it-structure-to-en-spec (first (it/parse "il cane dorme")))]
      (is (= :the (get-in spec [:sem :subj :quant])))
      (is (= :dog (get-in spec [:sem :subj :pred])))
      (is (= :sing (get-in spec [:sem :subj :ref :number])))
      ;; without this english generates the existential "there sleeps".
      (is (= false (get-in spec [:sem :subj :existential?]))))))

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

(deftest it-to-en-possessives
  (testing "a possessive determiner becomes english's :quant"
    ;; italiano's possessive carries the possessor in :of (il mio cane is
    ;; :of {:pred :i}); english uses the possessor itself as the quantifier,
    ;; cf. english "my" = {:pred :i} and "my dog" :quant :i
    (is (= "my dog" (translate/it-to-en "il mio cane")))
    (is (= "my dogs" (translate/it-to-en "i miei cani")))
    (is (= "my house" (translate/it-to-en "la mia casa")))
    (is (= "his dog" (translate/it-to-en "il suo cane")))
    (is (= "our dog" (translate/it-to-en "il nostro cane")))
    (is (= "their dog" (translate/it-to-en "il loro cane")))
    (let [spec (translate/it-structure-to-en-spec (first (it/parse "il mio cane")))]
      (is (= :i (get-in spec [:sem :quant])))
      (is (= [] (get-in spec [:sem :mod])))))
  (testing "a possessive subject reaches the verb too"
    (is (= "my dog sleeps" (translate/it-to-en "il mio cane dorme"))))
  (testing "second person keeps menard's person-emoji convention"
    ;; english keeps formality in :context/:note on its "your" entries, and
    ;; menard marks 2nd person with emoji whatever :context is set to (and
    ;; which emoji is picked varies), so match around them rather than pinning
    ;; an emoji.
    (let [en (translate/it-to-en "il tuo cane")]
      (is (re-find #"^your " en))
      (is (re-find #"dog$" en)))))

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
    (is (= :sleep (get-in (translate/it-structure-to-en-spec (first (it/parse "dormo"))) [:sem :pred]))))

  (testing "pronouns and possessives use english's predicate spellings"
    ;; the port kept babel's :I and :you-all, which english spells :i and :you;
    ;; english tells plural 'you' by :agr, not by the predicate. (These look at
    ;; the clause parse: the same words also parse as a bare vp, which has no
    ;; subject.)
    (is (= :i (u/get-in (translate/it-clause-parse (it/parse "io parlo")) [:sem :subj :pred])))
    (is (= :you (u/get-in (translate/it-clause-parse (it/parse "voi parlate")) [:sem :subj :pred])))
    (is (= :i (u/get-in (first (it/analyze "il mio" false)) [:sem :of :pred])))
    (is (= :you (u/get-in (first (it/analyze "il vostro" false)) [:sem :of :pred])))))

(deftest it-to-en-remaining-work
  (testing "documented gaps in the it->en alignment"
    ;; Recorded here rather than asserted, so the gaps stay visible:
    ;;   * a third-person-plural *pro-drop* subject comes out with menard's
    ;;     person-emoji markers ("parlano" -> "they <emojis> talk"), the same
    ;;     convention the español->english tests assert for 2nd person; an
    ;;     explicit subject noun does not ("i cani dormono" -> "the dogs
    ;;     sleep");
    ;;   * menard renders 2nd person with person-emoji markers, so 2nd-person
    ;;     subjects and possessives ("il tuo cane" -> "your <emoji> dog")
    ;;     carry them; that is menard's convention, not a conversion gap;
    ;;   * the conjoined pronoun subjects ("Giovanni e io", "Luisa e io") have
    ;;     no english predicate, so they fall back to the agreement pronoun
    ;;     ("we"), which loses the conjunction;
    ;;   * 155 italian predicates have no english counterpart at all
    ;;     ("donna" is :pred :donna where english has :woman, "madre" :madre
    ;;     vs :mother, "birra"/"vino" vs :beer/:wine), so those nouns cannot
    ;;     translate; some are simply unaligned and some are real gaps.
    ;; The italian predicates for common nouns and verbs are already
    ;; english-aligned (cane -> :dog, dormire -> :sleep, parlare ->
    ;; :talk/:speak).
    (is (string? (translate/it-to-en "parlo")))))
