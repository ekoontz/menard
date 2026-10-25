(ns menard.italiano.conjugate
  "Conjugation charts for Italian verbs, mirroring menard.español.conjugate.

   Simple tenses come straight from the morphology; compound tenses compose
   the auxiliary (avere/essere) with the past participle, and the progressive
   composes stare with the gerund."
  (:require [menard.italiano :as it]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [menard.log :as log])))

(def persons [:1st :2nd :3rd])

(def tenses
  "Italian tense variants, keyed by the :sem spec a verb set stores."
  [:present-simple :present-progressive :future :conditional :imperfetto
   :passato :trapassato])

(def tense-name->tense
  {"presente" :present-simple
   "present progressive" :present-progressive
   "futuro" :future
   "condizionale" :conditional
   "imperfetto" :imperfetto
   "passato" :passato
   "trapassato" :trapassato})

(def tense->infl
  {:present-simple :present
   :future :future
   :conditional :conditional
   :imperfetto :imperfetto
   :passato :passato
   :trapassato :passato
   :present-progressive :present})

(def tense->sem
  {:present-simple {:tense :present :aspect :simple}
   :present-progressive {:tense :present :aspect :progressive}
   :future {:tense :future}
   :conditional {:tense :conditional}
   :imperfetto {:tense :past :aspect :progressive}
   :passato {:tense :past :aspect :perfect}
   :trapassato {:tense :past :aspect :pluperfect}})

(def person-number->clitic
  {:1st {:sing "mi" :plur "ci"}
   :2nd {:sing "ti" :plur "vi"}
   :3rd {:sing "si" :plur "si"}})

(defn- model [] @it/model)

(defn- morphology [] (:morphology (model)))

(defn- lexicon [] (:lexicon (model)))

(def ^:private irregular-index-cache (atom nil))

(defn- build-irregular-index [lexicon]
  (->> lexicon
       (mapcat val)
       (filter #(true? (:exception? %)))
       ;; resolve :canonical: it can be a dag-unify atom.
       (group-by #(u/get-in % [:canonical]))))

(defn- irregular-index
  "canonical -> [exception lexemes] for the current model, cached per model load."
  []
  (let [loaded-when (:loaded-when (model))]
    (or (when (= loaded-when (:loaded-when @irregular-index-cache))
          (:index @irregular-index-cache))
        (let [index (build-irregular-index (lexicon))]
          (reset! irregular-index-cache {:loaded-when loaded-when
                                         :index index})
          index))))

(defn verb-canonicals
  "All verb canonicals in the model's lexicon (used when a set doesn't name
   any verbs, i.e. \"any verb\")."
  []
  (->> (lexicon)
       (keep (fn [[canonical lexemes]]
               (when (some #(and (= :verb (:cat %))
                                 (not (true? (:aux %)))
                                 (not (true? (:inflected? %))))
                           lexemes)
                 canonical)))
       sort
       vec))

(defn verb-lexeme
  "The base (non-auxiliary) verb lexeme for canonical, or nil."
  [canonical]
  (->> (get (lexicon) canonical)
       (filter #(and (= :verb (:cat %))
                     (not (true? (:aux %)))))
       first))

(defn reflexive? [canonical]
  (let [lexeme (verb-lexeme canonical)]
    (or (true? (:reflexive lexeme))
        (true? (u/get-in lexeme [:sem :reflexive])))))

(defn object-less?
  "True if the lexicon says canonical takes no direct object: andare and
   arrivare carry :subcat {:2 []} (an empty second argument slot), while a verb
   that can take an object leaves the slot open."
  [canonical]
  (let [lexeme (verb-lexeme canonical)]
    (and (some? lexeme)
         (= [] (u/get-in lexeme [:subcat :2])))))

(defn takes-indirect-object?
  "True if the lexicon marks canonical as taking an indirect object (a
   recipient): dare, dire, mostrare, portare, scrivere carry
   :indirect-object? true, which is what
   menard.italiano.conjugate/with-clitics needs to add gli/le."
  [canonical]
  (let [lexeme (verb-lexeme canonical)]
    (and (some? lexeme)
         (true? (:indirect-object? lexeme)))))

(defn takes-human-object?
  "True if canonical can take a human direct object, so that a clitic like lo
   or la can stand for a person: false for a verb that takes no direct object
   (object-less?) and for one whose object must be inanimate (giocare,
   suonare, whose lexicon entries carry :sem {:obj {:human false}})."
  [canonical]
  (let [lexeme (verb-lexeme canonical)]
    (and (some? lexeme)
         (not (object-less? canonical))
         (not= false (u/get-in lexeme [:sem :obj :human])))))

(defn essere? [canonical]
  (let [lexeme (verb-lexeme canonical)]
    (or (true? (:essere lexeme))
        (reflexive? canonical))))

(defn auxiliary [canonical]
  (if (essere? canonical) "essere" "avere"))

(defn- irregular-surface
  "The irregular surface form for canonical + infl + agreement, if any."
  [canonical infl agr]
  (->> (get (irregular-index) canonical)
       (filter #(and (string? (:surface %))
                     (not= :fail (u/unify (dissoc % :surface)
                                          {:infl infl :agr agr}))))
       first
       :surface))

(defn inflect
  "The surface form of canonical in the given inflection and agreement."
  [canonical infl agr]
  (or (irregular-surface canonical infl agr)
      (morph-leaf {:canonical canonical
                   :cat :verb
                   :infl infl
                   :agr agr}
                  (morphology))))

(defn participle
  "The past participle of canonical, agreeing with the given number and gender."
  [canonical number gender]
  (inflect canonical :passato {:number number :gender gender}))

(defn past-participle
  "The past participle used with auxiliary canonical: it agrees with the
   subject when the auxiliary is essere, and is invariant (masculine singular)
   with avere."
  ([canonical number] (past-participle canonical number :masc))
  ([canonical number gender]
   (if (= "essere" (auxiliary canonical))
     (participle canonical number gender)
     (participle canonical :sing :masc))))

(defn gerund [canonical]
  (inflect canonical :gerund {}))

(defn verb-form
  "The Italian surface form (without subject pronoun) for canonical in the
   given tense variant and agreement. gender only matters for the participle
   of verbs whose auxiliary is essere."
  ([canonical tense person number] (verb-form canonical tense person number :masc))
  ([canonical tense person number gender]
   (let [agr {:person person :number number}
         infl (get tense->infl tense)]
     (case tense
       (:present-simple :future :conditional :imperfetto)
       (inflect canonical infl agr)

       :passato
       (str (inflect (auxiliary canonical) :present agr) " "
            (past-participle canonical number gender))

       :trapassato
       (str (inflect (auxiliary canonical) :imperfetto agr) " "
            (past-participle canonical number gender))

       :present-progressive
       (str (inflect "stare" :present agr) " " (gerund canonical))

       (throw (ex-info (str "unknown italian tense: " tense) {:tense tense}))))))

(defn with-clitic
  "Prefix a reflexive verb's surface form with the appropriate clitic."
  [canonical person number surface]
  (if (reflexive? canonical)
    (str (get-in person-number->clitic [person number]) " " surface)
    surface))

(def person-number-gender->direct-object-clitic
  "Clitic direct object pronouns by agreement: the non-reflexive :case :acc
   pronoun entries in resources/italiano/lexicon/pronouns.edn."
  {:1st {:sing "mi" :plur "ci"}
   :2nd {:sing "ti" :plur "vi"}
   :3rd {:sing {:masc "lo" :fem "la"}
         :plur {:masc "li" :fem "le"}}})

(defn direct-object-clitic
  "The clitic direct object pronoun for the given agreement, or nil."
  ([person number]
   (direct-object-clitic person number :masc))
  ([person number gender]
   (let [clitic (get-in person-number-gender->direct-object-clitic [person number])]
     (if (map? clitic)
       (get clitic (or gender :masc))
       clitic))))

(def ^:private eliding-clitics
  "lo and la elide before a vowel: l'ho visto, l'amavo (mi, ti, ci and vi do
   not: mi alzo, ci hanno visto)."
  #{"lo" "la"})

(def ^:private elides-before
  "a vowel, initial h included: avere's forms (ho, ha, hanno) are spelled with
   a silent h, and lo ha visto elides to l'ha visto."
  #"^([aeiouàèéìòù]|h)")

(def person-number-gender->indirect-object-clitic
  "Clitic indirect object pronouns by agreement: the dative (\"to someone\")
   pronouns. 'gli' covers both a lui (to him) and a loro (to them) in modern
   Italian; 'le' is a lei (to her)."
  {:1st {:sing "mi" :plur "ci"}
   :2nd {:sing "ti" :plur "vi"}
   :3rd {:sing {:masc "gli" :fem "le"}
         :plur {:masc "gli" :fem "gli"}}})

(defn indirect-object-clitic
  "The clitic indirect object pronoun for the given agreement, or nil."
  ([person number]
   (indirect-object-clitic person number :masc))
  ([person number gender]
   (let [clitic (get-in person-number-gender->indirect-object-clitic [person number])]
     (if (map? clitic)
       (get clitic (or gender :masc))
       clitic))))

(def ^:private dative-before-object
  "mi/ti/ci/vi become me/te/ce/ve before another clitic: me lo dai, ce lo
   danno, me l'ha dato."
  {"mi" "me" "ti" "te" "ci" "ce" "vi" "ve"})

(defn with-clitics
  "Prefix _surface_ with the clitics for the objects in _objects_, a map of
   :direct and/or :indirect, each of [person number gender].

   The dative comes first, and the third person dative fuses with the
   accusative into one word: 'glielo do', 'gliela do', 'gliel'ho dato'; the
   first and second person datives stay separate: 'me lo dai', 'me l'ha
   dato'. lo and la elide before a vowel (see elides-before)."
  [surface {:keys [direct indirect]}]
  (let [dative (when indirect (apply indirect-object-clitic indirect))
        object (when direct (apply direct-object-clitic direct))
        eliding? (and object
                      (contains? eliding-clitics object)
                      (re-find elides-before surface))
        object-string (when object
                        (if eliding?
                          (str (subs object 0 1) "'")
                          (str object " ")))]
    (cond
      (nil? dative) (str object-string surface)

      (nil? object) (str dative " " surface)

      ;; gli/le + accusative fuse: glielo, gliela, glieli, gliele
      (contains? #{"gli" "le"} dative) (str "glie" object-string surface)

      ;; mi/ti/ci/vi stay a separate word: me lo, te lo, ce lo, ve lo
      :else (str (get dative-before-object dative dative) " " object-string surface))))

(defn with-direct-object-clitic
  "Prefix _surface_ with the direct object clitic for the given agreement:
   lo vedo, l'ho visto."
  [person number gender surface]
  (with-clitics surface {:direct [person number gender]}))

(defn with-indirect-object-clitic
  "Prefix _surface_ with the indirect object clitic for the given agreement:
   gli do, le do, mi dai."
  ([person number surface]
   (with-indirect-object-clitic person number :masc surface))
  ([person number gender surface]
   (with-clitics surface {:indirect [person number gender]})))

(defn form-with-clitic
  "The tense form of canonical including the reflexive clitic, if any."
  ([canonical tense person number]
   (form-with-clitic canonical tense person number :masc))
  ([canonical tense person number gender]
   (with-clitic canonical person number
                (verb-form canonical tense person number gender))))

(defn verb
  "Return {:singular {person surface} :plural {person surface}} for canonical."
  [canonical tense]
  (let [surface-fn (fn [person number]
                     (form-with-clitic canonical tense person number))]
    {:singular (into {} (map (fn [person] [person (surface-fn person :sing)]) persons))
     :plural (into {} (map (fn [person] [person (surface-fn person :plur)]) persons))}))

(defn generate-chart
  "A conjugation chart for canonical, in the shape verbcoach's conjugate.mst
   template expects."
  [canonical _async?]
  {:canonical canonical
   :moods [{:name "Modo indicativo" :css-class "indicativo"
            :inflections [(merge {:name "Presente"} (verb canonical :present-simple))
                          (merge {:name "Passato prossimo"} (verb canonical :passato))
                          (merge {:name "Imperfetto"} (verb canonical :imperfetto))
                          (merge {:name "Trapassato prossimo"} (verb canonical :trapassato))
                          (merge {:name "Futuro"} (verb canonical :future))]}
           {:name "Modo condizionale" :css-class "condicional"
            :inflections [(merge {:name "Condizionale presente"} (verb canonical :conditional))]}
           {:name "Perifrasi" :css-class "perifrasi"
            :inflections [(merge {:name "Presente progressivo"} (verb canonical :present-progressive))]}]})
