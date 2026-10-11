(ns menard.translate.it-en
  (:require [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            [dag_unify.diagnostics :as diag]
            [menard.english :as en]
            [menard.english.complete :as en-complete]
            [menard.italiano :as it]
            [menard.generate :as g]
            [menard.lexiconfn :as l]            
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [menard.log :as log])))

(defn- it-det->en-quant
  "italiano keeps its determiner as a modifier (:mod :first, :pred :definite /
   :indefinite); english wants it as :sem :quant, so move it."
  [sem]
  (let [quant (case (get-in sem [:mod :first :pred])
                :definite :the
                :indefinite :some
                nil)]
    (if quant
      (-> sem
          (assoc :quant quant)
          (assoc :mod (or (get-in sem [:mod :rest]) [])))
      sem)))

(defn- it-number->en-ref
  "carry the number across as english's :ref."
  [sem agr]
  (let [n (let [x (:number sem)] (if (or (nil? x) (= :top x)) (:number agr) x))
        ;; singular forms are unmarked in the italian structures, so default
        ;; to :sing when nothing says otherwise.
        n (if (or (nil? n) (= :top n)) :sing n)]
    (assoc sem :ref {:number n})))

(defn- resolve-dag
  "resolve dag atoms (menard represents unbound feature values as clojure atoms)
   so the conversion below sees plain values."
  [x]
  (cond #?(:clj (instance? clojure.lang.IDeref x) :cljs false) (resolve-dag @x)
        (map? x) (into {} (map (fn [[k v]] [k (resolve-dag v)])) x)
        (vector? x) (mapv resolve-dag x)
        :else x))

(defn- it-tense->en-sem
  "map the italiano :infl onto the :sem :tense/:aspect features english uses
   (cf. what menard.translate.es-en gets for free from the español structures)."
  [infl]
  (case infl
    :present     {:tense :present :aspect :simple}
    :future      {:tense :future}
    :conditional {:tense :conditional}
    :imperfetto  {:tense :past :aspect :progressive}
    :passato     {:tense :present :aspect :perfect}
    {}))

(defn- it-agr->en-subject
  "italiano is pro-drop: the subject is only marked by :agr, so synthesize the
   english subject pronoun english needs to generate deterministically."
  [agr]
  (let [person (case (:person agr) :1st 1 :2nd 2 :3rd 3 (:person agr))
        number (:number agr)]
    (cond (= [1 :sing] [person number]) :i
          (= [2 :sing] [person number]) :you
          (= [3 :sing] [person number]) :he
          (= [1 :plur] [person number]) :we
          (= [2 :plur] [person number]) :you
          (= [3 :plur] [person number]) :they
          :else nil)))

(def ^:private it-pronoun-preds
  "italiano's pronoun predicates (cf. resources/italiano/lexicon/pronouns.edn).
   english spells its pronouns itself, so a pronoun subject is realized from
   :agr (it-agr->en-subject) rather than from its italiano predicate."
  #{:i :I :we :you :you-all :he :she :they :it})

(defn- it-subject->en-subject
  "The clause's subject as english wants it.

   A sentence with an explicit subject noun gets its subject semantics from the
   grammar (menard.ug/subject-is-comp), so an italian noun phrase has to be
   converted the same way a noun phrase is: determiner to :quant, number to
   :ref. It is then marked non-existential, without which english generates the
   existential 'there sleeps' instead of 'the dog sleeps'.

   An italian pronoun subject, or no subject at all (italiano is pro-drop), is
   synthesized from :agr: english has no italian pronoun predicates."
  [it-subj agr]
  (if (and it-subj (not (contains? it-pronoun-preds (:pred it-subj))))
    (-> it-subj
        (it-det->en-quant)
        (it-number->en-ref agr)
        (assoc :existential? false))
    (when-let [pronoun (it-agr->en-subject agr)]
      {:pred pronoun
       :ref {:number (:number agr) :human? true}
       :existential? false
       :mod []})))

(defn it-structure-to-en-spec
  "Turn a parsed italiano structure into a spec menard.english can generate from.

   The conversion in menard.translate.es-en works because the español structures
   already have english's shape (tense/aspect inside :sem, a :sem :subj, :subcat
   as a list, :phrasal?). The ported italiano structures are flatter, so this
   aligns them with the english lexicon's expectations:
     * :infl becomes :sem :tense/:aspect
     * :sem :subj is an explicit subject noun if the sentence had one, or
       synthesized from :agr (italiano is pro-drop) otherwise
     * :sem :mod and :subcat default to []
     * nil/:top placeholders are dropped"
  [it-structure]
  (log/debug (str "it-structure-to-en-spec tree: " (it/syntax-tree it-structure)))
  (let [agr (let [a (resolve-dag (u/get-in it-structure [:agr]))] (if (map? a) a {}))
        infl (resolve-dag (u/get-in it-structure [:infl]))
        sem0 (let [s (resolve-dag (u/get-in it-structure [:sem]))] (if (map? s) s {}))
        it-subj (let [s (:subj sem0)]
                  (when (and (map? s) (some? (:pred s)) (not= :top (:pred s))) s))
        subj (it-subject->en-subject it-subj agr)
        cat (resolve-dag (u/get-in it-structure [:cat]))
        sem0 (cond-> (it-det->en-quant sem0)
               (= :noun cat) (it-number->en-ref agr))
        sem (cond-> (-> (merge sem0 {:mod (or (:mod sem0) [])} (it-tense->en-sem infl))
                        (dissoc :subj))
              subj (assoc :subj subj))
        subcat (let [sc (resolve-dag (u/get-in it-structure [:subcat]))] (if (or (nil? sc) (= :top sc) (map? sc)) [] sc))
        clean (fn [m] (->> m (remove (fn [[_ v]] (or (nil? v) (= :top v)))) (into {})))]
    (clean (cond-> {:cat cat
                    :sem (clean sem)
                    :subcat subcat
                    :phrasal? true}
             (seq agr) (assoc :agr agr)))))

(defn en-structure-to-it-spec [en-structure]
  (log/debug (str "en-structure-to-ee-spec tree: " (en/syntax-tree en-structure)))
  (let [sem-mod (if (not (= (u/get-in en-structure [:sem :mod]) []))
                  (u/get-in en-structure [:sem :mod])
                  [])

        ;; TODO: what is going on here?
        pronoun? (if (= (u/get-in en-structure [:pronoun?] ::none) ::none)
                   (u/get-in en-structure [:pronoun?])
                   false)]
    (unify {:pronoun? pronoun?
            :sem {:mod sem-mod}}
           {:agr (-> en-structure (u/get-in [:agr]))
            :sem (-> en-structure (u/get-in [:sem]))
            :cat (-> en-structure (u/get-in [:cat]))
            :phrasal? (-> en-structure (u/get-in [:phrasal?] :top))
            :subcat (-> en-structure (u/get-in [:subcat]))})))

(defn it-structure-to-en-structure [it-structure & [it-model en-model]]
  (let [english-spec (it-structure-to-en-spec it-structure)
        en-model (or en-model @en-complete/model)]
    (log/debug (str "it-structure-to-en-structure: " (it/syntax-tree it-structure)))
    (log/debug (str "it-structure-to-en-structure: english input spec for generation: " (l/pprint english-spec)))
    (log/debug (str "it-structure-to-en-structure: english input spec for generation (serialized): " (dag_unify.serialization/serialize english-spec)))
    (let [en-expression (try (-> english-spec (en/generate en-model))
                             (catch #?(:clj Exception :cljs :default) e
                               (log/error (str "it-to-en: failed to generate an English expression "
                                               "from spec: (serialized) "
                                               (dag_unify.serialization/serialize english-spec)))
                               {}))]
      (log/debug (str "it-to-en: en-expression: " (en/syntax-tree en-expression)))
      (when (nil? en-expression)
        (log/error (str "could not generate english expression for target: "
                        (it/syntax-tree it-structure) ";spec: " (l/pprint english-spec))))
      (log/debug (str "en-expression: " (l/pprint en-expression)))
      (log/debug (str "morph(en-expression): " (en/morph en-expression)))
      en-expression)))
  
(defn it-string-to-en-structure
  "return one English structure translation for the given Spanish input."  
  [it-input & [it-model en-model]]
  (if it-input
    (log/debug (str "it-string-to-en-structure: it-input: " it-input))    
    (log/error (str "it-string-to-en-structure: it-input was null.")))
  (log/debug (str "it-string-to-en-structure: starting with it-input: " it-input))
  (let [it-model (or it-model it/model)
        parses (it/parse it-input it-model)]
    (if (seq parses)
      (do
        (log/debug (str "translating it parse: " (it/syntax-tree (first parses))))
        (-> parses first (it-structure-to-en-structure it-model en-model)))
      (log/error (str "could not parse it-input: '" it-input "'")))))

(defn it-to-en-structure-alternatives
  "return several English structure translations for the given Spanish input."  
  [it-input & [it-model en-model]]
  (if it-input
    (log/debug (str "it-to-en: it-input: " it-input))    
    (log/error (str "it-to-en: it-input was null.")))
  (log/debug (str "it-to-en: starting with it-input: " it-input))
  (let [it-model (or it-model it/model)
        it-parses (-> it-input
                      (it/parse it-model))]
    (->> it-parses
         (map #(it-structure-to-en-structure % it-model en-model))
         (remove nil?))))

(defn it-to-en-surface-alternatives
  "return several English surface translations for the given Spanish input."  
  [it-input & [it-model en-model]]
  (if it-input
    (log/debug (str "it-to-en: it-input: " it-input))    
    (log/error (str "it-to-en: it-input was null.")))
  (log/debug (str "it-to-en: starting with it-input: " it-input))
  (->> (it-to-en-structure-alternatives it-input it-model en-model)
       (map en/morph)))

(defn it-structure-to-en-string
  [it-parse & [it-model en-model]]
  (-> it-parse
      (it-structure-to-en-structure it-model en-model)
      en/morph))
   
(defn it-to-en
  "return one English string translation for the given Spanish input."
  [it-input & [it-model en-model]]
  (-> it-input
      (it-string-to-en-structure it-model en-model)
      en/morph))

(defn translate [it]
  (-> it it-to-en))
