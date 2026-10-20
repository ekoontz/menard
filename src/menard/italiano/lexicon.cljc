(ns menard.italiano.lexicon
  "Lexicon preparation for italiano:

   * turn the babel-style :italiano irregular data kept in
     resources/italiano/lexicon/verbs.edn into menard :exceptions, so that
     morph-leaf and generation can inflect irregular verbs (vado, ho, sono,
     farò, detto, ...) instead of falling back to the regular rules;
   * patch graph :root propagation for noun phrases (see the :unify lists in
     resources/italiano/grammar.edn)."
  (:require [dag_unify.core :as u]
            [menard.lexiconfn :as l]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [menard.log :as log])))

(def person-keys
  {:1sing [:1st :sing]
   :2sing [:2nd :sing]
   :3sing [:3rd :sing]
   :1plur [:1st :plur]
   :2plur [:2nd :plur]
   :3plur [:3rd :plur]})

;; regular endings applied to a verb's :future-stem:
(def future-endings
  {:1sing "ò" :2sing "ai" :3sing "à"
   :1plur "emo" :2plur "ete" :3plur "anno"})

(def conditional-endings
  {:1sing "ei" :2sing "esti" :3sing "ebbe"
   :1plur "emmo" :2plur "este" :3plur "ebbero"})

(defn forms->exceptions
  "Convert a {person-key surface} map of irregular forms into :exceptions
   entries for the given inflection."
  [infl forms]
  (->> forms
       (keep (fn [[person-key surface]]
               (when-let [[person number] (person-keys person-key)]
                 {:surface surface
                  :infl infl
                  :agr {:person person :number number}})))))

(defn stem->exceptions
  "Apply the regular endings of the given tense to a future stem, e.g.
   andare's \"andr\" + \"ò\" = \"andrò\"."
  [infl stem endings]
  (forms->exceptions infl (map (fn [[person-key ending]] [person-key (str stem ending)])
                               endings)))

(defn participle-variants
  "Italian past participles agree in gender and number when the auxiliary is
   essere, so derive the other three forms from the (masculine singular)
   irregular participle by changing its final vowel."
  [participle]
  (let [stem (subs participle 0 (dec (count participle)))
        ending (last participle)]
    (cond
      (= ending \o)
      [{:surface participle :agr {:number :sing :gender :masc}}
       {:surface (str stem "a") :agr {:number :sing :gender :fem}}
       {:surface (str stem "i") :agr {:number :plur :gender :masc}}
       {:surface (str stem "e") :agr {:number :plur :gender :fem}}]

      (= ending \a)
      [{:surface participle :agr {:number :sing :gender :fem}}
       {:surface (str stem "i") :agr {:number :plur :gender :masc}}
       {:surface (str stem "e") :agr {:number :plur :gender :fem}}]

      :else
      [{:surface participle :agr {:number :sing :gender :masc}}])))

(defn lexeme->exceptions
  "Return lexeme with :exceptions derived from its :italiano irregular data
   (or unchanged when it has none)."
  [lexeme]
  (let [italiano (:italiano lexeme)
        exceptions
        (when (and (= :verb (:cat lexeme))
                   (map? italiano))
          (->> [(when (map? (:present italiano))
                  (forms->exceptions :present (:present italiano)))
                (when (map? (:imperfetto italiano))
                  (forms->exceptions :imperfetto (:imperfetto italiano)))
                (when (map? (:future italiano))
                  (forms->exceptions :future (:future italiano)))
                (when (map? (:conditional italiano))
                  (forms->exceptions :conditional (:conditional italiano)))
                (when-let [stem (:future-stem italiano)]
                  (stem->exceptions :future stem future-endings))
                (when-let [stem (:future-stem italiano)]
                  (stem->exceptions :conditional stem conditional-endings))
                (when (string? (:passato italiano))
                  (map (fn [{:keys [surface agr]}]
                         {:surface surface :infl :passato :agr agr})
                       (participle-variants (:passato italiano))))
                (when (string? (:gerund italiano))
                  [{:surface (:gerund italiano) :infl :gerund}])]
               (remove empty?)
               (mapcat identity)))]
    (if (seq exceptions)
      (assoc lexeme :exceptions exceptions)
      lexeme)))

(defn add-irregular-exceptions
  "Add :exceptions (derived from :italiano) to every lexeme in lexicon."
  [lexicon]
  (into {}
        (map (fn [[canonical lexemes]]
               [canonical (mapv lexeme->exceptions lexemes)]))
        lexicon))

(defn compile-lexicon-fn
  "compile-lexicon-fn for menard.italiano/model-fn: derive irregular
   exceptions, then expand them into lexicon entries."
  [lexicon _morphology _filter-lexicon-fn]
  (-> lexicon
      add-irregular-exceptions
      l/add-exceptions-to-lexicon))
