(ns menard.translate.es-en
  (:require [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            [dag_unify.diagnostics :as diag]
            [menard.english :as en]
            [menard.english.complete :as en-complete]
            [menard.español :as es]
            [menard.generate :as g]
            [menard.lexiconfn :as l]            
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [menard.log :as log])))


(defn es-reflexivity-to-en [es-structure]
  ;; English reflexivity is more constrained than Spanish:
  ;; only a subset of reflexive Spanish expressions are also 
  ;; reflexive in English, so we put up a number of conditions here
  ;; to test if the expression should really be reflexive in English:
  ;; An English expression 'en' that is equivalent to a Spanish expression 'es' iff:
  ;; 1. 'es' is reflexive
  ;; 2. 'es' has a non-empty object
  ;; 3. The subject and object of 'es' are referentially identical.
  (and (u/get-in es-structure [:reflexive?] false)
       (not (= :none (u/get-in es-structure [:sem :obj])))
       ;; here we are testing for referential equality:
       (= (get (u/get-in es-structure [:sem :subj]) :ref)
          (get (u/get-in es-structure [:sem :obj]) :ref))))

(defn en-reflexivity-to-es [en-structure]
  (if (= true (u/get-in en-structure [:reflexive?] false))
    true
    :top))

(defn es-structure-to-en-spec [es-structure]
  (log/debug (str "es-structure-to-en-spec:               tree: " (es/syntax-tree es-structure)))
  (log/debug (str "es-structure-to-en-spec:              morph: " (es/morph es-structure)))
  (log/debug (str "es-structure-to-en-spec: es-structure's sem: " (l/pprint (u/get-in es-structure [:sem]))))
  (let [sem-mod (if (not (= (u/get-in es-structure [:sem :mod]) []))
                  (u/get-in es-structure [:sem :mod])
                  [])

        ;; TODO: what is going on here?
        pronoun? (if (= (u/get-in es-structure [:pronoun?] ::none) ::none)
                   (u/get-in es-structure [:pronoun?])
                   false)]
    ;; Note that the Spanish :phrasal? cannot be copied over to English
    ;; because Spanish allows non-phrasal expressions like "duermo"
    ;; which must be translated with an explicit subject in English ("I sleep")
    (unify {:pronoun? pronoun?
            :sem {:mod sem-mod}}
           {:agr (-> es-structure (u/get-in [:agr]))
            :sem (-> es-structure (u/get-in [:sem]))
            :cat (-> es-structure (u/get-in [:cat]))
            :reflexive? (-> es-structure es-reflexivity-to-en)
            :subcat (-> es-structure (u/get-in [:subcat]))})))

(defn en-structure-to-es-spec [en-structure]
  (log/debug (str "en-structure-to-ee-spec tree: " (en/syntax-tree en-structure)))
  (let [sem-mod (if (not (= (u/get-in en-structure [:sem :mod]) []))
                  (u/get-in en-structure [:sem :mod])
                  [])

        ;; TODO: what is going on here?
        pronoun? (if (= (u/get-in en-structure [:pronoun?] ::none) ::none)
                   (u/get-in en-structure [:pronoun?])
                   false)]
    ;; Note that the English :phrasal? cannot be copied over to Spanish
    ;; because Spanish allows non-phrasal expressions like "duermo"
    ;; which must be translated with an explicit subject in English ("I sleep")
    (unify {:pronoun? pronoun?
            :sem {:mod sem-mod}}
           {:agr (-> en-structure (u/get-in [:agr]))
            :sem (-> en-structure (u/get-in [:sem]))
            :cat (-> en-structure (u/get-in [:cat]))
            :reflexive? (-> en-structure en-reflexivity-to-es)
            :subcat (-> en-structure (u/get-in [:subcat]))})))

(defn es-structure-to-en-structure [es-structure & [es-model en-model]]
  (let [english-spec (es-structure-to-en-spec es-structure)
        en-model (or en-model @en-complete/model)]
    (log/debug (str "es-structure-to-en-structure: " (es/syntax-tree es-structure)))
    (log/debug (str "es-structure-to-en-structure: english input spec for generation: " (l/pprint english-spec)))
    (log/debug (str "es-structure-to-en-structure: english input spec for generation (serialized): " (dag_unify.serialization/serialize english-spec)))
    (let [en-expression (try (-> english-spec (en/generate en-model))
                             (catch Exception e
                               (log/error (str "es-to-en: failed to generate an English expression "
                                               "from spec: (serialized) "
                                               (dag_unify.serialization/serialize english-spec)))
                               {}))]
      (log/debug (str "es-to-en: en-expression: " (en/syntax-tree en-expression)))
      (when (nil? en-expression)
        (log/error (str "could not generate english expression for target: "
                        (es/syntax-tree es-structure) ";spec: " (l/pprint english-spec))))
      (log/debug (str "en-expression: " (l/pprint en-expression)))
      (log/debug (str "morph(en-expression): " (en/morph en-expression)))
      en-expression)))
  
(defn es-string-to-en-structure
  "return one English structure translation for the given Spanish input."  
  [es-input & [es-model en-model]]
  (if es-input
    (log/debug (str "es-string-to-en-structure: es-input: " es-input))    
    (log/error (str "es-string-to-en-structure: es-input was null.")))
  (log/debug (str "es-string-to-en-structure: starting with es-input: " es-input))
  (let [es-model (or es-model es/model)
        parses (es/parse es-input es-model)]
    (if (seq parses)
      (do
        (log/debug (str "translating es parse: " (es/syntax-tree (first parses))))
        (-> parses first (es-structure-to-en-structure es-model en-model)))
      (log/error (str "could not parse es-input: '" es-input "'")))))

(defn es-to-en-structure-alternatives
  "return several English structure translations for the given Spanish input."  
  [es-input & [es-model en-model]]
  (if es-input
    (log/debug (str "es-to-en: es-input: " es-input))    
    (log/error (str "es-to-en: es-input was null.")))
  (log/debug (str "es-to-en: starting with es-input: " es-input))
  (let [es-model (or es-model es/model)
        es-parses (-> es-input
                      (es/parse (ref es-model)))]
    (->> es-parses
         (map #(es-structure-to-en-structure % es-model en-model))
         (remove nil?))))

(defn es-to-en-surface-alternatives
  "return several English surface translations for the given Spanish input."  
  [es-input & [es-model en-model]]
  (if es-input
    (log/debug (str "es-to-en: es-input: " es-input))    
    (log/error (str "es-to-en: es-input was null.")))
  (log/debug (str "es-to-en: starting with es-input: " es-input))
  (->> (es-to-en-structure-alternatives es-input es-model en-model)
       (map en/morph)))

(defn es-structure-to-en-string
  [es-parse & [es-model en-model]]
  (-> es-parse
      (es-structure-to-en-structure es-model en-model)
      en/morph))
   
(defn es-to-en
  "return one English string translation for the given Spanish input."
  [es-input & [es-model en-model]]
  (-> es-input (es-string-to-en-structure es-model en-model)
      en/morph))

(defn translate [es]
  (-> es es-to-en))
