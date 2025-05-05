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

(defn es-structure-to-en-spec [es-structure]
  (log/info (str "es-structure-to-en-spec tree: " (es/syntax-tree es-structure)))
  (let [sem-mod (if (not (= (u/get-in es-structure [:sem :mod]) []))
                  (u/get-in es-structure [:sem :mod])
                  [])

        ;; TODO: what is going on here?
        pronoun? (if (= (u/get-in es-structure [:pronoun?] ::none) ::none)
                   (u/get-in es-structure [:pronoun?])
                   false)]
    (unify {:pronoun? pronoun?
            :sem {:mod sem-mod}}
           {:agr (-> es-structure (u/get-in [:agr]))
            :sem (-> es-structure (u/get-in [:sem]))
            :cat (-> es-structure (u/get-in [:cat]))
            :phrasal? (-> es-structure (u/get-in [:phrasal?] :top))
            :subcat (-> es-structure (u/get-in [:subcat]))})))

(defn es-structure-to-en-structure [es-structure & [es-model en-model]]
  (let [english-spec (es-structure-to-en-spec es-structure)
        en-model (or en-model @en-complete/model)]
    (log/info (str "es-structure-to-en-structure: " (es/syntax-tree es-structure)))
    (log/info (str "es-structure-to-en-structure: english input spec for generation: " (l/pprint english-spec)))
    (log/info (str "es-structure-to-en-structure: english input spec for generation (serialized): " (dag_unify.serialization/serialize english-spec)))
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
