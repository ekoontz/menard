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

(defn object-pronoun-reflexivity [es-parse]
  (let [reflexive? (u/get-in es-parse [:obj :reflexive?] :none)]
    (if (= reflexive? :none)
      {:obj {:reflexive? reflexive?}}
      :top)))

(defn es-parse-to-en-spec [es-parse]
  (log/debug (str "es-parse-to-en-spec parse: " (es/syntax-tree es-parse)))
  (log/debug (str "es-parse-to-en-spec sem: " (l/pprint (u/get-in es-parse [:sem]))))
  (let [sem-mod (if (not (= (u/get-in es-parse [:sem :mod]) []))
                  (u/get-in es-parse [:sem :mod])
                  [])
        pronoun? (if (= (u/get-in es-parse [:pronoun?] ::none) ::none)
                   (u/get-in es-parse [:pronoun?])
                   false)]
    (unify {:pronoun? pronoun?
            :sem {:mod sem-mod}}
           {:agr (-> es-parse (u/get-in [:agr]))
            :sem (-> es-parse (u/get-in [:sem]))
            :cat (-> es-parse (u/get-in [:cat]))
            :phrasal? (-> es-parse (u/get-in [:phrasal?] :top))
            :subcat (-> es-parse (u/get-in [:subcat]))}
           ;; Below we set [:sem :iobj] to :none by default,
           ;; but we cannot do the same with [:sem :obj] because
           ;; reflexive verbs in Spanish do not have an [:sem :obj],
           ;; but the translation in English, depending on the verb,
           ;; might have a [:sem :obj].
           ;; For example, 'levantarse' (get up) has no [:sem :obj]
           ;; in English, but 'pettinarse' (comb oneself) *does*
           ;; have a [:sem :obj].
           {:sem {:iobj (-> es-parse (u/get-in [:sem :iobj] :none))}}

           ;; if there's an object pronoun, and it's reflexive, then set this in english too:
           (object-pronoun-reflexivity es-parse))))

(defn es-structure-to-en-structure [es-parse & [es-model en-model]]
  (let [english-spec (es-parse-to-en-spec es-parse)
        en-model (or en-model @en-complete/model)]
    (log/debug (str "es-structure-to-en-structure: " (es/syntax-tree es-parse)))
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
                        (es/syntax-tree es-parse) ";spec: " (l/pprint english-spec))))
      (log/info (str "en-expression: " (l/pprint en-expression)))
      (log/info (str "morph(en-expression): " (en/morph en-expression)))
      en-expression)))
  
(defn es-string-to-en-structure
  "return one English structure translation for the given Spanish input."  
  [es-input & [es-model en-model]]
  (if es-input
    (log/debug (str "es-string-to-en-structure: es-input: " es-input))    
    (log/error (str "es-string-to-en-structure: es-input was null.")))
  (log/info (str "es-string-to-en-structure: starting with es-input: " es-input))
  (let [es-model (or es-model es/model)
        parses (es/parse es-input es-model)]
    (if (seq parses)
      (do
        (log/info (str "translating es parse: " (es/syntax-tree (first parses))))
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
