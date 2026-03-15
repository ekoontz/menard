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

;; TODO: factor out commonalities with menard.translate.es-en

(defn structure-to-en-spec [structure]
  (log/debug (str "structure-to-en-spec tree: " (it/syntax-tree structure)))
  (let [sem-mod (if (not (= (u/get-in structure [:sem :mod]) []))
                  (u/get-in structure [:sem :mod])
                  [])

        ;; TODO: what is going on here?
        pronoun? (if (= (u/get-in structure [:pronoun?] ::none) ::none)
                   (u/get-in structure [:pronoun?])
                   false)]
    (unify {:pronoun? pronoun?
            :sem {:mod sem-mod}}
           {:agr (-> structure (u/get-in [:agr]))
            :sem (-> structure (u/get-in [:sem]))
            :cat (-> structure (u/get-in [:cat]))
            :phrasal? (-> structure (u/get-in [:phrasal?] :top))
            :subcat (-> structure (u/get-in [:subcat]))})))

(defn en-structure-to-spec [en-structure]
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

(defn structure-to-en-structure [structure & [model en-model]]
  (let [english-spec (structure-to-en-spec structure)
        en-model (or en-model @en-complete/model)]
    (log/debug (str "structure-to-en-structure: " (it/syntax-tree structure)))
    (log/debug (str "structure-to-en-structure: english input spec for generation: " (l/pprint english-spec)))
    (log/debug (str "structure-to-en-structure: english input spec for generation (serialized): " (dag_unify.serialization/serialize english-spec)))
    (let [en-expression (try (-> english-spec (en/generate en-model))
                             (catch Exception e
                               (log/error (str "it-to-en: failed to generate an English expression "
                                               "from spec: (serialized) "
                                               (dag_unify.serialization/serialize english-spec)))
                               {}))]
      (log/debug (str "it-to-en: en-expression: " (en/syntax-tree en-expression)))
      (when (nil? en-expression)
        (log/error (str "could not generate english expression for target: "
                        (it/syntax-tree structure) ";spec: " (l/pprint english-spec))))
      (log/debug (str "en-expression: " (l/pprint en-expression)))
      (log/debug (str "morph(en-expression): " (en/morph en-expression)))
      en-expression)))
  
(defn string-to-en-structure
  "return one English structure translation for the given Spanish input."  
  [input & [model en-model]]
  (if input
    (log/debug (str "string-to-en-structure: input: " input))    
    (log/error (str "string-to-en-structure: input was null.")))
  (log/debug (str "string-to-en-structure: starting with input: " input))
  (let [model (or model it/model)
        parses (it/parse input model)]
    (if (seq parses)
      (do
        (log/debug (str "translating es parse: " (it/syntax-tree (first parses))))
        (-> parses first (structure-to-en-structure model en-model)))
      (log/error (str "could not parse input: '" input "'")))))

(defn to-en-structure-alternatives
  "return several English structure translations for the given Spanish input."  
  [input & [model en-model]]
  (if input
    (log/debug (str "to-en: input: " input))    
    (log/error (str "to-en: input was null.")))
  (log/debug (str "to-en: starting with input: " input))
  (let [model (or model it/model)
        parses (-> input
                      (it/parse (ref model)))]
    (->> parses
         (map #(structure-to-en-structure % model en-model))
         (remove nil?))))

(defn to-en-surface-alternatives
  "return several English surface translations for the given Spanish input."  
  [input & [model en-model]]
  (if input
    (log/debug (str "to-en: input: " input))    
    (log/error (str "to-en: input was null.")))
  (log/debug (str "to-en: starting with input: " input))
  (->> (to-en-structure-alternatives input model en-model)
       (map en/morph)))

  [parse & [model en-model]]
  (-> parse
      (structure-to-en-structure model en-model)
      en/morph))
   
(defn string-to-string
  "return one English string translation for the given Spanish input."
  [input & [model en-model]]
  (-> input
      (string-to-en-structure model en-model)
      en/morph))

(defn translate [it]
  (-> it string-to-string))
