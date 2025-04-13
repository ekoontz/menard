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
           {:sem {:iobj (-> es-parse (u/get-in [:sem :iobj] :none))}})))

(defn es-to-en [es-input & [es-model en-model]]
  (if es-input
    (log/debug (str "es-to-en: es-input: " es-input))    
    (log/error (str "es-to-en: es-input was null.")))
  (log/debug (str "es-to-en: starting with es-input: " es-input))
  (let [es-model (or es-model @es/model)
        es-parse (-> es-input
                     ((fn [es-input]
                        (es/parse es-input (ref es-model))))
                     first)
        english-spec (es-parse-to-en-spec es-parse)
        en-model (or en-model @en-complete/model)]
    (if (nil? es-parse)
      (log/error (str "could not parse es-input: '" es-input "'")))
    (log/debug (str "es-to-en: es's phrasal?: "
                   (u/get-in es-parse [:phrasal?])))
    (log/debug (str "          english-spec: " (l/pprint english-spec)))
    (let [en-expression (try (-> english-spec (en/generate en-model))
                             (catch Exception e
                               (log/error (str "es-to-en: failed to generate an English expression "
                                               "from spec: (serialized) "
                                               (dag_unify.serialization/serialize english-spec)))
                               {}))]
      (log/debug (str "es-to-en: en-expression: " (en/syntax-tree en-expression)))
      (if en-expression
        (log/debug (str "successfully generated expression with spec: " (l/pprint english-spec) "; es-input: " es-input))
        (log/error (str "could not generate english expression for spec: " (l/pprint english-spec) "; es-input: " es-input)))
      (let [en-output (-> en-expression en/morph)]
        (log/info (str "es-to-en: " es-input " -> " en-output
                       "; target model: " (-> es-model :name)))
        en-output))))
