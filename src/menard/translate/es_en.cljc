(ns menard.translate.es-en
  (:require [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            [dag_unify.diagnostics :as diag]
            [menard.english :as en]
            [menard.español :as es]
            [menard.generate :as g]
            [menard.lexiconfn :as l]            
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [menard.log :as log])))

(defn es-parse-to-en-spec [es-parse]
  (log/debug (str "es-parse sem: " (l/pprint (u/get-in es-parse [:sem]))))
  (unify {:sem {:mod []}}
         {:agr (-> es-parse (u/get-in [:agr]))
          :sem (-> es-parse (u/get-in [:sem]))
          :cat (-> es-parse (u/get-in [:cat]))
          :subcat (-> es-parse (u/get-in [:subcat]))}
         {:sem {:iobj (-> es-parse (u/get-in [:sem :iobj] :none))
                :obj (-> es-parse (u/get-in [:sem :obj] :none))}}))

(defn es-to-en [es-input]
  (if es-input
    (log/debug (str "es-to-en: es-input: " es-input))    
    (log/error (str "es-to-en: es-input was null.")))

  (let [es-parse (-> es-input es/parse first)
        english-spec (es-parse-to-en-spec es-parse)]
    (log/debug (str "es-to-en: es-parse sem: " (l/pprint (u/get-in es-parse [:sem]))))
    (log/debug (str "es-to-en: english spec: " (l/pprint english-spec)))
    (let [en-expression (-> english-spec en/generate)]
      (log/debug (str "es-to-en: en-expression: " (en/syntax-tree en-expression)))
      (if en-expression
        (log/debug (str "successfully generated expression with spec: " (l/pprint english-spec) "; es-input: " es-input))
        (log/error (str "could not generate english expression for spec: " (l/pprint english-spec) "; es-input: " es-input)))
      (let [en-output (-> en-expression en/morph)]
        (log/info (str "es-to-en: " es-input " -> " en-output))
        en-output))))



