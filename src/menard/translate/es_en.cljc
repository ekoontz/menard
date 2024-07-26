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
  (log/info (str "es-parse sem: " (l/pprint (u/get-in es-parse [:sem]))))
  (unify {:sem {:mod []}}
         {:agr (-> es-parse (u/get-in [:agr]))
          :sem (-> es-parse (u/get-in [:sem]))
          :cat (-> es-parse (u/get-in [:cat]))
          :subcat (-> es-parse (u/get-in [:subcat]))}))

(defn es-to-en [es-input]
  (log/info (str "es-to-en: es-input: " es-input))
  (let [es-parse (-> es-input es/parse first)
        english-spec (es-parse-to-en-spec es-parse)]
    (log/info (str "es-parse sem: " (l/pprint (u/get-in es-parse [:sem]))))
    (log/info (str "english spec: " (l/pprint english-spec)))
    (-> english-spec
        en/generate
        en/morph)))

