(ns menard.translate.es-en
  (:require [menard.english :as en]
            [menard.español :as es]
            [menard.generate :as g]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [menard.log :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            [dag_unify.diagnostics :as diag]))

(defn es-to-en [es-input]
  (-> es-input es/parse first
      ((fn [es-parse]
         (let [spec
               (-> {:sem (-> es-parse (u/get-in [:sem]))
                    :cat (-> es-parse (u/get-in [:cat]))
                    :infl (-> es-parse (u/get-in [:infl]))
                    :subcat (-> es-parse (u/get-in [:subcat]))}
                   
                   (u/unify {;; fix espanol parsing to generate these:
                             :sem {:subj {:existential? false} 
                                   :obj :none
                                   :mod []}
                             :aux? false}))]
           (log/info (str "english spec: " spec))
           (-> spec
               en/generate
               en/morph))))))


