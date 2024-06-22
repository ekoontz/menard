(ns menard.test.translate.en-es
  (:require [dag_unify.core :as u]
            [menard.english :as en]
            [menard.español :as es]
            [clojure.test :refer [deftest is]]))

(def es-input "yo quiero")
(def es-parse (-> es-input es/parse first))

(defn es-to-en [es-input]
  (-> es-input es/parse first
      ((fn [es-parse]
         (-> {:sem (-> es-parse (u/get-in [:sem]))
              :cat (-> es-parse (u/get-in [:cat]))
              :infl (-> es-parse (u/get-in [:infl]))
              :subcat (-> es-parse (u/get-in [:subcat]))}

             (u/unify {;; fix espanol parsing to generate these:
                       :sem {:subj {:existential? false} 
                             :obj :none
                             :mod []}
                       :aux? false})
             en/generate
             en/morph)))))

(deftest transfer-1
  (-> "yo quiero" es-to-en (= "I want") is))

