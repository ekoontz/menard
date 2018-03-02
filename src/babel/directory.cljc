(ns babel.directory
  (:require [babel.english.grammar :as en]
            [babel.espanol.grammar :as es]
            [babel.francais.grammar :as fr]
            [babel.italiano.grammar :as it]
            [babel.latin :as la]))
(def models
  {:en (atom (delay (en/model)))
   :es (atom (delay (es/small)))
   :fr (atom (delay (fr/medium)))
   :la (atom (delay (la/model)))
   :it (atom (delay (it/model)))})

;; how to reload a model (e.g. english):
;; (swap! (:en models) (fn [old-model] (delay (en/model))))
