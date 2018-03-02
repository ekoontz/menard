(ns babel.directory
  (:require [babel.english.grammar :as en]
            [babel.espanol.grammar :as es]
            [babel.francais.grammar :as fr]
            [babel.italiano.grammar :as it]
            [babel.latin :as la]))
(def models
  {:en (atom (delay (en/medium)))
   :es (atom (delay (es/small)))
   :fr (atom (delay (fr/medium)))
   :la (atom (delay (la/model)))
   :it (atom (delay (it/model)))})


