(ns babel.directory
  (:require [babel.english.grammar :as en]
            [babel.espanol.grammar :as es]
            [babel.francais.grammar :as fr]
            [babel.italiano.grammar :as it]
            [babel.latin :as la]))
(def models
  {:en (delay (en/medium))
   :es (delay (es/small))
   :fr (delay (fr/medium))
   :la (delay (la/model))
   :it (delay (it/medium))})
