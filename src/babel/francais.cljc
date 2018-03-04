(ns babel.francais
  (:require
   [babel.francais.grammar :as grammar]
   [babel.francais.morphology :as morph :refer [fo]]
   [babel.generate :as generate]
   [babel.parse :as parse]
   [clojure.repl :refer [doc]]
   [clojure.string :as string]))

;; can't decide between 'morph' or 'fo' or something other better name.
(defn morph [expr & {:keys [from-language show-notes]
                     :or {from-language nil
                          show-notes false}}]
  (fo expr :from-language from-language :show-notes show-notes))

(defn analyze
  ([surface-form]
   (analyze surface-form (:lexicon (grammar/model))))
  ([surface-form lexicon]
   (morph/analyze surface-form lexicon)))
