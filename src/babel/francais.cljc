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

(def small-model (promise))
(defn small [] (if (realized? small-model)
                 @small-model
                 @(deliver small-model (grammar/small))))

(def medium-model (promise))
(defn medium [] (if (realized? medium-model)
                  @medium-model
                  @(deliver medium-model (grammar/medium))))

(defn analyze
  ([surface-form]
   (analyze surface-form (:lexicon (medium))))
  ([surface-form lexicon]
   (morph/analyze surface-form lexicon)))

(defn generate
  ([]
   (let [result (generate/generate :top (medium))]
     (if result
       (conj {:surface (fo result)}
             result))))
  ([spec & {:keys [max-total-depth model truncate-children]
            :or {max-total-depth generate/max-total-depth
                 model (medium)
                 truncate-children true}}]
   (let [result (generate/generate spec (medium))]
     (if result
       (conj {:surface (fo result)}
             result)))))

(defn parse
  "parse a string in French into zero or more (hopefully more) phrase structure trees"
  
  ([input]
   (parse input (medium)))

  ([input model]
   (parse/parse input model)))
