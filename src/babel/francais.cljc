(ns babel.francais
  (:require
   [babel.engine :as engine]
   [babel.francais.grammar :refer [medium small]]
   [babel.francais.morphology :as morph :refer [fo]]
   [babel.generate :as generate]
   [babel.parse :as parse]
   [clojure.string :as string]))

(defn analyze
  ([surface-form]
   (analyze surface-form (:lexicon (medium))))
  ([surface-form lexicon]
   (morph/analyze surface-form lexicon)))

(defn generate
  ([]
   (let [result (engine/generate :top (medium))]
     (if result
       (conj {:surface (fo result)}
             result))))
  ([spec]
   (let [result (engine/generate spec (medium))]
     (if result
       (conj {:surface (fo result)}
             result))))
  ([spec model]
   (let [result (engine/generate spec model)]
     (if result
       (conj {:surface (fo result)}
             result)))))

(defn parse
  "parse a string in Italian into zero or more (hopefully more) phrase structure trees"

  ([input]
   (parse input (medium)))

  ([input model]
   (parse/parse input model)))
