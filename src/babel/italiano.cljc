(ns babel.italiano
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.engine :as engine]
   [babel.italiano.grammar :refer [medium]]
   [babel.italiano.morphology :as morph :refer [fo]]
   [babel.generate :as generate :refer [try-hard-to]]
   [babel.parse :as parse]
   [clojure.repl :refer [doc]]
   [clojure.string :as string]
   [dag_unify.core :refer (get-in strip-refs)]))

(def lexicon (:lexicon medium))
(def infinitives
  (filter (fn [k] 
            (let [vals (get lexicon k)]
              (some (fn [val]
                      (and (= (get-in val [:synsem :cat]) :verb)
                           (= (get-in val [:synsem :infl]) :top)))
                    vals)))
          (keys lexicon)))

(def nominative-pronouns
  (filter (fn [k] 
            (let [vals (get lexicon k)]
              (some (fn [val]
                      (and (= (get-in val [:synsem :cat]) :noun)
                           (= (get-in val [:synsem :pronoun]) true)
                           (= (get-in val [:synsem :case]) :nom)))
                    vals)))
          (keys lexicon)))

(defn analyze
  ([surface-form]
   (analyze surface-form (:lexicon medium)))
  ([surface-form lexicon]
   (morph/analyze surface-form lexicon)))

(defn generate
  ([]
   (let [result (engine/generate :top medium)]
     (if result
       (conj {:surface (fo result)}
             result))))
  ([spec]
   (let [result (engine/generate spec medium)]
     (if result
       (conj {:surface (fo result)}
             result))))
  ([spec model]
   (let [result (engine/generate spec model)]
     (if result
       (conj {:surface (fo result)}
             result)))))

(defn lightning-bolt [spec]
  (generate/lightning-bolt (:grammar medium) (:lexicon medium) spec 0 (:index medium) nil (:morph medium)))

(def tokenizer #"[ ']")

(defn analyze-tokens
  "given a string, generate a list of tokenization hypotheses."
  [string]
  (map #(string/split % tokenizer)
       (if true
         (morph/replace-over [string])
         [string])))

(defn parse
  "parse a string in Italian into zero or more (hopefully more) phrase structure trees"

  ([input]
   (parse input medium))

  ([input model]
   (cond (string? input)
         (map (fn [tokenization]
                {:tokens tokenization
                 :parses (parse tokenization model)})
              (analyze-tokens input))

         (or (seq? input) (vector? input))
         (parse/parse input model)
        
         true
         (str "don't know how to parse input: " (type input)))))
