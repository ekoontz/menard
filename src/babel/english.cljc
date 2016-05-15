(ns babel.english
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.engine :as engine]
   [babel.forest :as forest]
   [babel.english.grammar :refer [medium small-plus-vp-pronoun small-plus-plus-np]]
   [babel.english.morphology :as morph :refer [fo]]
   [babel.parse :as parse]
   [clojure.string :as string]
   [dag_unify.core :refer [fail? get-in strip-refs]]))

(def lexicon (:lexicon medium))
(def grammar (:grammar-map medium))

(defn analyze
  ([surface-form]
   (analyze surface-form lexicon)) ;; use (:lexicon medium)

  ([surface-form lexicon] ;; use user-provided lexicon
   (morph/analyze surface-form lexicon)))

;; TODO: do morphological analysis
;; do find non-infinitives (e.g. find 'parler' given 'parle')
;; and then apply conjugated parts to lexeme
;; i.e. if input is 'parle', return
;; list of lexemes; for each, [:synsem :agr :person] will be
;; 1st, 2nd, or 3rd, and for all, number will be singular.
(defn lookup [lexeme]
  ((:lookup medium) lexeme))

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
  (forest/lightning-bolt (:grammar medium) (:lexicon medium) spec 0 (:index medium) nil (:morph medium)))

(defn parse
  "parse a string in English into zero or more (hopefully more) phrase structure trees"

  ([input]
   (parse input medium))

  ([input model]
   (parse/parse input model)))

