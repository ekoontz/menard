(ns babel.english
  (:refer-clojure :exclude [get-in])
  (:require
   [dag_unify.core :refer (fail-path get-in unifyc)]
   [babel.engine :as engine]
   [babel.generate :as generate :refer [try-hard-to]]
   [babel.english.grammar :refer [medium small-plus-vp-pronoun small-plus-plus-np]]
   [babel.english.morphology :as morph :refer [fo]]
   #?(:cljs [babel.logjs :as log])
   [babel.over :refer [over]]
   [babel.parse :as parse]
   [clojure.repl :refer [doc]]
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :refer [fail? fail-path get-in strip-refs]]))

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
  (generate/lightning-bolt (:grammar medium) (:lexicon medium) spec 0 (:index medium) nil (:morph medium) 0))

;; e.g.
;; babel.english> (preprocess "the womens' hats and the cats' pyjamas")
;; "the women 's  hats and the cats 's pyjamas"
;;
(defn preprocess [input]
  (string/replace (string/replace input #"(men)s'(\s*)" "$1 's $2") #"(\S)s'" "$1s 's"))

(defn parse
  "parse a string in English into zero or more (hopefully more) phrase structure trees"

  ([input]
   (parse (preprocess input) medium))

  ([input model]
   (parse/parse (preprocess input) model)))
