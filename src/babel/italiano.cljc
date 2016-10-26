(ns babel.italiano
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.generate :as generate]
   [babel.italiano.grammar :as grammar]
   [babel.italiano.lexicon :as lex]
   [babel.italiano.morphology :as morph :refer [fo]]
   [babel.generate :as generate]
   [babel.over :as over]
   [babel.parse :as parse]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [clojure.repl :refer [doc]]
   [clojure.string :as string]
   [dag_unify.core :refer [fail-path-between get-in strip-refs unifyc]]))

(def small-model (promise))
(defn small [] (if (realized? small-model)
                 @small-model
                 @(deliver small-model (grammar/small))))

(def medium-model (promise))
(defn medium []
  (if (realized? medium-model)
    @medium-model
    @(deliver medium-model (grammar/medium))))

(def np-grammar-model (promise))
(defn np-grammar []
  (if (realized? np-grammar-model)
    @np-grammar-model
    @(deliver np-grammar-model (grammar/np-grammar))))

;; can't decide between 'morph' or 'fo' or something other better name.
(defn morph [expr & {:keys [from-language show-notes]
                     :or {from-language nil
                          show-notes true}}]
  ;; modeled after babel.english/morph:
  ;; most arguments are simply discarded for italian.
  (fo expr))

(defn fo-ps [expr]
  (parse/fo-ps expr fo))

(defn analyze
  "analyze a word: as opposed to parsing which is multi-word."
  ;; TODO: should take a language model, not a lexicon
  ([surface-form]
   (analyze surface-form (:lexicon (medium))))
  ([surface-form lexicon]
   (morph/analyze surface-form lexicon)))

(defn generate
  [spec & {:keys [model do-enrich max-total-depth truncate]
           :or {do-enrich true
                max-total-depth generate/max-total-depth
                model (medium)
                truncate true}}]
  (log/debug (str "generating with spec: " (strip-refs spec) " with max-total-depth: " max-total-depth))
  (let [result (generate/generate spec model
                                  :do-enrich do-enrich
                                  :max-total-depth max-total-depth
                                  :truncate-children truncate)]
    (if result
      (conj {:surface (fo result)}
            result))))

(defn lightning-bolts [spec]
  (let [medium medium]
    (generate/lightning-bolts (:grammar medium) (:lexicon medium) spec 0 (:index medium) nil (:morph medium))))

(def tokenizer #"[ '\n,’».]")

(defn tokenize [input]
  (string/split input tokenizer))

(defn analyze-tokens
  "given a string, generate a list of tokenization hypotheses."
  [string]
  (map #(string/split % tokenizer)
       (morph/replace-over [string])))

(defn over
  "given a parent and 2 children, try to arrange them with the first child on the left and the second child on the right."
  [parent child1 child2]
  (over/over parent child1 child2))

(defn preprocess [input]
  "arbitrary regexp replacements to convert Italian orthography into a parsable whitespace-delimited expression"
  (let [processed
        (string/join
         " "
         (map string/lower-case
              (->
               input
               (string/replace #","   "")
               (string/replace #"\."   "")
               (string/replace #"\s+" " ")
               (string/split #" "))))]
    (log/debug (str "preprocess: " input " -> " processed))
    processed))

(defn parse
  "parse a string in Italian into zero or more (hopefully more) phrase structure trees"

  ([input]
   (let [input (preprocess input)]
     (parse input (medium))))

  ([input model]
   (let [model (if (future? model) @model model)
         input (preprocess input)]
     (cond (string? input)
           (map (fn [tokenization]
                  {:tokens tokenization
                   :input input
                   :parses (parse tokenization model input)})
                (analyze-tokens (string/trim input)))

           (or (seq? input) (vector? input))
           (parse/parse input model)
        
           true
           (str "don't know how to parse input: " (type input)))))

  ([input model original-input]
   (let [input (if (string? input)
                 (preprocess input)
                 input)]
     (cond (string? input)
           (map (fn [tokenization]
                  {:tokens tokenization
                   :parses (parse tokenization model input)})
                (analyze-tokens (string/trim input)))

           (or (seq? input) (vector? input))
           (parse/parse input model original-input)
        
           true
           (str "don't know how to parse input: " (type input))))))
