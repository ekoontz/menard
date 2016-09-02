(ns babel.italiano
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.engine :as engine]
   [babel.italiano.grammar :as grammar :refer [small medium]]
   [babel.italiano.lexicon :as lex]
   [babel.italiano.morphology :as morph :refer [fo-ps1]]
   [babel.generate :as generate]
   [babel.over :as over]
   [babel.parse :as parse]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [clojure.repl :refer [doc]]
   [clojure.string :as string]
   [dag_unify.core :refer [fail-path-between get-in strip-refs unifyc]]))

(defn fo [expression]
  (morph/fo expression))

(defn fo-ps [expression]
  (morph/fo-ps1 expression))

(defn morph-ps [tree]
  "print a concise one-line representation of the tree"
  ((:morph-ps medium) tree))

(defn analyze
  ([surface-form]
   (analyze surface-form (:lexicon (medium))))
  ([surface-form lexicon]
   (morph/analyze surface-form lexicon)))

(defn generate
  [spec & {:keys [do-enrich max-total-depth model truncate]
           :or {do-enrich true
                max-total-depth generate/max-total-depth
                model (medium)
                truncate true}}]
  (log/debug (str "generating with spec: " (strip-refs spec) " with max-total-depth: " max-total-depth))
  (let [result (engine/generate spec model
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
