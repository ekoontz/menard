(ns babel.italiano
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.engine :as engine]
   [babel.italiano.grammar :refer [small medium np-grammar]]
   [babel.italiano.morphology :as morph]
   [babel.generate :as generate :refer [try-hard-to]]
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

(def lexicon (:lexicon @medium))
(def infinitives
  (filter (fn [k] 
            (let [vals (get lexicon k)]
              (some (fn [val]
                      (and (= (get-in val [:synsem :cat]) :verb)
                           (= (get-in val [:synsem :infl]) :top)))
                    vals)))
          (keys lexicon)))

(def articles
  (filter (fn [k] 
            (let [vals (get lexicon k)]
              (some (fn [val]
                      (= (get-in val [:synsem :cat]) :det))
                    vals)))
          (keys lexicon)))

(def nouns
  (filter (fn [k] 
            (let [vals (get lexicon k)]
              (some (fn [val]
                      (= (get-in val [:synsem :cat]) :noun))
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

(def propernouns
  (filter (fn [k] 
            (let [vals (get lexicon k)]
              (some (fn [val]
                      (and (= (get-in val [:synsem :cat]) :noun)
                           (= (get-in val [:synsem :propernoun]) true)))
                    vals)))
          (keys lexicon)))

(defn analyze
  ([surface-form]
   (analyze surface-form (:lexicon @medium)))
  ([surface-form lexicon]
   (morph/analyze surface-form lexicon)))

(defn generate
  [spec & {:keys [do-enrich max-total-depth model truncate-children]
           :or {do-enrich true
                max-total-depth generate/max-total-depth
                model medium
                truncate-children true}}]
  (log/debug (str "generating with spec: " (strip-refs spec) " with max-total-depth: " max-total-depth))
  (let [result (engine/generate spec model
                                :do-enrich do-enrich
                                :max-total-depth max-total-depth
                                :truncate-children truncate-children)]
    (if result
      (conj {:surface (fo result)}
            result))))

(defn lightning-bolts [spec]
  (let [medium @medium]
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
     (parse input medium)))

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
