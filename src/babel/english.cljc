(ns babel.english
  (:refer-clojure :exclude [get-in])
  (:require
   [dag_unify.core :refer (fail-path get-in unifyc)]
   [babel.generate :as generate]
   [babel.english.grammar :as grammar]
   [babel.english.morphology :as morph]
   [babel.over :refer [over truncate]]
   [babel.parse :as parse]
   [clojure.repl :refer [doc]]
   [clojure.string :as string]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :refer [deserialize dissoc-paths
                           fail? fail-path get-in serialize strip-refs]]))
(declare morph)

(defn generate
  ([spec model & {:keys [max-total-depth]
                  :or {max-total-depth generate/max-depth}}]
   (log/debug (str "generating with spec: " (strip-refs spec) " with max-total-depth: " max-total-depth))
   (let [result (generate/generate spec model)]
     (if (keyword? result)
       (throw (Exception. (str "please don't send me a keyword :( : this is what you sent me: " result)))
       (conj {:surface (morph result model)}
             result)))))

;; can't decide between 'morph' or 'fo' or something other better name.
(defn morph [expr model & {:keys [from-language show-notes]
                           :or {from-language nil
                                show-notes true}}]
  (morph/fo expr
            :from-language from-language :show-notes show-notes
            :lexicon (:lexicon model)))

(defn fo-ps [expr]
  (parse/fo-ps expr morph/fo))

(defn morph-ps [expr]
  (fo-ps expr))

(defn analyze
  ([surface-form lexicon] ;; use user-provided lexicon
   (morph/analyze surface-form lexicon)))

(defn preprocess [input]
  "arbitrary regexp replacements to convert English orthography into a parsable whitespace-delimited expression"
  ;; e.g.
  ;; (preprocess "the womens' hats and the cats' pyjamas")
  ;;  => "the women 's  hats and the cats 's pyjamas"
  ;;
  (string/replace (string/replace input #"(men)s'(\s*)" "$1 's $2") #"(\S)s'" "$1s 's"))

(defn parse
  "parse a string in English into zero or more (hopefully more) phrase structure trees"
  ([input model truncate?]
   (parse/parse (preprocess input) model :parse-with-truncate truncate?)))

(def tree-variants
  [
   {:head {:phrasal true}}

   {:comp {:phrasal true}}

   {:head {:phrasal true}
    :comp {:phrasal true}}

   {:head {:phrasal true}
    :comp {:phrasal true
           :head {:phrasal true}}}
   
   {:comp {:phrasal true
           :head {:phrasal true}}
    :head {:phrasal true}}

   ;; failsafe
   {:comp {:phrasal :top
           :head :top}}])
