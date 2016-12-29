(ns babel.espanol
  (:refer-clojure :exclude [get-in])
  (:require
   [dag_unify.core :refer (fail-path get-in unifyc)]
   [babel.generate :as generate]
   [babel.espanol.grammar :as grammar]
   [babel.espanol.morphology :as morph :refer [fo]]
   #?(:cljs [babel.logjs :as log])
   [babel.over :refer [over truncate]]
   [babel.parse :as parse]
   [clojure.repl :refer [doc]]
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :refer [deserialize dissoc-paths
                           fail? fail-path get-in serialize strip-refs
                           ;;temporary
                           copy]]))

(def small-model (promise))
(defn small [] (if (and (not (nil? small-model))
                        (realized? small-model))
                 @small-model
                 @(deliver small-model (grammar/small))))

(def medium-model (promise))
(defn medium [] (if (and (realized? medium-model)
                         (not (nil? medium-model))
                         (not (nil? @medium-model)))
                  @medium-model
                  @(deliver medium-model (grammar/medium))))

(defn analyze
  ([surface-form]
   (map
    (fn [each-analysis]
      (dissoc each-analysis :serialized))
    (analyze surface-form (:lexicon (medium)))))

  ([surface-form lexicon] ;; use user-provided lexicon
   (map
    (fn [each-analysis]
      (dissoc each-analysis :serialized))
    (morph/analyze surface-form (:lexicon (medium))))))

;; TODO: do morphological analysis
;; do find non-infinitives (e.g. find 'parler' given 'parle')
;; and then apply conjugated parts to lexeme
;; i.e. if input is 'parle', return
;; list of lexemes; for each, [:synsem :agr :person] will be
;; 1st, 2nd, or 3rd, and for all, number will be singular.
(defn lookup [lexeme]
  ((:lookup (medium)) lexeme))

(defn generate
  [spec & {:keys [max-total-depth model truncate-children]
           :or {max-total-depth generate/max-total-depth
                model (medium)
                truncate-children true}}]
  (log/debug (str "generating with spec: " (strip-refs spec) " with max-total-depth: " max-total-depth))
  (let [result (generate/generate spec model
                                  :max-total-depth max-total-depth
                                  :truncate-children truncate-children)]
    (if result
      (conj {:surface (fo result)}
            result))))

(defn preprocess [input]
  "arbitrary regexp replacements to convert Spanish orthography into a parsable whitespace-delimited expression"
  ;; for now, it's a noop. see english.cljc for a non-noop (defn preprocess).
  input)

(defn parse
  "parse a string in English into zero or more (hopefully more) phrase structure trees"

  ([input]
   (parse (preprocess input) (medium)))

  ([input model]
   (parse/parse (preprocess input) model)))


