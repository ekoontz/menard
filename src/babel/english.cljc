(ns babel.english
  (:refer-clojure :exclude [get-in])
  (:require
   [dag_unify.core :refer (fail-path get-in unifyc)]
   [babel.generate :as generate]
   [babel.english.grammar :as grammar]
   [babel.english.morphology :as morph :refer [fo]]
   [babel.over :refer [over truncate]]
   [babel.parse :as parse]
   [clojure.repl :refer [doc]]
   [clojure.string :as string]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :refer [deserialize dissoc-paths
                           fail? fail-path get-in serialize strip-refs]]))

(def small-model (promise))
(defn small [] (if (realized? small-model)
                 @small-model
                 @(deliver small-model (grammar/small))))

(def medium-model (promise))
(defn medium []
  (if (realized? medium-model)
    @medium-model
    @(deliver medium-model (grammar/medium))))

;; can't decide between 'morph' or 'fo' or something other better name.
(defn morph [expr & {:keys [from-language show-notes]
                     :or {from-language nil
                          show-notes false}}]
  (fo expr :from-language from-language :show-notes show-notes))

(defn fo-ps [expr]
  (parse/fo-ps expr fo))

(defn analyze
  ([surface-form]
   (analyze surface-form (:lexicon (medium))))
  ([surface-form lexicon] ;; use user-provided lexicon
   (morph/analyze surface-form lexicon)))

(defn generate
  [spec & {:keys [max-total-depth model truncate-children]
           :or {max-total-depth generate/max-total-depth
                truncate-children true
                model (medium)}}]
  (log/debug (str "generating with spec: " (strip-refs spec) " with max-total-depth: " max-total-depth))
  (let [result (generate/generate spec model
                                  :max-total-depth max-total-depth
                                  :truncate-children truncate-children)]
    (if result
      (conj {:surface (fo result)}
            result))))

(defn preprocess [input]
  "arbitrary regexp replacements to convert English orthography into a parsable whitespace-delimited expression"
  ;; e.g.
  ;; (preprocess "the womens' hats and the cats' pyjamas")
  ;;  => "the women 's  hats and the cats 's pyjamas"
  ;;
  (string/replace (string/replace input #"(men)s'(\s*)" "$1 's $2") #"(\S)s'" "$1s 's"))

(defn parse
  "parse a string in English into zero or more (hopefully more) phrase structure trees"

  ([input]
   (parse (preprocess input) (medium)))

  ([input model]
   (parse/parse (preprocess input) model)))

(defn sentences [ & [count spec model]]
  (let [count (or (Integer. count) 100)
        model (or model (medium))
        spec (or (and spec
                      (unifyc spec {:modified false}))
                 {:modified false
                  :synsem {:cat :verb}})]
    (println (str "count: " count))
    (println (str "spec:" spec))
    (doall (pmap
            (fn [num]
              (let [expr (generate spec
                                   :model model)
                    fo (fo expr :show-notes false)]
                (let [to-print
                      (cond
                        (empty? fo) (str "(failed)")
                        true
                        (str (string/capitalize (nth fo 0))
                             (string/join "" (rest (vec fo)))
                             "."))]
                  (println (str (+ 1 num) ". " to-print)))))
            (range 0 count)))))
