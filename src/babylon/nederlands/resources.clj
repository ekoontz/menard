(ns babylon.nederlands.resources
  (:require
   [babylon.grammar :as grammar]
   [babylon.lexiconfn :as l]
   [clojure.java.io :as io :refer [resource]]))

(def finite-tenses
  [;; "hij werkt"
   {:variant :present-simple
    :abbreviation :simple-present
    :infl :present
    :modal false
    :sem {:tense :present
          :aspect :simple}}])

(def lexical-rules
  [(l/read-and-eval "babylon/nederlands/lexicon/rules/rules-0.edn")
   (l/read-and-eval "babylon/nederlands/lexicon/rules/rules-1.edn")
   (l/read-and-eval "babylon/nederlands/lexicon/rules/rules-2.edn")])

(defn compile-lexicon-source [source-filename]
  (-> source-filename
      l/read-and-eval
      l/add-exceptions-to-lexicon
      (l/apply-rules-in-order (nth lexical-rules 0) :0)
      (l/apply-rules-in-order (nth lexical-rules 1) :1)
      (l/apply-rules-in-order (nth lexical-rules 2) :2)))

(def lexicon
  (merge-with concat
              (compile-lexicon-source "babylon/nederlands/lexicon/adjectives.edn")
              (compile-lexicon-source "babylon/nederlands/lexicon/misc.edn")
              (compile-lexicon-source "babylon/nederlands/lexicon/nouns.edn")
              (compile-lexicon-source "babylon/nederlands/lexicon/propernouns.edn")
              (compile-lexicon-source "babylon/nederlands/lexicon/verbs.edn")))

(def expressions
  (-> "babylon/nederlands/expressions.edn"
      resource slurp read-string eval))

(defn write-compiled-lexicon []
  (l/write-compiled-lexicon lexicon
                            "src/babylon/nederlands/lexicon/compiled.edn"))

(declare grammar)

(defn write-compiled-grammar []
  (grammar/write-compiled-grammar grammar
                                  "src/babylon/nederlands/grammar/compiled.edn"))

(def grammar
  (-> "babylon/nederlands/grammar.edn"
      resource
      slurp
      read-string
      grammar/process))


