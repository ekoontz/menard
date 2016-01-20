(ns ^{:doc "Parsing Testing Code"}
    babel.test.parse
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.forest :as forest]
            [babel.francais.grammar :refer [small medium]]
            [babel.francais.lexicon :refer [lexicon]]
            [babel.francais.morphology :refer [analyze fo replace-patterns]]
            [babel.francais.workbook :as workbook]
            [babel.over :as over]
            [babel.parse :refer [create-bigram-map create-ngram-map create-trigram-map
                                 create-unigram-map create-xgram-map parse tokenizer toks toks2]]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [fail-path fail? get-in strip-refs unifyc]]))

;; TODO: (lookup) and (over) convenience functions are duplicated in
;; babel.workbook.francais: factor out to babel.francais.
;; TODO: do morphological analysis
;; do find non-infinitives (e.g. find 'parler' given 'parle')
;; and then apply conjugated parts to lexeme
;; i.e. if input is 'parle', return
;; list of lexemes; for each, [:synsem :agr :person] will be
;; 1st, 2nd, or 3rd, and for all, number will be singular.
(defn lookup [lexeme]
  (get (:lexicon medium) lexeme))

(defn over
  ([arg1]
   (over/over (vals (:grammar-map medium)) (lookup arg1)))
  ([grammar arg1]
   (over/over grammar (lookup arg1)))
  ([grammar arg1 arg2]
   (cond (string? arg1)
         (over grammar (lookup arg1)
               arg2)

         (string? arg2)
         (over grammar arg1 (lookup arg2))

         true
         (over/over grammar arg1 arg2))))

(deftest split
  (is (= 2 (count (string/split "je suis" tokenizer)))))

(deftest toks-test1
  (is (= 2 (count (toks "je suis" lexicon lookup)))))

(deftest toks-test2
  (is (= 2 (count (nth (toks "je suis" lexicon lookup) 0)))))

(deftest toks-test3
  (is (= 2 (count (nth (toks "je suis" lexicon lookup) 1)))))
