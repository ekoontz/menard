(ns menard.test.español.noun-morphology
  (:require [menard.español :as es]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]))

;; These are for convenience so you don't have
;; to reload the language-model files every time you make a change:
;; you can just reload this (i.e. menard.test.español.noun-morphology)
;; file and it will reload the changed language-model files.
(load "../../../../src/menard/español")

(def morphology (:morphology @es/model))

(defn common-noun
  "A common (neither pronoun nor proper) noun, as it looks once the
   lexicon and morphology have been applied."
  [canonical gender number]
  {:cat :noun
   :pronoun? false
   :propernoun? false
   :canonical canonical
   :agr {:gender gender
         :number number}})

(defn inflect
  "The surface form of the common noun _canonical_ in the given number."
  [canonical gender number]
  (morph-leaf (common-noun canonical gender number) morphology))

(defn lexicon-for
  "A lexicon containing only _canonical_, in the shape that
   menard.model/load-lexicon produces."
  [canonical & [gender]]
  {canonical [{:cat :noun
               :pronoun? false
               :propernoun? false
               :canonical canonical
               :agr {:gender (or gender :masc)}}]})

(defn analyses
  "The analyses of the surface form _surface_, looked up in _lexicon_."
  [surface lexicon]
  (l/matching-lexemes surface lexicon morphology))

(defn analyzed-canonicals
  "The canonical forms recovered from the surface form _surface_."
  [surface lexicon]
  (->> (analyses surface lexicon)
       (map #(u/get-in % [:canonical]))
       distinct
       vec))

(deftest noun-plural-generation
  ;; regular: vowel-final nouns add -s.
  (is (= "mesas" (inflect "mesa" :fem :plur)))
  ;; consonant-final nouns (d, l, n, r) add -es.
  (is (= "ciudades" (inflect "ciudad" :fem :plur)))
  ;; -z becomes -ces.
  (is (= "lápices" (inflect "lápiz" :masc :plur)))
  (is (= "veces" (inflect "vez" :fem :plur)))
  ;; nouns ending in a stressed -í or -ú add -es, keeping the accent.
  (is (= "rubíes" (inflect "rubí" :masc :plur)))
  (is (= "tabúes" (inflect "tabú" :masc :plur))))

(deftest noun-singular-generation
  (is (= "mesa" (inflect "mesa" :fem :sing)))
  (is (= "lápiz" (inflect "lápiz" :masc :sing)))
  (is (= "rubí" (inflect "rubí" :masc :sing))))

(deftest noun-plural-parsing
  (is (= ["mesa"] (analyzed-canonicals "mesas" (lexicon-for "mesa" :fem))))
  (is (= ["ciudad"] (analyzed-canonicals "ciudades" (lexicon-for "ciudad" :fem))))
  ;; -ces parses back to -z:
  (is (= ["lápiz"] (analyzed-canonicals "lápices" (lexicon-for "lápiz"))))
  (is (= ["vez"] (analyzed-canonicals "veces" (lexicon-for "vez" :fem))))
  ;; -íes/-úes parse back to -í/-ú:
  (is (= ["rubí"] (analyzed-canonicals "rubíes" (lexicon-for "rubí"))))
  (is (= ["tabú"] (analyzed-canonicals "tabúes" (lexicon-for "tabú"))))
  ;; the analysis agrees in number, and keeps the noun's gender:
  (is (= {:gender :fem :number :plur}
         (-> (analyses "veces" (lexicon-for "vez" :fem))
             first
             (u/get-in [:agr]))))
  (is (= {:gender :masc :number :plur}
         (-> (analyses "lápices" (lexicon-for "lápiz"))
             first
             (u/get-in [:agr])))))

(deftest noun-singular-parsing
  (is (= ["mesa"] (analyzed-canonicals "mesa" (lexicon-for "mesa" :fem))))
  (is (= ["lápiz"] (analyzed-canonicals "lápiz" (lexicon-for "lápiz"))))
  (is (= ["rubí"] (analyzed-canonicals "rubí" (lexicon-for "rubí"))))
  (is (= {:gender :masc :number :sing}
         (-> (analyses "lápiz" (lexicon-for "lápiz"))
             first
             (u/get-in [:agr])))))

;; The following tests use the real lexicon
;; (i.e. resources/español/lexicon/nouns.edn), which contains
;; a noun for each of the endings that the rules above handle:

(def real-lexicon (:lexicon @es/model))

(defn analyzed-agrs
  "The distinct number/gender agreements of the analyses of _surface_."
  [surface]
  (->> (es/analyze surface)
       (map #(u/get-in % [:agr]))
       ;; the lexicon's :agr values are unresolved; l/pprint resolves them:
       (map l/pprint)
       (map #(select-keys % [:gender :number]))
       distinct
       vec))

(deftest real-lexicon-analyze
  (is (= ["lápiz"] (analyzed-canonicals "lápices" real-lexicon)))
  (is (= ["vez"] (analyzed-canonicals "veces" real-lexicon)))
  (is (= ["rubí"] (analyzed-canonicals "rubíes" real-lexicon)))
  (is (= ["tabú"] (analyzed-canonicals "tabúes" real-lexicon)))
  (is (= [{:gender :masc :number :plur}] (analyzed-agrs "lápices")))
  (is (= [{:gender :fem :number :plur}] (analyzed-agrs "veces")))
  (is (= [{:gender :masc :number :plur}] (analyzed-agrs "rubíes")))
  (is (= [{:gender :masc :number :plur}] (analyzed-agrs "tabúes")))
  (is (= [{:gender :masc :number :sing}] (analyzed-agrs "lápiz")))
  (is (= [{:gender :fem :number :sing}] (analyzed-agrs "vez")))
  (is (= [{:gender :masc :number :sing}] (analyzed-agrs "rubí"))))

(deftest real-lexicon-parse
  (is (= "[np .el +lápiz]" (->> "el lápiz" es/parse (map es/syntax-tree) first)))
  (is (= "[np .los +lápices]" (->> "los lápices" es/parse (map es/syntax-tree) first)))
  (is (= "[np .las +veces]" (->> "las veces" es/parse (map es/syntax-tree) first)))
  (is (= "[np .el +rubí]" (->> "el rubí" es/parse (map es/syntax-tree) first)))
  (is (= "[np .los +rubíes]" (->> "los rubíes" es/parse (map es/syntax-tree) first)))
  (is (= "[np .los +tabúes]" (->> "los tabúes" es/parse (map es/syntax-tree) first))))

(deftest no-duplicate-analyses
  ;; -í and -ú share their plural rule with d, l, n and r, so a noun like
  ;; "rubí" does not also get a second, identical analysis (and therefore a
  ;; second, identical parse) from a rule of its own:
  (is (every? #(= 1 (count (es/parse %)))
              ["los gatos"
               "los lápices"
               "las veces"
               "los rubíes"
               "los tabúes"])))
