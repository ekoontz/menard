(ns babel.generate.toy
  (:require [dag_unify.core :as u :refer [unify]]
            [dag_unify.dissoc :refer [dissoc-in]]
            [babel.generate :as g]
            [babel.generate.lab :refer [morph morph-ps]]))

;; a toy english grammar and lexicon.

;; <lexicon>
(def lexicon
  {"a"      [{:cat :det}]
   "cat"    [{:cat :n :pred :cat :subcat {:1 {:cat :det}}}]
   "dog"    [{:cat :n :pred :dog :subcat {:1 {:cat :det}}}]
   "sleeps" [{:cat :v :pred :sleeps :subcat {:1 {:cat :n}}}]   
   "the"    [{:cat :det}]})

;; </lexicon>

;; <language-specific grammar rules>
(def grammar
  [{:rule "rule-1"
    :unify [head-rule head-last subcat-1]}])
;; </language-specific grammar rules>

;; <universal grammar rules>
(def head-rule
  (let [comp-cat (atom :top)
        head-pred (atom :top)
        head-cat (atom :top)]
    {:cat head-cat
     :pred head-pred
     :head {:cat head-cat
            :pred head-pred}}))

(def head-last
  (let [head (atom :top)
        comp (atom :top)]
    {:head head
     :1 comp
     :comp comp
     :2 head}))

(def subcat-1
  (let [complement (atom {:subcat []})]
    {:head {:subcat {:1 complement}}
     :subcat []
     :comp complement}))
;; </universal grammar rules>

(declare process-lexicon)
(declare process-grammar)

(def toy-english
  {:lexicon (process-lexicon lexicon)
   :grammar (process-grammar grammar)})

(defn process-lexicon [lexicon]
  (into {} (for [[surface lexemes-for-surface]
                 lexicon]
             [surface (map (fn [lexeme]
                             (merge lexeme {:phrasal false
                                            :surface surface}))
                           lexemes-for-surface)])))

(defn process-grammar [grammar]
  (->>
   grammar
   (map #(unify head-rule %))
   (map #(apply unify (cons (dissoc-in % [:unify]) (:unify %))))))

(defn generate [spec]
    (binding [g/grammar (:grammar toy-english)
              g/lexicon (:lexicon toy-english)
              g/morph-ps morph-ps]
      (g/generate spec)))

(defn demo []
  (count (take 10 (repeatedly #(println (morph (generate :top))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:cat :v}))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:cat :n})))))))
