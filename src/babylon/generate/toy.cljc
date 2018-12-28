(ns babylon.generate.toy
  (:require [dag_unify.core :as u :refer [unify]]
            [dag_unify.dissoc :refer [dissoc-in]]
            [babylon.generate :as g]
            [babylon.generate.lab :refer [morph morph-ps]]))
;; a toy english grammar and lexicon.

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

;; <compilation functions>
;; These are used to convert a human-friendly data structures
;; to machine-friendly data structures.
(defn process-lexicon [lexicon]
  (let [has-det {:cat :det}
        subcat-empty {:subcat []}]
    (into {} (for [[surface lexemes-for-surface]
                   lexicon]
               [surface
                (->> lexemes-for-surface
                     (map (fn [lexeme]
                            (merge lexeme {:phrasal false
                                           :surface surface})))
                     (mapcat (fn [lexeme]
                              (let [result (unify lexeme has-det)]
                                (cond (not (= :fail result))
                                      [(unify lexeme subcat-empty)]
                                      true
                                      [lexeme])))))]))))

(defn process-grammar [grammar]
  (->>
   grammar
   (map #(unify head-rule %))
   (map #(apply unify (cons (dissoc-in % [:unify]) (:unify %))))))
;; </compilation functions>

(def toy-english
  {:lexicon (process-lexicon lexicon)
   :grammar (process-grammar grammar)})

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
