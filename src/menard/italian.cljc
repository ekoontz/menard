(ns menard.italian
  (:require [dag_unify.core :as u :refer [unify]]
            [dag_unify.dissoc :refer [dissoc-in]]
            [menard.generate :as g]
            [menard.generate.lab :refer [morph morph-ps]]
            [menard.parse :as p]))
;; Italian grammar and lexicon.

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
  {"cane"  [{:cat :n :pred :dog :subcat {:1 {:cat :det}}}]
   "dorme" [{:cat :v :pred :sleeps :subcat {:1 {:cat :n}}}]   
   "gatto" [{:cat :n :pred :cat :subcat {:1 {:cat :det}}}]
   "il"    [{:cat :det}]
   "un"    [{:cat :det}]})
   
;; </lexicon>

;; <language-specific grammar rules>
(def grammar
  [{:rule "rule-1"
    :unify [head-rule head-last subcat-1]}])
;; </language-specific grammar rules>

;; <compilation functions>
;; These are used to convert human-friendly data structures
;; representing a lexicon and a grammar into machine-friendly data structures.
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

(def italian
  {:lexicon (process-lexicon lexicon)
   :grammar (process-grammar grammar)})

(defn generate [spec]
    (binding [g/grammar (:grammar italian)
              g/lexicon (:lexicon italian)
              g/morph-ps morph-ps]
      (g/generate spec)))

(defn parse [expression]
  (p/parse expression
           (merge italian
                  {:lookup (fn [word]
                             (get (:lexicon italian)
                                  word))})))

(defn demo []
  (println "generation:")
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate :top))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:cat :v}))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:cat :n}))))))
  (println "parsing:")
  (println "===")
  (count (take 10
               (repeatedly #(let [expression (morph (generate {:cat :top}))]
                              (println (->> (parse expression)
                                            (map morph-ps)
                                            (clojure.string/join ","))))))))
