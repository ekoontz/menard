(ns babylon.english
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.dissoc :refer [dissoc-in]]
            [babylon.generate :as g]
            [babylon.grammar :as grammar :refer [syntax-tree]]
            [babylon.parse :as p]
            [babylon.ug :refer [head-rule head-last subcat-1 process-grammar]]))

;; For generation and parsing English.
;; 
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
;; </compilation functions>

(def lexicon (-> "babylon/english/lexicon.edn"
                 io/resource
                 slurp
                 read-string
                 process-lexicon))

(def grammar (-> "babylon/english/grammar.edn"
                 io/resource
                 slurp
                 read-string
                 process-grammar))

(def morphology
  (concat
   [(-> "babylon/english/morphology/nouns.edn"
        clojure.java.io/resource
        slurp
        read-string)
    (-> "babylon/english/morphology/verbs.edn"
        clojure.java.io/resource
        slurp
        read-string)]))

(defn morph [structure]
  ;; TODO: flesh out:
  (grammar/default-morph-fn structure))

(defn generate [spec]
  (binding [g/grammar grammar
            g/lexicon lexicon
            g/morph-ps syntax-tree]
    (g/generate spec)))

(defn parse [expression]
  (binding [p/grammar grammar
            p/lexicon lexicon
            p/lookup-fn (fn [word]
                          (get lexicon word))]
    (p/parse expression
             {:grammar grammar
              :lexicon lexicon
              :lookup (fn [word]
                        (get lexicon word))})))

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
                                            (map syntax-tree)
                                            (string/join ", "))))))))

