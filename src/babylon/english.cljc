(ns babylon.english
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.dissoc :refer [dissoc-in]]
            [babylon.lexiconfn :as l]
            [babylon.generate :as g]
            [babylon.grammar :as grammar :refer [syntax-tree]]
            [babylon.parse :as p]
            [babylon.ug :as ug]))
;;
;; For generation and parsing of English.
;; 
(def lexical-rules
  (-> "babylon/english/lexical-compile-rules.edn"
      io/resource
      slurp
      read-string))

(def lexicon
  (-> "babylon/english/lexicon.edn"
      io/resource
      slurp
      read-string
      ((fn [lexicon]
         (map #(eval %) lexicon)))
      (l/process lexical-rules)))

(def grammar
  (-> "babylon/english/grammar.edn"
      io/resource
      slurp
      read-string
      ug/process))

(def morphology
  (concat
   (-> "babylon/english/morphology/nouns.edn"
       clojure.java.io/resource
       slurp
       read-string)
   (-> "babylon/english/morphology/verbs.edn"
       clojure.java.io/resource
       slurp
       read-string)))

(defn morph-leaf [structure]
  ;; TODO: flesh out:
  (log/debug (str "morphology of:" structure))
  (let [{u :u g :g} (nth morphology 1)]
    (let [unified (unify u structure)]
      (when (not (= :fail unified))
        (log/info (str "G: " g))
        (log/info (str "R: " (clojure.string/replace
                              (u/get-in structure [:canonical])
                              (first g) (second g))))))    
    (cond
      (u/get-in structure [:canonical])
      (u/get-in structure [:canonical])
      true
      "_")))

(defn morph [structure]
  (binding [grammar/morph-leaf morph-leaf]
    (grammar/default-morph-fn structure)))

(defn generate [spec]
  (binding [g/grammar grammar
            g/lexicon lexicon
            g/morph-ps syntax-tree]
    (g/generate spec)))

(defn parse [expression]
  (binding [p/grammar grammar
            p/lookup-fn (fn [word]
                          (get lexicon word))]
    (p/parse expression)))

(defn demo []
  (println "Generation:")
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate :top))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:cat :verb}))))))
  (println "===")
  (count (take 10 (repeatedly #(println (morph (generate {:cat :noun}))))))
  (println "Parsing:")
  (println "===")
  (count (take 10
               (repeatedly #(let [expression (morph (generate {:cat :top}))]
                              (println (->> (parse expression)
                                            (map syntax-tree)
                                            (string/join ", "))))))))

