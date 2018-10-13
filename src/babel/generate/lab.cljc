(ns babel.generate.lab
  (:require [dag_unify.core
             :as u :refer [strip-refs unify]]
            [clojure.string :as string]))

(def baby-language
  {:grammar [(let [one (atom :top)
                   two (atom :top)]
               (unify {:phrasal true
                       :head {:cat :top
                              :phrasal false}
                       :comp {:cat :top
                              :phrasal false}}
                      {:1 one :head one
                       :2 two :comp two}))]
   :lexicon
   (into {} (for [[surface lexemes-for-surface]
                  {"ba" [{:cat :ba}]
                   "da" [{:cat :da}]
                   "ga" [{:cat :ga}]
                   "ma" [{:cat :ma}]}]
              [surface (map (fn [lexeme]
                              (merge lexeme {:surface surface}))
                        lexemes-for-surface)]))})

(defn morph [structure]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure

        (u/get-in structure [:surface])
        (morph (u/get-in structure [:surface]))
        
        (and (not (nil? (u/get-in structure [:1])))
             (not (nil? (u/get-in structure [:2]))))
        (->
         (->> 
          [(u/get-in structure [:1])
           (u/get-in structure [:2])]
          (map morph)
          (string/join " "))
         string/trim)

        true "??"))

(defn generate [spec]
  (binding [babel.generate/grammar (:grammar baby-language)
            babel.generate/lexicon (:lexicon baby-language)
            babel.generate/index-fn (fn [spec]
                                      (shuffle (flatten (vals (:lexicon baby-language)))))]
    (babel.generate/generate :top baby-language)))

