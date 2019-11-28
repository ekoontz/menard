(ns babylon.translate
  (:require [babylon.nederlands :as nl]
            [babylon.english :as en]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [pprint unify]]))

(defn demo []
  (count
   (->>
    (range 0 (count nl/expressions))
    (map (fn [index]
           (let [generated-expressions
                 (->> (repeatedly #(nl/generate (nth nl/expressions index)))
                      (take 10)
                      (filter #(not (nil? %))))]
             ;; for each expression:
             ;; generate it, and print the surface form
             ;; parse the surface form and return the first parse tree.
             (count
              (->> generated-expressions
                   (map (fn [generated-expression]
                          (-> generated-expression
                              (nl/morph :sentence-punctuation? true)
                              println)
                          (if true
                            (-> generated-expression
                                nl/morph
                                nl/parse
                                first
                                ((fn [expression]
                                   {:cat (u/get-in expression [:cat])
                                    :phrasal true
                                    :agr {:number (u/get-in expression [:agr :number])}
                                    :sem (u/get-in expression [:sem])
                                    :mod (u/get-in expression [:mod])}))
                                en/generate
                                (en/morph :sentence-punctuation? true)
                                println))
                          (if true (println))))))))))))


