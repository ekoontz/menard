(ns babylon.translate
  (:require [babylon.nederlands :as nl]
            [babylon.english :as en]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [pprint unify]]))

;; in the demo, we first generate a target expression in Dutch,
;; and then translate it to English.
;; If this is true, after generating the target expression, we
;; parse its surface form and then generate the source expression from that parse
;; tree.
;; If this is false, we generate the source expression directly from the target
;; expression without first parsing it.
(def intermediate-parse? false)

(defn demo []
  (count
   (->>
    (range 0 (count nl/expressions))
    (map (fn [index]
           (let [target-expressions
                 (->> (repeatedly #(nl/generate (nth nl/expressions index)))
                      (take 10)
                      (filter #(not (nil? %))))]
             ;; for each expression:
             ;; generate it, and print the surface form
             ;; parse the surface form and return the first parse tree.
             (count
              (->> target-expressions
                   (map (fn [target-expression]
                          (-> target-expression
                              (nl/morph :sentence-punctuation? true)
                              println)
                          (-> target-expression
                              (#(if intermediate-parse?
                                  (-> % nl/morph nl/parse shuffle first)
                                  %))
                              ((fn [target-expression]
                                 {:cat (u/get-in target-expression [:cat])
                                  :phrasal true
                                  :agr {:number (u/get-in target-expression [:agr :number] :top)}
                                  :sem (u/get-in target-expression [:sem])
                                  :mod (u/get-in target-expression [:mod])}))
                              en/generate
                              (en/morph :sentence-punctuation? true)
                              println)
                          (println)))))))))))



