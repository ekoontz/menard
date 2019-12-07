(ns babylon.translate
  (:require [babylon.nederlands :as nl]
            [babylon.english :as en]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [pprint unify]]))

(def generate-this-many 1)

;; in the demo, we first generate a target expression in Dutch,
;; and then translate it to English.
;; If this is true, after generating the target expression, we
;; parse its surface form and then generate the source expression from that parse
;; tree.
;;
;; If this is false, we generate the source expression directly from the target
;; expression without first parsing it.
(def intermediate-parse? false)

(defn nl-to-en-spec [nl-expression]
  (log/debug (str "input:  " nl-expression))
  (let [retval
        {:cat (u/get-in nl-expression [:cat])
         :subcat []
         :modal (u/get-in nl-expression [:modal] false)
         :phrasal true
         :agr {:number (u/get-in nl-expression [:agr :number] :top)}
         :sem (u/get-in nl-expression [:sem])}]
    (log/debug (str "retval: " retval))
    retval))

(defn translate [source-expression]
  (if (:annotation source-expression)
    (println (str ";; " (:annotation source-expression))))
  ;; 1. print the surface form of the source expression:
  (-> source-expression
      ((fn [src] (nl/morph src :sentence-punctuation? true)))
      println)

  ;; 2. generate the target expression from the source expression:
  (-> source-expression
      (#(if intermediate-parse?
           (-> % nl/morph nl/parse shuffle first)
           %))
      ;; 2.a. create a specification for generation:
      nl-to-en-spec

      ;; 2.b. generate from this spec:
      en/generate

      ;; 2.c. print the surface form of the target expression:
      (en/morph :sentence-punctuation? true)
      println
      println))

(defn demo []
  (count
   (->>
    (range 0 (count nl/expressions))
    (map (fn [index]
           (let [source-expressions
                 (->> (repeatedly #(nl/generate (nth nl/expressions index)))
                      (take generate-this-many)
                      (filter #(not (nil? %))))]
             ;; for each expression:
             ;; generate it, and print the surface form
             ;; parse the surface form and return the first parse tree.
             (count
              (->> source-expressions
                   (mapcat translate)))))))))



