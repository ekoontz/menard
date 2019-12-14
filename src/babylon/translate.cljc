(ns babylon.translate
  (:require [babylon.nederlands :as nl]
            [babylon.english :as en]
            [babylon.generate :as g]
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
        {:agr {:number (u/get-in nl-expression [:agr :number] :top)}
         :cat (u/get-in nl-expression [:cat])
         :modal (u/get-in nl-expression [:modal] false)
         :phrasal true
         :reflexive (u/get-in nl-expression [:reflexive] :top)
         :sem (u/get-in nl-expression [:sem])
         :subcat []}]
    (log/debug (str "English spec to generate: " (u/strip-refs retval)))
    (unify
     retval
     (u/get-in nl-expression [:target] :top))))

(defn en-generate [spec allow-backtracking?]
  (binding [g/allow-backtracking? allow-backtracking?
            g/die-on-no-matching-lexemes? false
            g/generate-only-one? false]
    (en/generate spec)))

(defn translate [source-expression]
  (if (:note source-expression)
    (if false (println (str ";; " (:note source-expression)))))
  ;; 1. print the surface form of the source expression:

  (-> source-expression
      ((fn [src] (str (nl/morph src :sentence-punctuation? true) "|")))
      println)

  ;; 2. generate the target expression from the source expression:
  (-> source-expression
      (#(if intermediate-parse?
           (-> % nl/morph nl/parse shuffle first)
           %))
      ;; 2.a. create a specification for generation:
      nl-to-en-spec

      (#(unify %
               (u/get-in % [:target] :top)))

      ;; 2.b. generate from this spec:
      (#(or (en-generate % false)
            (en-generate % true)))

      ;; 2.c. print the surface form of the target expression:
      (#(str (reduce str (map (fn [x] (str " ")) (range 0 (* 1 (count (nl/morph source-expression :sentence-punctuation? true))))))
             "|" (en/morph % :sentence-punctuation? true)))

      println))


(defn demo [ & [index]]
  (count
   (->>
    (range 0 (count nl/expressions))
    (filter (fn [each-index]
              (or (nil? index) (= index each-index))))
    (map (fn [index]
           (let [generate-this-many (cond (:dotimes (nth nl/expressions index))
                                          (:dotimes (nth nl/expressions index))

                                          true
                                          1)

                 source-expressions
                 (->> (repeatedly #(nl/generate (nth nl/expressions index)))
                      (filter #(not (nil? %))) ;; a nil means that generation failed, so retry.
                      (take generate-this-many))]

             ;; for each expression:
             ;; generate it, and print the surface form
             ;; parse the surface form and return the first parse tree.
             (if (:note (nth nl/expressions index))
               (println (str "# " (:note (nth nl/expressions index)) "; " generate-this-many " example" (if (not (= 1 generate-this-many)) "s") ":")))
             (println "---")
             (count
              (->> source-expressions
                   (mapcat (fn [expr]
                             (translate expr)))))
             (println)))))))



