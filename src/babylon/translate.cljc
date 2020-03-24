(ns babylon.translate
  (:require [babylon.nederlands :as nl]
            [babylon.english :as en]
            [babylon.generate :as g]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
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
        {:agr {:number (u/get-in nl-expression [:agr :number] :top)
               :person (u/get-in nl-expression [:agr :person] :top)
               :gender (let [gender (u/get-in nl-expression [:agr :gender] :top)]
                         (cond (or (= gender :masc) (= gender :fem))
                               gender
                               true :top))}
         :cat (u/get-in nl-expression [:cat])
         :phrasal true
         :reflexive (cond (= :top (u/get-in nl-expression [:reflexive] :top))
                          false
                          true
                          (u/get-in nl-expression [:reflexive] :top))

         :sem (u/get-in nl-expression [:sem])
         :subcat []}]
    (log/debug (str "English spec to generate: " (u/strip-refs retval)))
    (let [final-check
          (unify
           retval
           (u/get-in nl-expression [:target] :top))]
      (if (= :fail final-check)
        (do (log/warn (str "something was wrong with the :target spec: "
                           (u/fail-path retval (u/get-in nl-expression [:target] :top)) " "
                           ", so ignoring it."))
            retval)
        final-check))))

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

      ;; 2.b. generate from this spec:
      (#(en-generate % false))


      ;; 2.c. print the surface form of the target expression:
      (#(str (reduce str (map (fn [x] (str " ")) (range 0 (* 1 (count (nl/morph source-expression :sentence-punctuation? true))))))
             "|" (en/morph % :sentence-punctuation? true)))

      println))

(defn demo [ & [index this-many]]
  (count
   (->>
    (range 0 (count nl/expressions))
    (filter (fn [each-index]
              (or (nil? index) (= index each-index))))
    (map (fn [index]
           (let [generate-this-many (cond this-many this-many

                                          (:dotimes (nth nl/expressions index))
                                          (:dotimes (nth nl/expressions index))

                                          true
                                          1)
                 source-expressions
                 (let [spec (nth nl/expressions index)
                       size ((:size (nth nl/expressions index)))]
                   (->>
                    (cond (and (> size 20)
                               (> generate-this-many 1))
                          (nl/generate-seedlike spec (int (* size 0.3)))
                          true
                          (repeatedly #(nl/generate spec)))
                    (take generate-this-many)))]

             ;; for each expression:
             ;; generate it, and print the surface form
             ;; parse the surface form and return the first parse tree.
             (if (:note (nth nl/expressions index))
               (println (str "# " (:note (nth nl/expressions index)) "; " generate-this-many " example" (if (not (= 1 generate-this-many)) "s") ":")))
             (println "---")
             (count
              (->> source-expressions
                   (mapcat translate)))
             (println)))))))

(defn translate-with-target-spec [spec]
  (let [target (-> spec
                   nl/generate)
        source (-> target
                   nl-to-en-spec
                   en/generate)]
    {:source (en/morph source)
     :target (en/morph target)}))


(defn fast-demo []
  (count (map (fn [i]
                (vec (take 1 (repeatedly #(-> (nth nl/expressions i)
                                              nl/generate
                                              ((fn [tree]
                                                 (println
                                                  (str i ". "
                                                       (nl/morph tree
                                                                 :sentence-punctuation? true)))
                                                 tree))
                                              nl-to-en-spec
                                              en/generate
                                              ((fn [tree]
                                                 (println
                                                  (str "   "
                                                       (en/morph tree
                                                                 :sentence-punctuation? true)))))
                                              println
                                              ((fn [nothing]
                                                 "")))))))
              (range 0 (count nl/expressions)))))
