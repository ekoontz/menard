(ns menard.translate
  (:require [menard.nederlands :as nl]
            [menard.english :as en]
            [menard.generate :as g]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            [dag_unify.diagnostics :as diag]))

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
        {:agr {:number
               (or
                (u/get-in nl-expression [:sem :subj :ref :number])
                (u/get-in nl-expression [:agr :number] :top))
               :person (u/get-in nl-expression [:agr :person] :top)
               :gender (let [gender (u/get-in nl-expression [:agr :gender] :top)]
                         (cond (or (= gender :masc) (= gender :fem))
                               gender
                               :else :top))}
         :cat (u/get-in nl-expression [:cat])
         :comp {:interrogative? (u/get-in nl-expression [:comp :interrogative?] :top)}
         
         :max-depth (u/get-in nl-expression [:target :max-depth] g/max-depth)
         :phrasal true
         :reflexive (cond (= :top (u/get-in nl-expression [:reflexive] :top))
                          false
                          :else
                          (u/get-in nl-expression [:reflexive] :top))


         ;; TODO: this is totally unintuitive: see TODO(1) below.
         :sem (u/unify (u/get-in nl-expression [:sem])
                       {:obj {:obj (u/get-in nl-expression [:sem :obj :obj] :unspec)}})


         :subcat []}]
    (log/debug (str "English spec to generate: " (serialize retval)))
    (let [final-check
          (unify
           retval
           (u/get-in nl-expression [:target] :top))]
      (if (= :fail final-check)
        (do (log/warn (str "something was wrong with the :target spec: "
                           (diag/fail-path retval (u/get-in nl-expression [:target] :top)) " "
                           ", so ignoring it."))
            retval)
        final-check))))

(defn en-generate [spec allow-backtracking?]
  (binding [g/allow-backtracking? allow-backtracking?]
    (en/generate spec)))

(defn translate [source-expression]
  (when (:note source-expression)
    (when false (println (str ";; " (:note source-expression)))))
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
      (#(en-generate % true))

      ;; 2.c. print the surface form of the target expression:
      (#(str (reduce str (map
                          (fn [_] (str " ")) (range 0 (* 1 (count (nl/morph source-expression :sentence-punctuation? true))))))
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

                                          :else
                                          5)
                 source-expressions
                 (let [spec (nth nl/expressions index)]
                   (->>
                    (repeatedly #(nl/generate spec))
                    (take generate-this-many)))]
             ;; for each expression:
             ;; generate it, and print the surface form
             ;; parse the surface form and return the first parse tree.
             (when (:note (nth nl/expressions index))
               (println (str "# " (:note (nth nl/expressions index)) "; "
                             generate-this-many " example"
                             (when (not (= 1 generate-this-many)) "s") ":")))
             (println "---")
             (count
              (->> source-expressions
                   (mapcat translate)))
             (println)))))))

(comment
  "TODO (1) from above. It's to prevent:
   generating:"

  (-> (->> "ik heb het geld nodig"
           nl/parse
           (take 1))
      first
      nl-to-en-spec
      en/generate
      en/morph)

  "being: 'I need to have'"
  
  "with semantics:"
  
  {:tense :present,
   :aspect :simple,
   :pred :need,
   :mod [],
   :subj
   [[1]
    {:pred :i,
     :ref {:number [[2] :sing], :human true},
     :mod [],
     :context :unspec}],
   :obj
   {:tense :past,
    :aspect :perfect,
    :pred :money,
    :ref {:number :sing},
    :top :top,
    :mod [],
    :quant :the,
    :context :unspec,
    :subj [1],
    :obj :anaphor}})


