(ns babylon.nederlands
  #?(:cljs (:require-macros [babylon.grammar]))
  (:require [clojure.string :as string]
            [babylon.lexiconfn :as l]
            [babylon.generate :as g]
            [babylon.grammar :as grammar]
            [babylon.morphology :as m]
            [babylon.parse :as p]
            [babylon.serialization :as s]
            [babylon.ug :as ug]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [pprint unify]]))
;;
;; For generation and parsing of Dutch.
;;

;; <lexicon>
#?(:clj
   (def lexical-rules
     [(l/read-and-eval "babylon/nederlands/lexicon/rules/rules-0.edn")
      (l/read-and-eval "babylon/nederlands/lexicon/rules/rules-1.edn")
      (l/read-and-eval "babylon/nederlands/lexicon/rules/rules-2.edn")
      (l/read-and-eval "babylon/nederlands/lexicon/rules/rules-3.edn")]))

#?(:clj
   (defn compile-lexicon-source [source-filename]
     (binding [babylon.lexiconfn/include-derivation? true]
       (-> source-filename
           l/read-and-eval
           l/add-exceptions-to-lexicon
           (l/apply-rules-in-order (nth lexical-rules 0) :0)
           (l/apply-rules-in-order (nth lexical-rules 1) :1)
           (l/apply-rules-in-order (nth lexical-rules 2) :2)
           (l/apply-rules-in-order (nth lexical-rules 3) :3)))))

#?(:clj
   (def lexicon
     (merge-with concat
       (compile-lexicon-source "babylon/nederlands/lexicon/adjectives.edn")
       (compile-lexicon-source "babylon/nederlands/lexicon/misc.edn")
       (compile-lexicon-source "babylon/nederlands/lexicon/nouns.edn")
       (compile-lexicon-source "babylon/nederlands/lexicon/propernouns.edn")
       (compile-lexicon-source "babylon/nederlands/lexicon/verbs.edn"))))

#?(:cljs
   (def lexicon
     (-> (l/read-compiled-lexicon "babylon/nederlands/lexicon/compiled.edn")
         l/deserialize-lexicon              
         vals
         flatten)))
#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon lexicon
                               "src/babylon/nederlands/lexicon/compiled.edn")))
#?(:clj
   (def flattened-lexicon
     (flatten (vals lexicon))))

#?(:clj
   (def verb-lexicon
     (->> flattened-lexicon
          (filter #(and (not (u/get-in % [:exception]))
                        (= (u/get-in % [:cat]) :verb))))))

#?(:clj
   (def adjective-lexicon
     (->> flattened-lexicon
          (filter #(and (not (u/get-in % [:exception]))
                        (= (u/get-in % [:cat]) :adjective))))))

#?(:clj
   (def noun-lexicon
     (->> flattened-lexicon
          (filter #(and (not (u/get-in % [:exception]))
                        (= (u/get-in % [:cat]) :noun))))))

#?(:clj
   (def misc-lexicon
     (->> flattened-lexicon
          (filter #(and (not (= (u/get-in % [:cat]) :verb))
                        (not (= (u/get-in % [:cat]) :adjective))
                        (not (= (u/get-in % [:cat]) :noun))
                        (not (u/get-in % [:exception])))))))

#?(:clj
   (defn index-fn [spec]
     (log/debug (str "spec: " (u/strip-refs spec)))
     (let [pre-result
           (cond (= (u/get-in spec [:cat]) :verb)
                 verb-lexicon

                 (= (u/get-in spec [:cat]) :adjective)
                 adjective-lexicon

                 (= (u/get-in spec [:cat]) :noun)
                 noun-lexicon
                 
                 true misc-lexicon)
           spec (if true spec (u/copy (u/strip-refs spec)))
           result (if true
                    pre-result
                    (->> pre-result
                         (map #(unify % spec))
                         (filter #(not (= :fail %)))))]
       (if true
         (shuffle result)
         result))))

#?(:cljs
   ;; note that we exclude [:exception]s from the lexemes that we use for
   ;; generation since they are only to be used for parsing.
   (def lexeme-map
     {:verb (->> lexicon
                 (filter #(= :verb (u/get-in % [:cat])))
                 (filter #(not (u/get-in % [:exception]))))
      :det (->> lexicon
                (filter #(= :det (u/get-in % [:cat]))))
      :intensifier (->> lexicon
                        (filter #(= :intensifier (u/get-in % [:cat]))))
      :noun (->> lexicon
                 (filter #(= :noun (u/get-in % [:cat])))
                 (filter #(not (u/get-in % [:exception]))))
      :top lexicon
      :adjective (->> lexicon
                      (filter #(= :adjective (u/get-in % [:cat]))))}))

#?(:cljs
   (defn index-fn [spec]
     ;; for now a somewhat bad index function: simply returns
     ;; lexemes which match the spec's :cat, or, if the :cat isn't
     ;; defined, just return all the lexemes.
     (let [result (get lexeme-map (u/get-in spec [:cat] :top) nil)]
       (if (not (nil? result))
           (shuffle result)
           (do
             (log/warn (str "no entry from cat: " (u/get-in spec [:cat] ::none) " in lexeme-map: returning all lexemes."))
             lexicon)))))

;; </lexicon>

;; <morphology>

(def morphology (m/compile-morphology
                 ["babylon/nederlands/morphology/adjectives.edn"
                  "babylon/nederlands/morphology/nouns.edn"
                  "babylon/nederlands/morphology/verbs.edn"]))

(declare sentence-punctuation)

(defn morph
  ([tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (s/morph (u/get-in tree [:syntax-tree]) morphology)

     true
     (s/morph tree morphology)))

  ([tree & {:keys [sentence-punctuation?]}]
   (if sentence-punctuation?
     (-> tree
         morph
         (sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

;; </morphology>

;; <grammar>
(def finite-tenses
  [;; "hij werkt"
   {:variant :present-simple
    :abbreviation :simple-present
    :infl :present
    :sem {:tense :present
          :aspect :simple}}])

(def inf-tense
  [;; "te [vp(:infinitive) zien de kat]"
   {:variant :infinitive
    :abbreviation :inf
    :infl :infinitive
    :sem {:tense :infinitive}}])

(def finite-plus-inf-tense
  [;; "hij werkt"
   {:variant :present-simple
    :abbreviation :simple-present
    :infl :present
    :sem {:tense :present
          :aspect :simple}}
   ;; "te [vp(:infinitive) zien de kat]"
   {:variant :infinitive
    :abbreviation :inf
    :infl :base
    :sem {:tense :infinitive}}])

#?(:clj
   (def grammar
     (-> "babylon/nederlands/grammar.edn"
         grammar/read-grammar
         (grammar/process true))))

#?(:cljs
   (def grammar
     (->> (babylon.grammar/read-compiled-grammar
           "babylon/nederlands/grammar/compiled.edn")
          (map dag_unify.serialization/deserialize))))

#?(:clj
   (defn write-compiled-grammar []
     (grammar/write-compiled-grammar grammar
                                     "src/babylon/nederlands/grammar/compiled.edn")))

;; </grammar>

(declare generate)
(declare syntax-tree)

(def expressions
  (->> (-> "babylon/nederlands/expressions.edn"
           grammar/read-expressions)
       (map (fn [spec]
              (let [generated-expression (generate spec)]
                (merge spec
                       {:example (morph generated-expression)
                        :size @g/count-adds}))))))
;; <functions>

(defn syntax-tree [tree]
  (s/syntax-tree tree morphology))

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec]
  (binding []) ;;  g/stop-generation-at [:head :comp :head :comp]
  (g/generate spec grammar index-fn syntax-tree))

(defn generate-seedlike
  [spec seed-size]
  (g/generate-seedlike spec seed-size grammar index-fn syntax-tree))

(defn parse [expression]
  (binding [p/grammar grammar
            p/syntax-tree syntax-tree
            l/lexicon lexicon
            l/morphology morphology
            p/split-on #"[ ]"
            p/lookup-fn l/matching-lexemes]
    (p/parse expression morph)))

(defn analyze [surface]
  (binding [l/lexicon lexicon
            l/morphology morphology]
    (l/matching-lexemes surface)))

#(:clj
  (defn demo []
    (count
     (->>
      (range 0 (count expressions))
      (map (fn [index]
             (let [generated-expressions
                   (->> (repeatedly (fn [] (generate (nth expressions index))))
                        (take 20)
                        (filter (fn [generated] (not (nil? generated)))))]
               ;; for each expression:
               ;; generate it, and print the surface form
               ;; parse the surface form and return the first parse tree.
               (count
                (->> generated-expressions
                     (map (fn [generated-expression]
                            (-> generated-expression
                                (morph :sentence-punctuation? true)
                                println)
                            (if false
                              (-> generated-expression
                                  morph
                                  parse
                                  first
                                  syntax-tree
                                  println))
                            (if false (println)))))))))))))

(defn demo-10 [index]
  (->>
   (repeatedly #(println (-> (nth expressions index)
                             generate
                             ((fn [x] (morph x :sentence-punctuation? true))))))
   (take 10)
   count))

(defn sentence-punctuation
  "Capitalizes the first letter and puts a period (.) or question mark (?) at the end."
  [input mood]
  (str (string/capitalize (first input))
       (subs input 1 (count input))
       (if (= mood :interog)
         "?"
         ".")))

(defn incremental-demo
  []
  (count (->> (generate-seedlike (nth expressions 16) 10)
              (take 10)
              (map #(morph % :sentence-punctuation? true))
              (map println))))


