(ns babylon.nederlands
  (:require #?(:clj [clojure.java.io :as io :refer [resource]])
            [clojure.string :as string]
            [babylon.lexiconfn :as l]
            [babylon.generate :as g]
            [babylon.grammar :as grammar]
            [babylon.morphology :as m]
            [babylon.parse :as p]
            [babylon.serialization :as s]
            [babylon.ug :as ug]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [pprint unify]]
            [dag_unify.serialization :refer [serialize]]))
;;
;; For generation and parsing of Dutch.
;;

#?(:clj
   (def lexical-rules
     [(l/read-and-eval "babylon/nederlands/lexicon/rules/rules-0.edn")
      (l/read-and-eval "babylon/nederlands/lexicon/rules/rules-1.edn")
      (l/read-and-eval "babylon/nederlands/lexicon/rules/rules-2.edn")]))

#?(:clj
   (defn compile-lexicon-source [source-filename]
     (-> source-filename
         l/read-and-eval
         l/add-exceptions-to-lexicon
         (l/apply-rules-in-order (nth lexical-rules 0) :0)
         (l/apply-rules-in-order (nth lexical-rules 1) :1)
         (l/apply-rules-in-order (nth lexical-rules 2) :2))))

#?(:clj
   (def lexicon
     (merge-with concat
       (compile-lexicon-source "babylon/nederlands/lexicon/adjectives.edn")
       (compile-lexicon-source "babylon/nederlands/lexicon/misc.edn")
       (compile-lexicon-source "babylon/nederlands/lexicon/nouns.edn")
       (compile-lexicon-source "babylon/nederlands/lexicon/propernouns.edn")
       (compile-lexicon-source "babylon/nederlands/lexicon/verbs.edn"))))

(def finite-tenses
  [;; "hij werkt"
   {:variant :present-simple
    :abbreviation :simple-present
    :infl :present
    :modal false
    :sem {:tense :present
          :aspect :simple}}])

#?(:clj
   (def grammar
     (-> "babylon/nederlands/grammar.edn"
         resource
         slurp
         read-string
         grammar/process)))

#?(:clj
   (def morphology
     (concat
      (-> "babylon/nederlands/morphology/adjectives.edn"
          l/read-and-eval)
      (-> "babylon/nederlands/morphology/nouns.edn"
          l/read-and-eval)
      (-> "babylon/nederlands/morphology/verbs.edn"
          l/read-and-eval))))

#?(:clj
   (def expressions
     (-> "babylon/nederlands/expressions.edn"
         resource slurp read-string eval)))

#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon lexicon
                               "src/babylon/nederlands/lexicon/compiled.edn")))

#?(:clj
   (defn write-compiled-grammar []
     (grammar/write-compiled-grammar grammar
                                     "src/babylon/nederlands/grammar/compiled.edn")))

#?(:cljs
   (defn slurp [x]))
#?(:cljs
   (defn read-string [x]))
#?(:cljs
   (defn resource [x]))
#?(:cljs
   (def morphology nil))

(defmacro read-compiled-lexicon []
  `~(-> "babylon/nederlands/lexicon/compiled.edn"
        resource
        slurp
        read-string))

(defmacro read-compiled-grammar []
  `~(-> "babylon/nederlands/grammar/compiled.edn"
        resource
        slurp
        read-string))

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

(defn syntax-tree [tree]
   (s/syntax-tree tree morphology))

(defn sentence-punctuation
  "Capitalizes the first letter and puts a period (.) or question mark (?) at the end."
  [input mood]
  (str (string/capitalize (first input))
       (subs input 1 (count input))
       (if (= mood :interog)
         "?"
         ".")))

(def flattened-lexicon
  (flatten (vals lexicon)))

(def verb-lexicon
  (->> flattened-lexicon
       (filter #(and (not (u/get-in % [:exception]))
                     (= (u/get-in % [:cat]) :verb)))))

(defmacro verb-lexicon-macro []
  `~verb-lexicon)

(def non-verb-lexicon
  (->> flattened-lexicon
       (filter #(and (not (= (u/get-in % [:cat]) :verb))
                     (not (u/get-in % [:exception]))))))

(defn index-fn [spec]
  (let [result
        (cond (= (u/get-in spec [:cat]) :verb)
              verb-lexicon

              (and (= (u/get-in spec [:cat]))
                   (not (= :top (u/get-in spec [:cat]))))
              non-verb-lexicon

              true
              (lazy-cat verb-lexicon non-verb-lexicon))]
    (if true
      (shuffle result)
      result)))

(defn lookup
  "find lexemes that satisfy _spec_."
  [spec]
  (let [spec (let [with-subcat-empty
                   (unify spec {:subcat []})]
               (if (= :fail with-subcat-empty)
                 spec
                 with-subcat-empty))]
    (g/get-lexemes spec index-fn syntax-tree)))

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec]
  (binding [] ;;  g/stop-generation-at [:head :comp :head :comp]
    (try
      (g/generate spec grammar index-fn syntax-tree)
     (catch Exception e
       (log/debug (str "generation failed: " e "; input spec was:" spec))))))

(defn get-lexemes [spec]
  (g/get-lexemes spec index-fn))

(defn generate-n
  "generate _n_ consecutive in-order expressions that satisfy _spec_."
  [spec n]
  (take n (repeatedly #(generate spec))))

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

(defn demo []
  (count
   (->>
    (range 0 (count expressions))
    (map (fn [index]
           (let [generated-expressions
                 (->> (repeatedly #(generate (nth expressions index)))
                      (take 20)
                      (filter #(not (nil? %))))]
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
                          (if false (println))))))))))))

