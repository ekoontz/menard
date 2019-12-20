(ns babylon.nederlands
  (:require #?(:clj [clojure.java.io :as io])
            [clojure.string :as string]
            [babylon.lexiconfn :as l]
            [babylon.generate :as g]
            [babylon.grammar :as grammar]
            [babylon.morphology :as m]
            [babylon.parse :as p]
            [babylon.serialization :as s]
            [babylon.ug :as ug]
            #?(:clj [clojure.core :refer [read-string]])
            #?(:cljs [clojure.reader :refer [read-string]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [pprint unify]])
  #?(:cljs (:require-macros [babylon.io :refer [load-the-file]])))

;; 
;; For generation and parsing of Dutch.
;;
(defn apply-rules-to [lexicon]
  (-> lexicon
      (l/apply-rules-in-order (l/read-rules "babylon/nederlands/lexicon/rules/rules-0.edn") :0)
      (l/apply-rules-in-order (l/read-rules "babylon/nederlands/lexicon/rules/rules-1.edn") :1)
      (l/apply-rules-in-order (l/read-rules "babylon/nederlands/lexicon/rules/rules-2.edn") :2)))

(defn compile-lexicon [filename]
  (-> filename
      l/read-rules
      l/add-exceptions-to-lexicon
      apply-rules-to))

(def js-lexicon
  #?(:cljs {"babylon/nederlands/lexicon/adjectives.edn"
            (load-the-file "babylon/nederlands/lexicon/adjectives.edn")

            "babylon/nederlands/lexicon/misc.edn"
            (load-the-file "babylon/nederlands/lexicon/misc.edn")

            "babylon/nederlands/lexicon/propernouns.edn"
            (load-the-file "babylon/nederlands/lexicon/propernouns.edn")

            "babylon/nederlands/lexicon/nouns.edn"
            (load-the-file "babylon/nederlands/lexicon/nouns.edn")

            "babylon/nederlands/lexicon/verbs.edn"
            (load-the-file "babylon/nederlands/lexicon/verbs.edn")}))

(def lexicon
  (merge-with concat
    (compile-lexicon "babylon/nederlands/lexicon/adjectives.edn")
    (compile-lexicon "babylon/nederlands/lexicon/misc.edn")
    (compile-lexicon "babylon/nederlands/lexicon/propernouns.edn")
    (compile-lexicon "babylon/nederlands/lexicon/nouns.edn")
    (compile-lexicon "babylon/nederlands/lexicon/verbs.edn")))

(def finite-tenses
  [;; "hij werkt"
   {:variant :present-simple
    :abbreviation :simple-present
    :infl :present
    :modal false
    :sem {:tense :present
          :aspect :simple}}])

(def grammar
  #?(:clj
     (-> "babylon/nederlands/grammar.edn"
         io/resource
         slurp
         read-string
         grammar/process)))

(def morphology
  (concat
   (-> "babylon/nederlands/morphology/adjectives.edn"
       l/read-rules)
   (-> "babylon/nederlands/morphology/nouns.edn"
       l/read-rules)
   (-> "babylon/nederlands/morphology/verbs.edn"
       l/read-rules)))

(declare an)
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

(defn lookup
  "find lexemes that satisfy _spec_."
  [spec]
  (binding [g/lexicon lexicon
            g/syntax-tree syntax-tree]
    (let [spec (let [with-subcat-empty
                     (unify spec {:subcat []})]
                 (if (= :fail with-subcat-empty)
                   spec
                   with-subcat-empty))]
       (g/get-lexemes spec))))

(def flattened-lexicon
  (flatten (vals lexicon)))

(def verb-lexicon
  (->> flattened-lexicon
       (filter #(and (not (u/get-in % [:exception]))
                     (= (u/get-in % [:cat]) :verb)))))
  
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

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec]
  (binding [g/grammar grammar
            g/lexicon lexicon
            g/syntax-tree syntax-tree
;;            g/stop-generation-at [:head :comp :head :comp]
            g/lexicon-index-fn index-fn]
    (try
      (-> spec
          g/generate)
     (catch Exception e
       (log/debug (str "generation failed: " e "; input spec was:" spec))))))


(defn get-lexemes [spec]
  (binding [g/lexicon-index-fn index-fn]
    (g/get-lexemes spec)))

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

(def expressions
 #?(:clj
    (-> "babylon/nederlands/expressions.edn"
        io/resource
        slurp
        read-string
        eval)))

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

