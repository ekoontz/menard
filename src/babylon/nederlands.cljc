(ns babylon.nederlands
  (:require #?(:clj [clojure.java.io :refer [resource]])
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
            [dag_unify.core :as u :refer [pprint unify]]))
;;
;; For generation and parsing of Dutch.
;;

;; <lexicon>
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

#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon lexicon
                               "src/babylon/nederlands/lexicon/compiled.edn")))

(defmacro read-compiled-lexicon []
  `~(-> "babylon/nederlands/lexicon/compiled.edn"
        resource
        slurp
        read-string))

#?(:clj
   (def flattened-lexicon
     (flatten (vals lexicon))))

#?(:clj
   (def verb-lexicon
     (->> flattened-lexicon
          (filter #(and (not (u/get-in % [:exception]))
                        (= (u/get-in % [:cat]) :verb))))))

(defmacro verb-lexicon-macro []
  `~verb-lexicon)

(def non-verb-lexicon
  (->> flattened-lexicon
       (filter #(and (not (= (u/get-in % [:cat]) :verb))
                     (not (u/get-in % [:exception]))))))

;; </lexicon>

;; <morphology>
(defmacro compile-morphology []
  (let [filenames ["babylon/nederlands/morphology/adjectives.edn"
                   "babylon/nederlands/morphology/nouns.edn"
                   "babylon/nederlands/morphology/verbs.edn"]]
    `(reduce
      concat
      ~(vec (map (fn [filename]
                   (l/read-and-eval filename))
                 filenames)))))

;; TODO: move other cljs functions to this file as
;; with this one (def morphology).
(def morphology (compile-morphology))

(declare sentence-punctuation)

#?(:clj
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
            (sentence-punctuation (u/get-in tree [:sem :mood] :decl)))))))

#?(:cljs
   (defn morph
     ([tree]
      (log/info (str "in babylon.nederlands morph.."))
      (cond
        (map? (u/get-in tree [:syntax-tree]))
        (s/morph (u/get-in tree [:syntax-tree]) morphology)

        true
        (s/morph tree morphology)))

     ([tree & {:keys [sentence-punctuation?]}]
      (if sentence-punctuation?
        (-> tree
            morph
            (sentence-punctuation (u/get-in tree [:sem :mood] :decl)))))))

;; </morphology>

;; <grammar>
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
   (defn write-compiled-grammar []
     (grammar/write-compiled-grammar grammar
                                     "src/babylon/nederlands/grammar/compiled.edn")))

;; </grammar>

;; <expressions>

#?(:clj
   (def expressions
     (-> "babylon/nederlands/expressions.edn"
         resource slurp read-string eval)))

(defmacro read-expressions []
  `~(-> "babylon/nederlands/expressions.edn"
        resource
        slurp
        read-string))

#?(:cljs
   (def expressions-atom (atom nil)))

;; </expressions>


;; <functions>

#?(:clj
   (defn syntax-tree [tree]
      (s/syntax-tree tree morphology)))

#?(:cljs
   (defn syntax-tree [tree]
     (s/syntax-tree tree morphology)))

#?(:clj
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
         result))))

#?(:clj
   (defn generate
     "generate one random expression that satisfies _spec_."
     [spec]
     (binding []) ;;  g/stop-generation-at [:head :comp :head :comp]
     (g/generate spec grammar index-fn syntax-tree)))

#?(:clj
   (defn get-lexemes [spec]
     (g/get-lexemes spec index-fn syntax-tree)))

#?(:clj
   (defn generate-n
     "generate _n_ consecutive in-order expressions that satisfy _spec_."
     [spec n]
     (take n (repeatedly #(generate spec)))))

(defn parse [expression]
  (binding [p/grammar grammar
            p/syntax-tree syntax-tree
            l/lexicon lexicon
            l/morphology morphology
            p/split-on #"[ ]"
            p/lookup-fn l/matching-lexemes]
    (p/parse expression morph)))

#?(:clj
   (defn analyze [surface]
     (binding [l/lexicon lexicon
               l/morphology morphology]
       (l/matching-lexemes surface))))              

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

(defn generate-word-by-word [tree grammar index-fn syntax-tree]
  (let [add-rule (fn [tree]
                   (first (g/add-rule tree grammar syntax-tree)))
        add-lexeme (fn [tree]
                     (first (g/add-lexeme tree index-fn syntax-tree)))
        add (fn [tree]
              (let [at (g/frontier tree)
                    add-phrasal? (u/get-in tree (concat at [:phrasal]))]
                (cond add-phrasal?
                      (add-rule tree)
                      true
                      (add-lexeme tree))))]
    (log/debug (str "intermediate result: " (morph tree)))
    (cond
      (nil? tree) tree
      (:fail tree) tree
      (u/get-in tree [:babylon.generate/done?]) tree
      true (generate (add tree) grammar index-fn syntax-tree))))

(defn testing-with [grammar index-fn syntax-tree]
  (g/generate
   {:phrasal true
    :rule "s"
    :reflexive false
    :comp {:phrasal true
           :rule "np"
           :head {:phrasal true
                  :comp {:phrasal true}}
           :comp {:phrasal false}}
    :head {:phrasal true
           :rule "vp"
           :head {:phrasal false}
           :comp {:phrasal true
                  :rule "np"
                  :head {:phrasal true
                         :comp {:phrasal true}}
                  :comp {:phrasal false}}}}
   grammar index-fn syntax-tree))

#?(:clj
   (defn testing []
     (let [testing (fn []
                     (time (testing-with grammar index-fn syntax-tree)))]
       (repeatedly #(do (println (str " " (sentence-punctuation (morph (testing)) :decl)))
                        1)))))

(def bigram
  {:phrasal true
   :head {:phrasal false}
   :comp {:phrasal false}
   :subcat []})

#?(:clj
   (defn bigrams []
     (repeatedly #(println
                   (morph (time
                           (g/generate
                            bigram
                            grammar index-fn syntax-tree)))))))

(defn swap-with [the-atom the-fn]
  (or @the-atom
      (swap! the-atom (fn [x] (the-fn @the-atom)))))

(defn sentence-punctuation
  "Capitalizes the first letter and puts a period (.) or question mark (?) at the end."
  [input mood]
  (str (string/capitalize (first input))
       (subs input 1 (count input))
       (if (= mood :interog)
         "?"
         ".")))

;; </functions>
