(ns babylon.nederlands
  (:require [clojure.string :as string]
            [babylon.lexiconfn :as l]
            [babylon.generate :as g]
            [babylon.nederlands.resources :as resources]
            [babylon.parse :as p]
            [babylon.serialization :as s]
            #?(:clj [clojure.tools.logging :as log])
            #?(:clj [clojure.java.io :as io :refer [resource]])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [pprint unify]]
            [dag_unify.serialization :refer [serialize]]))
;;
;; For generation and parsing of Dutch.
;;

(declare sentence-punctuation)
(declare morphology)

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
  (flatten (vals resources/lexicon)))

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
  (binding []) ;;  g/stop-generation-at [:head :comp :head :comp]
  (g/generate spec resources/grammar index-fn syntax-tree))

(defn get-lexemes [spec]
  (g/get-lexemes spec index-fn syntax-tree))

(defn generate-n
  "generate _n_ consecutive in-order expressions that satisfy _spec_."
  [spec n]
  (take n (repeatedly #(generate spec))))

(defn parse [expression]
  (binding [p/grammar resources/grammar
            p/syntax-tree syntax-tree
            l/lexicon resources/lexicon
            l/morphology morphology
            p/split-on #"[ ]"
            p/lookup-fn l/matching-lexemes]
    (p/parse expression morph)))

(defn analyze [surface]
  (binding [l/lexicon resources/lexicon
            l/morphology morphology]
    (l/matching-lexemes surface)))              

(defn demo []
  (count
   (->>
    (range 0 (count resources/expressions))
    (map (fn [index]
           (let [generated-expressions
                 (->> (repeatedly #(generate (nth resources/expressions index)))
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

(defn testing []
  (let [testing (fn []
                  (time (testing-with resources/grammar index-fn syntax-tree)))]
    (repeatedly #(do (println (str " " (sentence-punctuation (morph (testing)) :decl)))
                     1))))

(def bigram
  {:phrasal true
   :head {:phrasal false}
   :comp {:phrasal false}
   :subcat []})

(def see-bigram
  {:phrasal true
   :sem {:pred :see
         :subj {:pred :i}}
   :head {:phrasal false}
   :comp {:phrasal false}
   :subcat []})

(defn bigrams []
  (repeatedly #(println
                (morph (time
                        (g/generate
                         bigram
                         resources/grammar index-fn syntax-tree))))))

(defmacro compile-morphology []
  `(concat
     ~(-> "babylon/nederlands/morphology/adjectives.edn"
          l/read-and-eval)
     ~(-> "babylon/nederlands/morphology/nouns.edn"
          l/read-and-eval)
     ~(-> "babylon/nederlands/morphology/verbs.edn"
          l/read-and-eval)))

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

(defmacro read-expressions []
  `~(-> "babylon/nederlands/expressions.edn"
        resource
        slurp
        read-string))

(defmacro verb-lexicon-macro []
  `~verb-lexicon)

#?(:clj
   (def morphology
     (compile-morphology)))

#?(:cljs
   (defn slurp [x]))
#?(:cljs
   (defn read-string [x]))
#?(:cljs
   (defn resource [x]))
#?(:cljs
   (def morphology nil))
#?(:cljs
   (def grammar nil))
#?(:cljs
   (def lexicon nil))
