(ns babylon.nederlands
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [babylon.lexiconfn :as l]
            [babylon.generate :as g]
            [babylon.grammar :as grammar]
            [babylon.morphology :as m]
            [babylon.parse :as p]
            [babylon.serialization :as s]
            [babylon.ug :as ug]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [pprint unify]]))
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

(def lexicon
  (merge-with concat
    (compile-lexicon "babylon/nederlands/lexicon/adjectives.edn")
    (compile-lexicon "babylon/nederlands/lexicon/misc.edn")
    (compile-lexicon "babylon/nederlands/lexicon/propernouns.edn")
    (compile-lexicon "babylon/nederlands/lexicon/nouns.edn")
    (compile-lexicon "babylon/nederlands/lexicon/verbs.edn")))

(def finite-tenses
  [;; "would see"
   {:variant :conditional
    :infl :present
    :sem {:tense :conditional}
    :head {:aux true}}

   ;; "will see"
   {:variant :future
    :infl :present
    :sem {:tense :future}
    :head {:aux true}}

   ;; "sees"
   {:variant :present-simple
    :abbreviation :simple-present
    :infl :present
    :modal false
    :sem {:tense :present
          :aspect :simple}}

   ;; "can see"
   {:variant :modal
    :infl :base
    :modal :base
    :head {:modal :base}
    :sem {:tense :present
          :aspect :simple}}

   ;; "tries to see"
   {:variant :modal-present
    :infl :present
    :modal :infinitive
    :head {:modal :infinitive}
    :sem {:tense :present
          :aspect :simple}}

   ;; "saw"
   {:variant :past
    :infl :past-simple
    :sem {:tense :past
          :aspect :simple}}

   ;; "is seeing"
   {:variant :present-progressive
    :infl :present
    :head {:aux true}
    :sem {:tense :present
          :aspect :progressive}}

   ;; "was seeing"
   {:variant :past-progressive
    :infl :past-simple    
    :head {:aux true}
    :sem {:tense :past
          :aspect :progressive}}

   ;; "has seen"
   {:variant :perfect
    :infl :present
    :head {:aux true}
    :sem {:tense :past 
          :aspect :perfect}}

   ;; "had seen"
   {:variant :pluperfect
    :infl :past-simple
    :head {:aux true}
    :sem {:tense :past
          :aspect :pluperfect}}])

(comment
  (def slash-tenses ;; used for vp-aux-slash
    (->> finite-tenses
         (filter #(not (= :simple-present (u/get-in % [:abbreviation])))))))

(comment
  (def tenses
   (concat finite-tenses
           [{:infl :base
             :variant :base}
            {:infl :gerund
             :variant :gerund}
            {:infl :past-participle
             :variant :past-participle}])))
(comment
  (def aux-tenses
    [{:infl :present
      :sem {:tense :conditional}}
     {:infl :present
      :sem {:tense :future}}
     {:infl :present
      :sem {:tense :present
            :aspect :progressive}}
     {:infl :past-simple
      :sem {:tense :past
            :aspect :progressive}}
     {:infl :present
      :sem {:tense :past
            :aspect :perfect}}
     {:infl :past-simple
      :sem {:tense :past
            :aspect :pluperfect}}]))

(def grammar
  (-> "babylon/nederlands/grammar.edn"
      io/resource
      slurp
      read-string
      grammar/process))

(def morphology
  (concat
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
       (log/warn (str "generation failed: " e "; input spec was:" spec))))))


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
            p/lookup-fn l/matching-lexemes]
    (p/parse expression morph)))

(defn analyze [surface]
  (binding [l/lexicon lexicon
            l/morphology morphology]
    (l/matching-lexemes surface)))              

(def expressions
  (-> "babylon/nederlands/expressions.edn"
      io/resource slurp read-string eval))

(defn demo []
  (empty?
   (->>
    (range 0 (count expressions))
    (pmap (fn [index]
            (let [generated-expression (generate (nth expressions index))]
              (println (morph generated-expression
                              :sentence-punctuation? true))
              (println (syntax-tree generated-expression))
              (println)))))))


