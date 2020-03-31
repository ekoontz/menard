(ns babylon.english
  #?(:cljs (:require-macros [babylon.grammar]))
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
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :as diag :refer [pprint]]))
;;
;; For generation and parsing of English.
;;
;; <lexicon>
#?(:clj
   (def lexical-rules
     [(l/read-and-eval "babylon/english/lexicon/rules/rules-0.edn")
      (l/read-and-eval "babylon/english/lexicon/rules/rules-1.edn")
      (l/read-and-eval "babylon/english/lexicon/rules/rules-2.edn")
      (l/read-and-eval "babylon/english/lexicon/rules/rules-3.edn")]))

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
       (compile-lexicon-source "babylon/english/lexicon/adjectives.edn")
       (compile-lexicon-source "babylon/english/lexicon/misc.edn")
       (compile-lexicon-source "babylon/english/lexicon/nouns.edn")
       (compile-lexicon-source "babylon/english/lexicon/propernouns.edn")
       (compile-lexicon-source "babylon/english/lexicon/verbs.edn"))))

#?(:cljs
   (def lexicon
     (-> (l/read-compiled-lexicon "babylon/english/lexicon/compiled.edn")
         l/deserialize-lexicon              
         vals
         flatten)))

#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon lexicon
                               "src/babylon/english/lexicon/compiled.edn")))

#?(:clj
   (def flattened-lexicon
     (flatten (vals lexicon))))

#?(:clj
   (def verb-lexicon
     (->> flattened-lexicon
          (filter #(and (not (u/get-in % [:exception]))
                        (= (u/get-in % [:cat]) :verb))))))

#?(:clj
   (def non-verb-lexicon
     (->> flattened-lexicon
          (filter #(and (not (= (u/get-in % [:cat]) :verb))
                        (not (u/get-in % [:exception])))))))

#?(:clj
   (defn index-fn [spec]
     (let [result
           (cond (= (u/get-in spec [:cat]) :verb)
                 verb-lexicon

                 (and (u/get-in spec [:cat])
                      (not (= :top (u/get-in spec [:cat]))))
                 non-verb-lexicon

                 true
                 (lazy-cat verb-lexicon non-verb-lexicon))]
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
                 ["babylon/english/morphology/nouns.edn"
                  "babylon/english/morphology/verbs.edn"]))

(declare an)
(declare sentence-punctuation)

(defn morph
  ([tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (-> (u/get-in tree [:syntax-tree])
         (s/morph morphology)     
         an)

     true
     (-> tree
         (s/morph morphology)
         an)))

  ([tree & {:keys [sentence-punctuation?]}]
   (if sentence-punctuation?
     (-> tree
         morph
         an
         (sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

;; </morphology>

;; <grammar>

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

(def slash-tenses ;; used for vp-aux-slash
  (->> finite-tenses
       (filter #(not (= :simple-present (u/get-in % [:abbreviation]))))))

(def tenses
  (concat finite-tenses
          [{:infl :base
            :variant :base}
           {:infl :gerund
            :variant :gerund}
           {:infl :past-participle
            :variant :past-participle}]))

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
          :aspect :pluperfect}}])

#?(:clj
   (def loaded-grammar
     (-> "babylon/english/grammar.edn"
         resource
         slurp
         read-string
         grammar/process)))

#?(:cljs
   (def loaded-grammar
     (->> (babylon.grammar/read-compiled-grammar
           "babylon/english/grammar/compiled.edn")
          (map dag_unify.serialization/deserialize))))

(def grammar
  (sort (fn [x y] 
          (not (u/get-in x [:comp :phrasal] false)))
        loaded-grammar))

#?(:clj
   (defn write-compiled-grammar []
     (grammar/write-compiled-grammar grammar
                                     "src/babylon/english/grammar/compiled.edn")))

;; </grammar>

(defn syntax-tree [tree]
  (s/syntax-tree tree morphology))

(defn an
  "change 'a' to 'an' if the next word starts with a vowel; 
   and change 'an' to 'a' if the next word does *not* start with a vowel."
  [input]
  (-> input
      (string/replace #"\b([aA]) ([aeiou])"  "$1n $2")
      (string/replace #"\b([aA])n ([^aeiou])" "$1 $2")))

(declare sentence-punctuation)

(defn sentence-punctuation
  "Capitalizes the first letter and puts a period (.) or question mark (?) at the end."
  [input mood]
  (str (string/capitalize (first input))
       (subs input 1 (count input))
       (if (= mood :interog)
         "?"
         ".")))

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec]
  (binding [] ;; g/stop-generation-at [:head :comp :head :comp]
    (log/debug (str "english generate: " (diag/strip-refs spec)))
    (g/generate spec
                grammar
                index-fn syntax-tree)))

(defn get-lexemes [spec]
  (g/get-lexemes spec index-fn syntax-tree))

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
  (-> "babylon/english/expressions.edn"
      grammar/read-expressions))

(defn demo []
  (count
   (->>
    (range 0 (count expressions))
    (map (fn [index]
           (let [generated-expression (first (->> (take 3 (repeatedly #(generate (nth expressions index))))
                                                  (filter #(not (nil? %)))))]
             (println (morph generated-expression
                             :sentence-punctuation? true))
             (println (syntax-tree generated-expression))
             (println)))))))

