(ns menard.english
  #?(:cljs (:require-macros [menard.grammar]))
  (:require #?(:clj [clojure.java.io :refer [resource]])
            [clojure.string :as string]
            [menard.lexiconfn :as l]
            [menard.generate :as g]
            [menard.grammar :as grammar]
            [menard.model :as model]
            [menard.morphology :as m]
            [menard.nesting :as nest]
            [menard.parse :as p]
            [menard.serialization :as s]
            [menard.ug :as ug]
            [menard.subcat :as su]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [pprint unify]]
            [dag_unify.diagnostics :as diag]))
;;
;; For generation and parsing of English.
;;

;; <morphology>
#?(:clj
   (defn load-morphology []
     (m/compile-morphology-fn
      [(model/use-path "menard/english/morphology/misc.edn")
       (model/use-path "menard/english/morphology/nouns.edn")
       (model/use-path "menard/english/morphology/verbs.edn")])))

#?(:cljs
   (defn load-morphology []
     (m/compile-morphology
      ["menard/english/morphology/misc.edn"
       "menard/english/morphology/nouns.edn"
       "menard/english/morphology/verbs.edn"])))

;; </morphology>

;; <lexicon>

#?(:clj
   (defn load-lexical-rules []
     [(l/read-and-eval (model/use-path "menard/english/lexicon/rules/rules-0.edn"))
      (l/read-and-eval (model/use-path "menard/english/lexicon/rules/rules-1.edn"))
      (l/read-and-eval (model/use-path "menard/english/lexicon/rules/rules-2.edn"))
      (l/read-and-eval (model/use-path "menard/english/lexicon/rules/rules-3.edn"))]))

#?(:clj
   (defn compile-lexicon-source [source-filename lexical-rules & [unify-with]]
     (binding [menard.lexiconfn/include-derivation? true]
       (-> source-filename
           l/read-and-eval
           ((fn [lexicon]
              (l/apply-to-every-lexeme lexicon
                                       (fn [lexeme]
                                         (if (nil? unify-with)
                                           lexeme
                                           (unify lexeme unify-with))))))
           l/add-exceptions-to-lexicon
           (l/apply-rules-in-order (nth lexical-rules 0) :0)
           (l/apply-rules-in-order (nth lexical-rules 1) :1)
           (l/apply-rules-in-order (nth lexical-rules 2) :2)
           (l/apply-rules-in-order (nth lexical-rules 3) :3)))))

#?(:clj
   (defn load-lexicon [lexical-rules]
     (merge-with concat
                 (compile-lexicon-source (model/use-path "menard/english/lexicon/adjectives.edn") lexical-rules {:cat :adjective})
                 (compile-lexicon-source (model/use-path "menard/english/lexicon/adverbs.edn") lexical-rules {:cat :adverb})

                 ;; misc has various :cat values, so can't supply a :cat for this part of the lexicon:
                 (compile-lexicon-source (model/use-path "menard/english/lexicon/misc.edn") lexical-rules)
                 
                 (compile-lexicon-source (model/use-path "menard/english/lexicon/nouns.edn") lexical-rules {:cat :noun})
                 (compile-lexicon-source (model/use-path "menard/english/lexicon/numbers.edn") lexical-rules {:cat :adjective})
                 (compile-lexicon-source (model/use-path "menard/english/lexicon/propernouns.edn") lexical-rules {:cat :noun :pronoun false :propernoun true})
                 (compile-lexicon-source (model/use-path "menard/english/lexicon/verbs.edn") lexical-rules {:cat :verb}))))

#?(:cljs
   (def lexicon
     (-> (l/read-compiled-lexicon "menard/english/lexicon/compiled.edn")
         l/deserialize-lexicon              
         vals
         flatten)))

#?(:clj
  (defn fill-lexicon-indexes [lexicon]
    (let [flattened-lexicon (flatten (vals lexicon))]
      {:non-verb-lexicon
       (->> flattened-lexicon
            (filter #(and (not (= (u/get-in % [:cat]) :verb))
                          (not (u/get-in % [:exception])))))
       :verb-lexicon
       (->> flattened-lexicon
            (filter #(and (not (u/get-in % [:exception]))
                          (= (u/get-in % [:cat]) :verb))))})))

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

;; </lexicon>

(declare an)
(declare sentence-punctuation)

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

#?(:cljs
   (def loaded-grammar
     (->> (menard.grammar/read-compiled-grammar
           "menard/english/grammar/compiled.edn")
          (map dag_unify.serialization/deserialize))))

;; </grammar>

#?(:clj
   (defn load-grammar []
     (-> (model/use-path "menard/english/grammar.edn")
         grammar/read-grammar-fn
         grammar/process)))

#?(:clj
   (def model
     (atom (model/load "en" load-lexical-rules
                       load-lexicon fill-lexicon-indexes
                       load-morphology load-grammar))))

#?(:cljs
   (comment
   (def model
     (atom nil)))) ;; TODO: add call to macro function like with morphology/compile-morphology.

#?(:clj
   (defn write-compiled-grammar []
     (grammar/write-compiled-grammar (-> @model :grammar)
                                     "src/menard/english/grammar/compiled.edn")))

(defn morph
  ([tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (-> (u/get-in tree [:syntax-tree])
         (s/morph (:morphology @model))
         an)

     true
     (-> tree
         (s/morph (:morphology @model))
         an)))

  ([tree & {:keys [sentence-punctuation?]}]
   (if sentence-punctuation?
     (-> tree
         morph
         an
         (sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon (:lexicon @model)
                               "src/menard/english/lexicon/compiled.edn")))

#?(:clj
   (defn index-fn [spec]
     (let [result
           (cond (= (u/get-in spec [:cat]) :verb)
                 (-> @model :indices :verb-lexicon)

                 (and (u/get-in spec [:cat])
                      (not (= :top (u/get-in spec [:cat]))))
                 (-> @model :indices :non-verb-lexicon)

                 ;; TODO: make a :misc-lexicon index, as in nl.
                 true
                 (lazy-cat
                  (-> @model :indices :verb-lexicon)
                  (-> @model :indices :non-verb-lexicon)))]
       (if true
         (shuffle result)
         result))))

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

(defn syntax-tree [tree]
  (s/syntax-tree tree (:morphology @model)))

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
  (binding [g/max-depth (if (get-in spec [:max-depth])
                          (+ 3 (get-in spec [:max-depth]))
                          (get-in spec [:max-depth] g/max-depth))]
    (log/debug (str "english generate: " (diag/strip-refs spec)))
    (g/generate spec
                (-> @model :grammar)
                index-fn syntax-tree)))

(defn get-lexemes [spec]
  (g/get-lexemes spec index-fn syntax-tree))

(defn generate-n
  "generate _n_ consecutive in-order expressions that satisfy _spec_."
  [spec n]
  (take n (repeatedly #(generate spec))))

(defn analyze [surface]
  (binding [l/lexicon (-> @model :lexicon)
            l/morphology (-> @model :morphology)]
    (let [variants (vec (set [(clojure.string/lower-case surface)
                              (clojure.string/upper-case surface)
                              (clojure.string/capitalize surface)]))]
      (->> variants
           (mapcat (fn [surface]
                     (l/matching-lexemes surface)))))))

;; TODO: consider setting p/truncate? false here in (defn parse)
;; to improve performance:
(defn parse [expression]
  (binding [p/grammar (-> @model :grammar)
            p/syntax-tree syntax-tree
            l/lexicon (-> @model :lexicon)
            l/morphology (-> @model :morphology)
            p/lookup-fn analyze]
    (p/parse expression morph)))

(def expressions
  (-> "menard/english/expressions.edn"
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

