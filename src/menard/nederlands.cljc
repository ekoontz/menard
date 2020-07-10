(ns menard.nederlands
  #?(:cljs (:require-macros [menard.grammar]))
  (:require [clojure.string :as string]
            [menard.lexiconfn :as l]
            [menard.generate :as g]
            [menard.grammar :as grammar]
            [menard.morphology :as m]
            [menard.parse :as p]
            [menard.nesting :as nest]
            [menard.model :as model]
            [menard.serialization :as s]
            [menard.ug :as ug]
            [menard.subcat :as su]            
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [pprint unify]]
            [dag_unify.diagnostics :as diag :refer [fail-path]]))
;;
;; For generation and parsing of Dutch.
;;

;; <lexicon>

#?(:clj
   (defn load-lexical-rules []
     [(l/read-and-eval "menard/nederlands/lexicon/rules/rules-0.edn")
      (l/read-and-eval "menard/nederlands/lexicon/rules/rules-1.edn")
      (l/read-and-eval "menard/nederlands/lexicon/rules/rules-2.edn")
      (l/read-and-eval "menard/nederlands/lexicon/rules/rules-3.edn")]))

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
                 (compile-lexicon-source "menard/nederlands/lexicon/adjectives.edn"
                                         lexical-rules {:cat :adjective})
                 (compile-lexicon-source "menard/nederlands/lexicon/adverbs.edn"
                                         lexical-rules {:cat :adverb})

                 ;; misc has various :cat values, so can't supply a :cat for this part of the lexicon:
                 (compile-lexicon-source "menard/nederlands/lexicon/misc.edn" lexical-rules)
                 
                 (compile-lexicon-source "menard/nederlands/lexicon/nouns.edn" lexical-rules {:cat :noun})
                 (compile-lexicon-source "menard/nederlands/lexicon/numbers.edn" lexical-rules {:cat :adjective})
                 (compile-lexicon-source "menard/nederlands/lexicon/propernouns.edn" lexical-rules {:cat :noun :pronoun false :propernoun true})
                 (compile-lexicon-source "menard/nederlands/lexicon/verbs.edn" lexical-rules {:cat :verb}))))

#?(:clj
  (defn fill-lexicon-indexes [lexicon]
    (let [flattened-lexicon (flatten (vals lexicon))]
      {:adjective-lexicon
       (->> flattened-lexicon
            (filter #(and (not (u/get-in % [:exception]))
                          (= (u/get-in % [:cat]) :adjective))))
       :det-lexicon
       (->> flattened-lexicon
            (filter #(and (not (u/get-in % [:exception]))
                          (= (u/get-in % [:cat]) :det))))
       
       :noun-lexicon
       (->> flattened-lexicon
            (filter #(and (not (u/get-in % [:exception]))
                          (= (u/get-in % [:cat]) :noun))))
       
       :misc-lexicon
       (->> flattened-lexicon
            (filter #(and (not (= (u/get-in % [:cat]) :verb))
                          (not (= (u/get-in % [:cat]) :adjective))
                          (not (= (u/get-in % [:cat]) :det))
                          (not (= (u/get-in % [:cat]) :noun))
                          (not (u/get-in % [:exception])))))
       
       :verb-lexicon
       (->> flattened-lexicon
            (filter #(and (not (u/get-in % [:exception]))
                          (= (u/get-in % [:cat]) :verb))))})))

#?(:clj
   (def model
     (atom (model/load "nl" load-lexical-rules load-lexicon fill-lexicon-indexes))))

#?(:clj
   (defn load-model []
     (reset! model 
             (model/load "nl" load-lexical-rules load-lexicon fill-lexicon-indexes))
     "loaded: " (keys @model)))

#?(:cljs
   (def lexicon
     (-> (l/read-compiled-lexicon "menard/nederlands/lexicon/compiled.edn")
         l/deserialize-lexicon              
         vals
         flatten)))

#?(:clj
   (defn index-fn [spec]
     (log/debug (str "spec: " (diag/strip-refs spec)))
     (let [pre-result
           (cond (= (u/get-in spec [:cat]) :verb)
                 (-> @model :indices :verb-lexicon)

                 (= (u/get-in spec [:cat]) :adjective)
                 (-> @model :indices :adjective-lexicon)

                 (= (u/get-in spec [:cat]) :noun)
                 (-> @model :indices :noun-lexicon)

                 (= (u/get-in spec [:cat]) :det)
                 (-> @model :indices :det-lexicon)
                 
                 true (-> @model :indices :misc-lexicon))
           spec (if true spec (u/copy (diag/strip-refs spec)))
           result (if true
                    pre-result
                    (->> pre-result
                         (map #(unify % spec))
                         (filter #(not (= :fail %)))))]
       (if true
         (shuffle result)
         result))))

#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon (:lexicon @model)
                               "src/menard/nederlands/lexicon/compiled.edn")))

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

(def morphology-atom
  (atom
   (m/compile-morphology
    ["menard/nederlands/morphology/adjectives.edn"
     "menard/nederlands/morphology/misc.edn"
     "menard/nederlands/morphology/nouns.edn"
     "menard/nederlands/morphology/verbs.edn"])))
  
(def morphology @morphology-atom)

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
    :infl :te
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
    :infl :infinitive
    :sem {:tense :infinitive}}])

#?(:clj
   (def grammar-atom
     (atom
      (-> "menard/nederlands/grammar.edn"
          grammar/read-grammar
          (grammar/process true)))))

#?(:clj
   (def grammar @grammar-atom))

#?(:cljs
   (def grammar
     (->> (menard.grammar/read-compiled-grammar
           "menard/nederlands/grammar/compiled.edn")
          (map dag_unify.serialization/deserialize))))

#?(:clj
   (defn write-compiled-grammar []
     (grammar/write-compiled-grammar grammar
                                     "src/menard/nederlands/grammar/compiled.edn")))

;; </grammar>

(declare generate)
(declare syntax-tree)

(def expressions
  (->> (-> "menard/nederlands/expressions.edn"
           grammar/read-expressions)))

;; <functions>

(defn syntax-tree [tree & [path]]
  (s/syntax-tree tree morphology))

(def ^:dynamic grammar-for-generation grammar)

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec]
  (binding [g/max-depth (:max-depth spec g/max-depth)
            g/allow-backtracking? true]
    (-> spec
        ((fn [x] (unify x (:training-wheels x :top))))
        (dissoc :training-wheels)
        (g/generate grammar-for-generation index-fn syntax-tree))))

(defn generate-all
  "generate all expressions that satisfy _spec_."
  [spec]
  (binding [] ;;  g/stop-generation-at [:head :comp :head :comp]
    (g/generate-all [spec] grammar index-fn syntax-tree)))

(defn analyze [surface]
  (binding [l/lexicon (-> @model :lexicon)
            l/morphology morphology]
    (let [variants (vec (set [(clojure.string/lower-case surface)
                              (clojure.string/upper-case surface)
                              (clojure.string/capitalize surface)]))]
      (->> variants
           (mapcat (fn [surface]
                     (l/matching-lexemes surface)))))))

(defn parse [expression]
  (binding [p/grammar grammar
            p/syntax-tree syntax-tree
            p/truncate? false
            l/lexicon (-> @model :lexicon)
            l/morphology morphology
            p/split-on #"[ ]"
            p/lookup-fn analyze]
    (p/parse expression morph)))

(defn generate-demo [index & [this-many]]
  (->>
   (repeatedly #(println (-> (nth expressions index)
                             generate
                             ((fn [x] (morph x :sentence-punctuation? true))))))
   (take (or this-many 10))
   count))

(defn demo []
  (->> (range 0 (count expressions))
       (map #(do
               (println (str % ": " (-> expressions (nth %) :note)))
               (generate-demo %)
               (println)))
       count))

(defn parse-demo [index & [this-many]]
  (->>
   (repeatedly #(println (-> (nth expressions index)
                             generate
                             morph
                             parse
                             time
                             first
                             ((fn [x] (syntax-tree x))))))
   (take (or this-many 10))
   count))

(defn demo-with-pruning [index & [this-many]]
  (binding [g/fold? true
            g/truncate? true]
    (->>
     (repeatedly #(println (-> (nth expressions index)
                               generate
                               time
                               ((fn [x] (morph x :sentence-punctuation? true))))))
     (take (or this-many 10))
     count)))

(defn sentence-punctuation
  "Capitalizes the first letter and puts a period (.) or question mark (?) at the end."
  [input mood]
  (str (string/capitalize (first input))
       (subs input 1 (count input))
       (if (= mood :interog)
         "?"
         ".")))

(defn roundtrip [input]
  (-> input
      parse 
      first
      ((fn [input-expression]
         (let [spec {:sem (u/get-in input-expression [:sem] :top)
                     :mod (u/get-in input-expression [:mod] :top)
                     :phrasal (u/get-in input-expression [:phrasal] :top)
                     :subcat (-> input-expression (u/get-in [:subcat] []))
                     :cat (u/get-in input-expression [:cat] :top)}
               generated (-> spec generate)]
           {:input-expression (-> input-expression syntax-tree)
            :input-spec spec
            :readability-divider "--------------------------------"
            :generated-expression (-> generated syntax-tree)
            :generated (-> generated morph)
            :output-structure {:sem (-> generated (u/get-in [:sem] ::unspec))
                               :cat (-> generated (u/get-in [:cat] ::unspec))
                               :mod (-> generated (u/get-in [:mod] ::unspec))}})))
      u/pprint))


(defn map-get [path input]
  (->>
   input
   (map (fn [x]
          {:tree (syntax-tree x)
           :path path
           :v (u/get-in x path)}))
   (map u/pprint)))
