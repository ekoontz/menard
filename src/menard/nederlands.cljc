(ns menard.nederlands

  ;; TODO: don't we need to have a :require-macros
  ;; for menard.morphology, too?
  #?(:cljs (:require-macros [menard.grammar]))

  (:require [clojure.string :as string]
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            [menard.generate :as g]
            [menard.grammar :as grammar]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [menard.model :as model]
            [menard.morphology :as m]
            [menard.nesting]
            [menard.parse :as p]
            [menard.serialization :as s]
            [menard.subcat]
            [menard.ug]
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :as diag]
            [dag_unify.serialization :refer [deserialize]]))
;;
;; For generation and parsing of Dutch.
;;

;; <morphology>
#?(:clj
   (defn load-morphology []
     (m/compile-morphology-fn
      [(model/use-path "nederlands/morphology/adjectives.edn")
       (model/use-path "nederlands/morphology/misc.edn")
       (model/use-path "nederlands/morphology/nouns.edn")
       (model/use-path "nederlands/morphology/verbs.edn")])))

#?(:cljs
   (defn load-morphology []
     (m/compile-morphology
      ["nederlands/morphology/adjectives.edn"
       "nederlands/morphology/misc.edn"
       "nederlands/morphology/nouns.edn"
       "nederlands/morphology/verbs.edn"])))

;; </morphology>

;; <lexicon>

#?(:clj
   (defn load-lexical-rules []
     [(l/read-and-eval (model/use-path "nederlands/lexicon/rules/rules-0.edn"))
      (l/read-and-eval (model/use-path "nederlands/lexicon/rules/rules-1.edn"))
      (l/read-and-eval (model/use-path "nederlands/lexicon/rules/rules-2.edn"))
      (l/read-and-eval (model/use-path "nederlands/lexicon/rules/rules-3.edn"))]))

#?(:clj
   (defn compile-lexicon-source [source-filename lexical-rules & [unify-with apply-fn]]
     (binding [menard.lexiconfn/include-derivation? true]
       (-> source-filename
           l/read-and-eval
           ((fn [lexicon]
              (l/apply-to-every-lexeme lexicon
                                       (fn [lexeme]
                                         (if (nil? unify-with)
                                           lexeme
                                           (let [result (unify lexeme unify-with)]
                                             (if (= :fail result)
                                               (exception (str "hit a fail while proccessing source filename: " source-filename "; lexeme: " lexeme "; unify-with: " unify-with)))
                                             result))))))
           ((fn [lexicon]
              (l/apply-to-every-lexeme lexicon
                                       (fn [lexeme]
                                         (if (nil? apply-fn)
                                           lexeme
                                           (apply-fn lexeme))))))
           l/add-exceptions-to-lexicon
           (l/apply-rules-in-order (nth lexical-rules 0) :0)
           (l/apply-rules-in-order (nth lexical-rules 1) :1)
           (l/apply-rules-in-order (nth lexical-rules 2) :2)
           (l/apply-rules-in-order (nth lexical-rules 3) :3)))))

#?(:clj
   (defn get-inflection-of [lexeme morphology]
     (when lexeme
       (->> morphology
            (map (fn [rule]
                   {:u (reduce unify
                               [lexeme (:u rule)
                                {:cat :noun
                                 :exception false
                                 :agr {:number :plur}}])
                    :m (re-find (-> rule :g first)
                                (:canonical lexeme))}))
            (filter (fn [x] (and (not (= :fail (:u x)))
                                 (not (nil? (:m x))))))
            (map (fn [result]
                   (-> result :u :inflection)))
            first))))

#?(:clj
   (defn mark-irregular-verbs [lexeme]
     (if (seq (->> (u/get-in lexeme [:exceptions])
                   (filter #(= :past-simple (u/get-in % [:infl])))))
       (unify lexeme {:irregular-past-simple? true})
       (unify lexeme {:irregular-past-simple? false}))))

#?(:clj
   (defn load-lexicon [lexical-rules]
     (merge-with concat
                 (compile-lexicon-source (model/use-path "nederlands/lexicon/adjectives.edn")   lexical-rules
                                         {:cat :adjective})
                 (compile-lexicon-source (model/use-path "nederlands/lexicon/adverbs.edn")      lexical-rules
                                         {:cat :adverb})
                 (compile-lexicon-source (model/use-path "nederlands/lexicon/determiners.edn")  lexical-rules
                                         {:cat :det})
                 (compile-lexicon-source (model/use-path "nederlands/lexicon/exclamations.edn") lexical-rules
                                         {:cat :exclamation})
                 (compile-lexicon-source (model/use-path "nederlands/lexicon/intensifiers.edn") lexical-rules
                                         {:cat :intensifier})
                 ;; misc has various :cat values, so can't supply a :cat for this part of the lexicon:
                 (compile-lexicon-source (model/use-path "nederlands/lexicon/misc.edn")         lexical-rules)

                 (compile-lexicon-source (model/use-path "nederlands/lexicon/nouns.edn")        lexical-rules
                                         {:cat :noun})
                 (compile-lexicon-source (model/use-path "nederlands/lexicon/numbers.edn")      lexical-rules
                                         {:cat :adjective})
                 (compile-lexicon-source (model/use-path "nederlands/lexicon/pronouns.edn")     lexical-rules
                                         {:cat :noun :pronoun true})
                 (compile-lexicon-source (model/use-path "nederlands/lexicon/propernouns.edn")  lexical-rules
                                         {:cat :noun :propernoun true})
                 (compile-lexicon-source (model/use-path "nederlands/lexicon/verbs.edn")        lexical-rules
                                         {:cat :verb} mark-irregular-verbs))))

#?(:clj

   (defn load-lexicon-with-morphology [lexicon morphology-rules]
     (-> lexicon
         (l/apply-to-every-lexeme
          (fn [lexeme]
            (let [inflection (get-inflection-of lexeme morphology-rules)]
              (if (and (= :noun (u/get-in lexeme [:cat]))
                       (not (= true (u/get-in lexeme [:propernoun])))
                       (not (= true (u/get-in lexeme [:pronoun]))))
                (cond
                  inflection
                  (unify lexeme
                         {:inflection inflection})

                  (and (= :noun (u/get-in lexeme [:cat]))
                       (not (= true (u/get-in lexeme [:propernoun])))
                       (not (= true (u/get-in lexeme [:pronoun])))
                       (false? (u/get-in lexeme [:inflected?] false)))
                  (do
                    (log/warn (str "no inflection found for lexeme: "
                                   (u/get-in lexeme [:canonical])))
                    lexeme)
                
                  :else lexeme)
                lexeme))))

         ;; The lexicon is a map where each
         ;; key is a canonical string
         ;; and each value is the list of lexemes for
         ;; that string. we turn the list into a vec
         ;; so that it's completely realized rather than
         ;; a lazy sequence, so that when we periodically
         ;; reload the model from disk, (generate) or
         ;; (parse) won't have to de-lazify the list:
         ;; it will already be done before they (generate or
         ;; parse) see it.
         ;; (TODO: move this to some function within
         ;;  menard/model).
         ((fn [lexicon]
            (zipmap (keys lexicon)
                    (map (fn [vs]
                           (vec vs))
                         (vals lexicon))))))))

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

(def finite-tenses
  (-> "nederlands/finite-tenses.edn" resource slurp read-string))

(def inf-tense
  (-> "nederlands/infinite-tense.edn" resource slurp read-string))

(def finite-plus-inf-tense
  (concat finite-tenses
          inf-tense))

#?(:clj
   (defn load-grammar []
     (-> (model/use-path "nederlands/grammar.edn")
         grammar/read-grammar-fn
         grammar/process)))


#?(:cljs
   (def model
     (atom nil))) ;; TODO: add call to macro function like with morphology/compile-morphology.

#?(:clj
   (defn create-model []
     (log/info (str "creating model for Nederlands.."))
     (model/load "nl"
                 load-lexical-rules
                 (fn [lexical-rules]
                   (load-lexicon-with-morphology
                    (load-lexicon lexical-rules)
                    (load-morphology)))
                 fill-lexicon-indexes
                 load-morphology load-grammar)))

#?(:clj
   (def model (ref (create-model))))

#?(:clj
   (defn load-model []
     (dosync
      (when (nil? @model)
        (ref-set model (create-model)))
      @model)))

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
                 
                 :else (-> @model :indices :misc-lexicon))
           spec (if true spec (u/copy (diag/strip-refs spec)))
           result (if true
                    (->>
                     pre-result
                     (filter #(not (true? (u/get-in % [:null?])))))
                    (->> pre-result
                         (filter #(not (true? (u/get-in % [:null?]))))                         
                         (map #(unify % spec))
                         (filter #(not (= :fail %)))))]
       (if true
         (shuffle result)
         result))))

#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon (:lexicon @model)
                               "src/nederlands/lexicon/compiled.edn")))

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

(declare sentence-punctuation)

(defn morph
  ([tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (s/morph (u/get-in tree [:syntax-tree]) (:morphology @model))

     :else
     (s/morph tree (:morphology @model))))

  ([tree & {:keys [sentence-punctuation?]}]
   (when sentence-punctuation?
     (-> tree
         morph
         (sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

#?(:cljs
   (def grammar
     (->> (menard.grammar/read-compiled-grammar
           "menard/nederlands/grammar/compiled.edn")
          (map deserialize))))

#?(:clj
   (defn write-compiled-grammar []
     (grammar/write-compiled-grammar (-> @model :grammar)
                                     "src/menard/nederlands/grammar/compiled.edn")))
(declare generate)
(declare syntax-tree)

(def expressions
  (->> (-> "nederlands/expressions.edn"
           grammar/read-expressions)))

;; <functions>

(defn syntax-tree [tree]
  (s/syntax-tree tree (:morphology (load-model))))

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec]
  (let [model (load-model)]
    (binding [g/max-depth (:max-depth spec g/max-depth)
              g/allow-backtracking? true]
      (-> spec
          ((fn [x] (unify x (:training-wheels x :top))))
          (dissoc :training-wheels)
          (g/generate (-> model :grammar) index-fn syntax-tree)))))

(defn generate-all
  "generate all expressions that satisfy _spec_."
  [spec]
  (let [model (load-model)]
    (binding [] ;;  g/stop-generation-at [:head :comp :head :comp]
      (g/generate-all [spec] (-> model :grammar) index-fn syntax-tree))))

(defn analyze [surface]
  (let [model (load-model)]
    (binding [l/lexicon (-> model :lexicon)
              p/syntax-tree syntax-tree
              l/morphology (:morphology model)]
      (let [variants (vec (set [(clojure.string/lower-case surface)
                                (clojure.string/upper-case surface)
                                (clojure.string/capitalize surface)]))
            found (mapcat l/matching-lexemes variants)]
        (log/debug (str "found: " (count found) " for: [" surface "]"))
        (if (seq found)
          found
          (let [found (l/matching-lexemes "_")]
            (log/info (str "no lexemes found for: [" surface "]"
                           (when (seq found)
                             (str "; will use null lexemes instead."))))
            found))))))

(defn parse [expression]
  (let [model (load-model)]
    (binding [p/grammar (-> model :grammar)
              p/syntax-tree syntax-tree
              p/truncate? false
              l/lexicon (-> model :lexicon)
              l/morphology (-> model :morphology)
              p/split-on #"[ ]"
              p/lookup-fn analyze]
      (p/parse expression morph))))

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

(defn sample [spec n]
  (->> #(-> spec generate morph)
       repeatedly (take n) set vec sort (map println)
       count))
