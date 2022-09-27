(ns menard.english

  ;; TODO: don't we need to have a :require-macros
  ;; for menard.morphology, too?
  #?(:cljs (:require-macros [menard.grammar]))

  (:require [clojure.string :as string]

            ;; models
            [menard.english.complete :as complete]
            
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
;; For generation and parsing of English.
;;

;; <morphology>
#?(:clj
   (defn load-morphology []
     (m/compile-morphology-fn
      [(model/use-path "english/morphology/misc.edn")
       (model/use-path "english/morphology/nouns.edn")
       (model/use-path "english/morphology/verbs.edn")])))

#?(:cljs
   (defn load-morphology []
     (m/compile-morphology
      ["english/morphology/misc.edn"
       "english/morphology/nouns.edn"
       "english/morphology/verbs.edn"])))

;; </morphology>

;; <lexicon>

#?(:clj
   (defn load-lexical-rules []
     (l/read-and-eval (model/use-path "english/lexicon/rules.edn"))))

#?(:clj
   (defn compile-lexicon-source [source-filename lexical-rules & [unify-with]]
     (binding [menard.lexiconfn/include-derivation? false]
       (-> source-filename
           l/read-and-eval
           ((fn [lexicon]
              (l/apply-to-every-lexeme lexicon
                                       (fn [lexeme]
                                         (if (nil? unify-with)
                                           lexeme
                                           (unify lexeme unify-with))))))
           l/add-exceptions-to-lexicon
           (l/apply-rules-in-order lexical-rules)))))

#?(:clj
   (defn load-lexicon [lexical-rules]
     (->
      (merge-with concat
                  (compile-lexicon-source (model/use-path "english/lexicon/adjectives.edn")   lexical-rules {:cat :adjective})
                  (compile-lexicon-source (model/use-path "english/lexicon/adverbs.edn")      lexical-rules {:cat :adverb})
                  (compile-lexicon-source (model/use-path "english/lexicon/exclamations.edn") lexical-rules {:cat :exclamation})
                  (compile-lexicon-source (model/use-path "english/lexicon/intensifiers.edn") lexical-rules {:cat :intensifier})
                  ;; misc has various :cat values, so can't supply a :cat for this part of the lexicon:
                  (compile-lexicon-source (model/use-path "english/lexicon/misc.edn")         lexical-rules)
                  (compile-lexicon-source (model/use-path "english/lexicon/nouns.edn")        lexical-rules {:cat :noun})
                  (compile-lexicon-source (model/use-path "english/lexicon/numbers.edn")      lexical-rules {:cat :adjective})
                  (compile-lexicon-source (model/use-path "english/lexicon/pronouns.edn")     lexical-rules {:cat :noun
                                                                                                             :pronoun? true
                                                                                                             :propernoun? false})
                  (compile-lexicon-source (model/use-path "english/lexicon/propernouns.edn")  lexical-rules {:cat :noun
                                                                                                             :pronoun? false
                                                                                                             :propernoun? true})
                  (compile-lexicon-source (model/use-path "english/lexicon/verbs.edn")        lexical-rules {:cat :verb}))
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
                        (if true (vec vs)
                            vs))
                      (vals lexicon))))))))

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

#?(:cljs
   (def loaded-grammar
     (->> (menard.grammar/read-compiled-grammar
           "english/grammar/compiled.edn")
          (map deserialize))))

;; </grammar>

#?(:clj
   (defn load-grammar []
     (->>
      (-> (model/use-path "english/grammar.edn")
          grammar/read-grammar-fn
          grammar/process))))

#?(:clj
   (defn create-model []
     (log/info (str "creating model for English.."))
     (-> (model/load "en" load-lexical-rules
                     load-lexicon
                     load-morphology load-grammar {:name "default"})
         (merge {:name "default"})
         (model/add-functions))))

#?(:clj
   (def model (ref (create-model))))

#?(:cljs
   (def model
     (atom nil))) ;; TODO: add call to macro function like with morphology/compile-morphology.

#?(:clj
   (defn write-compiled-grammar []
     (grammar/write-compiled-grammar (-> @model :grammar)
                                     "resources/english/grammar/compiled.edn")))

#?(:clj
   (defn load-model []
     (dosync
      (when (nil? @model)
        (ref-set model (create-model))))
     @model))

(defn morph
  ([tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (-> (u/get-in tree [:syntax-tree])
         (s/morph (:morphology @model))
         an)

     :else
     (-> tree
         (s/morph (:morphology @model))
         an)))

  ([tree & {:keys [sentence-punctuation?]}]
   (when sentence-punctuation?
     (-> tree
         morph
         an
         (sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon (:lexicon @model)
                               "resources/english/lexicon/compiled.edn")))

#?(:cljs
   (def lexicon
     (-> (l/read-compiled-lexicon "english/lexicon/compiled.edn")
         l/deserialize-lexicon              
         vals
         flatten)))

#?(:clj
   (defn index-fn [spec]
     ((-> model deref :lexicon-index-fn) spec)))

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

(defn tokenize [input-string]
  (let [split-on #"[ ]"]
    (p/tokenize split-on input-string)))

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
  [spec & [model]]
  ;; should block on this until a model exists: maybe @model should be a future
  ;; or a promise (not sure what the difference is).
  (log/debug (str "menard.english/generate with spec: " spec))
  (let [model (or model complete/model (load-model))
        model (cond (= (type model) clojure.lang.Ref)
                    @model
                    (map? model)
                    model
                    
                    :else
                    (let [error-message (str "menard.english/generate: invalid model: " model)]
                      (log/error error-message)
                      (exception error-message)))]
    (binding [g/max-depth (if (get-in spec [:max-depth])
                            (+ 5 (get-in spec [:max-depth]))
                            (get-in spec [:max-depth] g/max-depth))]
      (log/debug (str "english generate: " (diag/strip-refs spec)
                      " with max-depth: " g/max-depth))
      (g/generate spec
                  (-> model :grammar)
                  (-> model :lexicon-index-fn) syntax-tree))))

(defn generate-n
  "generate _n_ consecutive in-order expressions that satisfy _spec_."
  [spec n]
  (take n (repeatedly #(generate spec))))

(defn analyze [surface & [model]]
  (let [model (or model (load-model))]
    (binding [l/lexicon (-> model :lexicon)
              l/morphology (-> model :morphology)]
      (let [variants (vec (set [(clojure.string/lower-case surface)
                                (clojure.string/upper-case surface)
                                (clojure.string/capitalize surface)]))]
        (->> variants
             (mapcat (fn [surface]
                       (l/matching-lexemes surface))))))))

;; TODO: consider setting p/truncate? false here in (defn parse)
;; to improve performance:
(defn parse [expression & [model]]
  (let [model (or model (load-model))
        model (cond (= (type model) clojure.lang.Ref)
                    @model
                    (map? model)
                    model
                    
                    :else
                    (let [error-message (str "menard.english/parse: invalid model: " model)]
                      (log/error error-message)
                      (exception error-message)))
        split-on #"[ ]"]
    (if (map? model)
      (log/info (str "menard.english/parse with model name: "
                     (-> model :name)))
      (let [error-message (str "menard.english/parse: model is not a map as expected!")]
        (log/error error-message)
        (exception error-message)))
    (binding [l/lexicon (-> model :lexicon)
              l/morphology (-> model :morphology)]
      (let [truncate? true]
        (->
         expression
         (p/all-groupings split-on (fn [token] (analyze token model)))
         (p/parse (-> model :grammar) #(analyze % model) syntax-tree morph truncate?))))))

(defn parse-start [expression & [model]]
  (let [model (or model (load-model))

        ;; remove trailing '.' if any:
        expression (string/replace expression #"[.]*$" "")
        ;; ^ TODO: should handle '.' and other punctuation like '?' '!' and
        ;; use it as part of the meaning
        ;; i.e.
        ;; '.' -> declarative
        ;; '?' -> interrogative
        ;; '!' -> imperative

        split-on #"[ ]"]
    (binding [l/morphology (-> model :morphology)]
      (p/parse-start expression split-on analyze))))

(def expressions
  (-> "english/expressions.edn"
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

(defn parse-all [expression]
  (p/parse-all expression load-model syntax-tree #"[ ]" analyze))
