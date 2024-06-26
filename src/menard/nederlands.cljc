(ns menard.nederlands

  ;; TODO: don't we need to have a :require-macros
  ;; for menard.morphology, too?
  #?(:cljs (:require-macros [menard.grammar]))

  (:require [clojure.core.async :refer [go-loop]]
            [clojure.string :as string]
            [config.core :refer [env]]
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            [menard.generate :as g]
            [menard.grammar :as grammar]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [menard.model :refer [load-model]]
            [menard.morphology :as m]
            [menard.nederlands.tenses :as tenses]

            ;; models
            [menard.nederlands.basic :as basic]
            [menard.nederlands.complete :as complete]

            [menard.nesting]
            [menard.parse :as p]
            [menard.punctuation :refer [sentence-punctuation]]
            [menard.serialization :as s]
            [menard.subcat]
            [menard.ug]
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [deserialize]]))
;;
;; For generation and parsing of Dutch.
;;

;; for parsing diagnostics:
(def truncate? true)

;; how to split up a string into tokens that can be analyzed:
(def split-on #"[ ]+")

#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon (:lexicon @complete/model)
                               "resources/nederlands/lexicon/compiled.edn")))

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

(defn morph
  ([tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (s/morph (u/get-in tree [:syntax-tree]) (:morphology @complete/model))

     :else
     (s/morph tree (:morphology @complete/model))))

  ([tree & {:keys [sentence-punctuation?]}]
   (when sentence-punctuation?
     (-> tree
         morph
         (sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

#?(:cljs
   (def grammar
     (->> (menard.grammar/read-compiled-grammar
           "resources/nederlands/grammar/compiled.edn")
          (map deserialize))))

#?(:clj
   (defn write-compiled-grammar []
     (grammar/write-compiled-grammar (-> @complete/model :grammar)
                                     "resources/nederlands/grammar/compiled.edn")))
(declare generate)
(declare syntax-tree)

(def expressions
  (->> (-> "nederlands/expressions.edn"
           grammar/read-expressions)))

;; <functions>

#?(:clj
   (defn syntax-tree [tree & [model]]
     (s/syntax-tree tree (:morphology (or model complete/model (load-model complete/model))))))

#?(:cljs
   (defn syntax-tree [tree]
     (s/syntax-tree tree [])))

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec model]
  (let [model (or model complete/model (load-model complete/model))
        model (cond (= (type model) clojure.lang.Ref)
                    @model
                    (map? model)
                    model

                    :else
                    (exception (str "invalid model: " model)))
        name (-> model :spec :name)]
    (if name
      (log/debug (str "menard.nederlands/generate: generating with model named: " name))
      (log/warn (str "generating with model with no name, but has keys: " (keys model)
                     " and maybe a spec? " (:spec model))))

    ;; TODO: these bindings will go away soon.
    (let [retval
          (binding [g/max-depth (:max-depth spec g/max-depth)
                    g/max-fails (:max-fails spec g/max-fails)
                    g/allow-backtracking? true]
            (-> spec
                ((fn [x] (unify x (:training-wheels x :top))))
                (dissoc :training-wheels)
                (g/generate (-> model :grammar)
                            (-> model :lexicon-index-fn)
                            syntax-tree)))]
      (log/debug (str "menard.nederlands/generate: generated: " (-> retval syntax-tree)))
      retval)))

(defn generate-all
  "generate all expressions that satisfy _spec_."
  [spec]
  (let [model (load-model complete/model)]
    (binding [] ;;  g/stop-generation-at [:head :comp :head :comp]
      (g/generate-all [spec]
                      (-> model :grammar)
                      (-> model :lexicon-index-fn)
                      syntax-tree))))

(defn analyze
  ([surface]
   (analyze surface false))
  ([surface use-null-lexemes?]
   (analyze surface false @complete/model))
  ([surface use-null-lexemes? model]
   (log/debug (str "analyze with model named: " (-> model :name)))
   (let [variants (vec (set [(clojure.string/lower-case surface)
                             (clojure.string/upper-case surface)
                             (clojure.string/capitalize surface)]))
         lexicon (-> model :lexicon)
         morphology (:morphology model)
         found (mapcat (fn [variant]
                         (l/matching-lexemes variant lexicon morphology))
                       variants)]
     (log/debug (str "found: " (count found) " for: [" surface "]"))
     (if (seq found)
       found
       (if (and use-null-lexemes?
                (not (string/includes? surface " ")))
         (let [found (l/matching-lexemes "_" lexicon morphology)]
           (log/debug (str "no lexemes found for: [" surface "]"
                           (when (seq found)
                             (str "; will use null lexemes instead."))))
           found))))))

(defn resolve-model [model]
  (cond (= (type model) clojure.lang.Ref) @model
        (map? model)                      model
        :else                             (exception (str "invalid model: " model))))

(defn parse
  ([expression model]
   (let [model (resolve-model model)
         ;; remove trailing '.' if any:
         expression (string/replace expression #"[.]*$" "")

         ;; meaning of 1st arg passed to (analyze):
         ;; true: allow use of null lexemes
         analyze-fn-with-nulls #(analyze % true model)
         ;; false: DON'T allow use of null lexemes
         analyze-fn-without-nulls #(analyze % false model)

         ;; ^ TODO: should handle '.' and other punctuation like '?' '!' and
         ;; use it as part of the meaning
         ;; i.e.
         ;; '.' -> declarative
         ;; '?' -> interrogative
         ;; '!' -> imperative
         grammar (-> model :grammar)]
     (log/debug (str "calling p/parse with grammar: " (count grammar)))
     (let [parses-without-nulls
           (->> (p/parse expression grammar analyze-fn-without-nulls
                         syntax-tree morph split-on truncate?)
                (filter :complete?))]
       (log/debug (str "was without-null parses empty? " (empty? parses-without-nulls)))
       (if (seq parses-without-nulls)
         parses-without-nulls

         ;; else if no results ,try *with* nulls:
         (let [parses-with-nulls
               (->> (p/parse expression grammar analyze-fn-with-nulls
                             syntax-tree morph split-on truncate?)
                    (filter :complete?))]
           (log/debug (str "was with-null parses empty? " (empty? parses-with-nulls)))
           (if (seq parses-with-nulls)
             parses-with-nulls
             (let [parses-with-null-appended
                   (->> (p/parse (str expression " _") grammar analyze-fn-with-nulls
                                 syntax-tree morph split-on truncate?)
                        (filter :complete?))]
               (log/debug (str "was with-null-appended parses empty? " (empty? parses-with-null-appended)))
               parses-with-null-appended)))))))
  ([expression]
   (parse expression (load-model complete/model))))

(defn parse-starts-with-no-nulls [parse-starts]
  (->> parse-starts
       (filter (fn [parse-start]
                 (empty? (->> (vals parse-start)
                              (filter #(nil? %))))))))

(defn parse-start [expression & [model]]
  (let [model (or model (load-model complete/model))
        model (resolve-model model)

        ;; remove trailing '.' if any:
        expression (string/replace expression #"[.]*$" "")
        ;; ^ TODO: should handle '.' and other punctuation like '?' '!' and
        ;; use it as part of the meaning
        ;; i.e.
        ;; '.' -> declarative
        ;; '?' -> interrogative
        ;; '!' -> imperative

        lookup-fn-without-nulls
        (fn [token]
          (log/debug (str "menard.nederlands/parse-start: looking up: " token))
          (analyze token false model))
        lookup-fn-with-nulls
        (fn [token]
          (log/debug (str "menard.nederlands/parse-start: looking up: " token))
          (analyze token true model))
        without-nulls (p/parse-start expression split-on lookup-fn-without-nulls)]
    (if (empty? (parse-starts-with-no-nulls without-nulls))
      ;; if not all 'words' could be analyzed (i.e. found in the lexicon),
      ;; then try again to analyze, which will
      ;; analyze unknown words as null lexemes as provided by lookup-fn-with-nulls.
      (do
        (log/debug (str "some unknown lexemes in: '" expression "'; trying again allowing nulls.."))
        (p/parse-start expression split-on lookup-fn-with-nulls))
      without-nulls)))

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
  (binding [g/fold? true]
    (->>
     (repeatedly #(println (-> (nth expressions index)
                               generate
                               time
                               ((fn [x] (morph x :sentence-punctuation? true))))))
     (take (or this-many 10))
     count)))

(defn tokenize [input-string]
  (p/tokenize input-string split-on analyze))

(defn roundtrip [input]
  (-> input
      parse
      first
      ((fn [input-expression]
         (let [spec {:sem (u/get-in input-expression [:sem] :top)
                     :mod (u/get-in input-expression [:mod] :top)
                     :phrasal? (u/get-in input-expression [:phrasal?] :top)
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

;; example usage:
;;
;; (->> "slapen honden"
;;      parse
;;      (map (ugins [:infl] [:sem] [:head :sem][:subcat]))
;;      (map u/pprint))
;;
;; => parses with selected parts shown
;;
(defn ugin
  ([& paths]
   (fn [arg1]
     (reduce dag_unify.core/unify
             (map (fn [path]
                    (dag_unify.serialization/create-path-in path (u/get-in arg1 path)))
                  paths)))))

(defn parse-all [expression & [model]]
  (let [model (or model (load-model complete/model))
        model (cond (= (type model) clojure.lang.Ref) @model
                    (map? model)                      model
                    :else                             (exception (str "invalid model: " model)))
        lookup-fn (fn [token] (analyze token false model))]
    (p/parse-all expression (:grammar model) syntax-tree lookup-fn truncate?)))

(defn round-trip-demo [& [spec times]]
  (let [spec (or spec
                 {:phrasal? true
                         :head {:phrasal? false}
                  :comp {:phrasal? false}})
        times (or times 10)]
    (->> (repeatedly #(-> spec
                          generate
                          time
                          morph
                          parse
                          first
                          time
                          syntax-tree
                          println))
         (take times)
         count)))

;; (->> "mij zien" parse (map #(u/get-in % [:head-derivation])) (map menard.lexiconfn/encode-derivation))
