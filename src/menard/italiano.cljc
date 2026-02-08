(ns menard.italiano
  (:require [menard.italiano.tenses :refer [tenses]]
            [menard.generate :as g]
            [menard.lexiconfn :as l]
            [menard.model :refer [create load-model]]
            [menard.morphology :refer [morph-leaf]]
            [menard.parse :as p]
            [menard.serialization :as s]
            [dag_unify.core :as u :refer [unify]]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn model-fn []
  (create "italiano/models/basic"
          "basic"

          ;; compile-lexicon-fn
          (fn [lexicon _ _] lexicon)

          ;; filter-out-nils?
          true

          {:include-derivation? false}
          ;; change to true to  ^^^^^
          ;;show derivation.
          ))

(def closed-class
  (ref (create "italiano/models/closed-class"
               "closed-class"

               ;; compile-lexicon-fn
               (fn [lexicon _ _] lexicon)

               ;; filter-out-nils?
               true
               {:include-derivation? false}
               ;; change to true to  ^^^^^
               ;;show derivation.
               )))

(def curated-verbs
  (ref (create "italiano/models/curated-verbs"
               "curated-verbs"

          ;; compile-lexicon-fn
               (fn [lexicon _ _] lexicon)

               ;; filter-out-nils?
               true

               {:include-derivation? false}
               ;; change to true to  ^^^^^
               ;;show derivation.
               )))

(def model
  (ref (model-fn)))

(defn reload []
  (dosync (ref-set model
                   (model-fn)))
  (type @model))

(defn morph [tree]
   (cond
     ;; inflection doesn't work right, so disabling
     (and false

          (map? (u/get-in tree [:syntax-tree])))
     (morph (u/get-in tree [:syntax-tree]))

     :else
     (s/morph tree (:morphology @model))))

#?(:clj
   (defn syntax-tree [tree & [options]]
     (if (= options :morph)
       (morph tree)
       (s/syntax-tree tree (-> model deref :morphology)))))

(defn generate [spec & [input-model]]
  (let [model (or input-model model)
        model @model
        retval
        (binding [g/max-depth (:max-depth spec g/max-depth)
                  g/max-fails (:max-fails spec g/max-fails)
                  g/allow-backtracking? true]
          (-> spec
              (g/generate (-> model :grammar)
                          (-> model :lexicon-index-fn)
                          syntax-tree)))]
    (log/debug (str "generate: generated: " (-> retval syntax-tree)))
    retval))

(defn generate-all [spec & [input-model]]
  (let [model (or input-model model)
        model @model
        retval
        (binding [g/max-depth (:max-depth spec g/max-depth)
                  g/max-fails (:max-fails spec g/max-fails)
                  g/allow-backtracking? true]
          (g/generate-all [spec]
                          (-> model :grammar)
                          (-> model :lexicon-index-fn)
                          syntax-tree))]
    retval))

;; for parsing diagnostics: set to false to prevent space optimization (truncating trees):
(def truncate? true)

;; how to split up a string into tokens that can be analyzed:
(def split-on #"[ ]+")

(defn analyze
  ([surface]
   (analyze surface false))
  ([surface use-null-lexemes?]
   (analyze surface false model))
  ([surface use-null-lexemes? model]
   (log/debug (str "analyze with model named: " (-> model :name) "; morphology size: " (count (vec (:morphology model)))))
   (let [variants (vec (set [(clojure.string/lower-case surface)
                             (clojure.string/upper-case surface)
                             (clojure.string/capitalize surface)
                             surface]))
         model @model
         lexicon (-> model :lexicon)
         morphology (:morphology model)
         found (mapcat (fn [variant]
                         (l/matching-lexemes variant lexicon morphology))
                       variants)]
     (log/debug (str "found: " (count found) " for: [" surface "]"))
     (if (seq found)
       found
       (if (and use-null-lexemes?
                (not (clojure.string/includes? surface " ")))
         (let [found (l/matching-lexemes "_" lexicon morphology)]
           (log/debug (str "no lexemes found for: [" surface "]"
                           (when (seq found)
                             (str "; will use null lexemes instead."))))
           found))))))

(defn parse
  ([expression model]
   (let [;; remove trailing '.' if any:
         expression (clojure.string/replace expression #"[.]*$" "")

         ;; meaning of 1st arg passed to (analyze):
         ;; true: allow use of null lexemes
         analyze-fn-with-nulls #(analyze % true model)
         ;; false: DON'T allow use of null lexemes
         analyze-fn-without-nulls #(analyze % false model)

         model @model

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
   (parse expression model)))


(defn get-grammar [rule-name]
  (->> (-> model deref :grammar)
       (filter #(= rule-name (:rule %)))))

(defn get-lexicon [lexeme]
  (get (-> model deref :lexicon) lexeme))

(defn get-indices []
  (-> model deref :indices))
