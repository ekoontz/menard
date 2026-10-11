(ns menard.italiano
  "Italiano language module (JVM only), mirroring menard.español's structure:
   loads menard.italiano.tenses (whose symbols are referenced from
   resources/italiano/grammar.edn :options) and provides the model plus
   morph/syntax-tree/analyze/generate/parse wrappers."
  #?(:clj (:require [menard.generate :as g]
                    [menard.italiano.tenses]
                    [menard.lexiconfn :as l]
                    [menard.model :refer [create]]
                    [menard.morphology :refer [morph-leaf]]
                    [menard.parse :as p]
                    [menard.serialization :as s]
                    [clojure.string :as string]
                    #?(:clj [clojure.tools.logging :as log]))))

#?(:clj
   (defn model-fn []
     (create "italiano/models/basic"
             "basic"
             ;; compile-lexicon-fn
             (fn [lexicon _ _] lexicon)
             ;; filter-out-nils?
             true
             {:include-derivation? false})))

#?(:clj (def model (ref (model-fn))))

#?(:clj
   (defn reload []
     (dosync (ref-set model (model-fn)))
     (type @model)))

#?(:clj
   (defn morph [tree]
     (s/morph tree (:morphology @model))))

#?(:clj
   (defn syntax-tree [tree & [options]]
     (if (= options :morph)
       (morph tree)
       (s/syntax-tree tree (-> model deref :morphology)))))

;; for parsing diagnostics: set to false to prevent space optimization (truncating trees):
#?(:clj (def truncate? true))

;; how to split up a string into tokens that can be analyzed:
#?(:clj (def split-on #"[ ]+"))

#?(:clj
   (defn preprocess
     "arbitrary replacements to convert Italian orthography into a
      whitespace-delimited expression."
     [input]
     (let [processed (string/join " "
                                  (map string/lower-case
                                       (-> input
                                           (string/replace #"," "")
                                           (string/replace #"\." "")
                                           (string/replace #"\s+" " ")
                                           (string/split #" "))))]
       (log/debug (str "preprocess: " input " -> " processed))
       processed)))

#?(:clj
   (defn analyze
     ([surface] (analyze surface false))
     ([surface use-null-lexemes?] (analyze surface use-null-lexemes? model))
     ([surface use-null-lexemes? model]
      (let [variants (vec (set [(string/lower-case surface)
                                (string/upper-case surface)
                                (string/capitalize surface)
                                surface]))
            m @model
            lexicon (:lexicon m)
            morphology (:morphology m)
            found (mapcat (fn [variant] (l/matching-lexemes variant lexicon morphology))
                          variants)]
        (if (seq found)
          found
          (when (and use-null-lexemes?
                     (not (string/includes? surface " ")))
            (l/matching-lexemes "_" lexicon morphology)))))))

#?(:clj
   (defn generate
     ([spec] (generate spec model))
     ([spec input-model]
      (let [m (or input-model model)
            m @m]
        (binding [g/max-depth (:max-depth spec g/max-depth)
                  g/max-fails (:max-fails spec g/max-fails)
                  g/allow-backtracking? true]
          (-> spec
              (g/generate (:grammar m) (:lexicon-index-fn m) syntax-tree)))))))

#?(:clj
   (defn parse
     ([expression] (parse expression model))
     ([expression model]
      (let [expression (string/replace expression #"[.]*$" "")
            analyze-fn-with-nulls #(analyze % true model)
            analyze-fn-without-nulls #(analyze % false model)
            m @model
            grammar (:grammar m)]
        (log/debug (str "calling p/parse with grammar: " (count grammar)))
        (let [parses-without-nulls
              (->> (p/parse expression grammar analyze-fn-without-nulls
                            syntax-tree morph split-on truncate?)
                   (filter :complete?))]
          (if (seq parses-without-nulls)
            parses-without-nulls
            (let [parses-with-nulls
                  (->> (p/parse expression grammar analyze-fn-with-nulls
                                syntax-tree morph split-on truncate?)
                       (filter :complete?))]
              (if (seq parses-with-nulls)
                parses-with-nulls
                (->> (p/parse (str expression " _") grammar analyze-fn-with-nulls
                              syntax-tree morph split-on truncate?)
                     (filter :complete?))))))))))
