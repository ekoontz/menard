(ns menard.english.woordenlijst
  (:require #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u]
            [menard.english.compile :refer [compile-lexicon]]
            [menard.lexiconfn :as l]
            [menard.model :refer [create]]
            [menard.parse :as p]
            [menard.serialization :as s]))

(def create-model? true)

(when create-model?
  (def en-model
    (ref (create "english/models/woordenlijst"
                 compile-lexicon)))
  (def nl-model
    (ref (create "nederlands/models/woordenlijst"
                 compile-lexicon))))

(defn syntax-tree [tree model]
  (let [model (deref model)]
    (s/syntax-tree tree (:morphology model))))

(defn analyze [surface model]
  (let [model (deref model)]
    (binding [l/lexicon (-> model :lexicon)
              p/syntax-tree syntax-tree
              l/morphology (:morphology model)]
      (let [variants (vec (set [(clojure.string/lower-case surface)
                                (clojure.string/upper-case surface)
                                (clojure.string/capitalize surface)]))
            found (mapcat l/matching-lexemes variants)]
        (log/info (str "analyze: found: " (count found) " for: [" surface "]"))
        (if (seq found)
          found
          (let [found (l/matching-lexemes "_")]
            (log/info (str "no lexemes found for: [" surface "]"
                           (when (seq found)
                             (str "; will use null lexemes instead."))))
            found))))))


(defn all-groupings [input-string model]
  (binding [p/lookup-fn (fn [input-string] (analyze input-string model))]
    (p/all-groupings input-string (fn [surface] (analyze surface model)))))


