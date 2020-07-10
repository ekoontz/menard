(ns menard.model
  (:require [clojure.tools.logging :as log]))

(defn reload [rules-fn lexicon-fn fill-lexicon-indexes-fn]
  (let [rules-atom (atom nil)
        lexicon-atom (atom nil)
        indices-atom (atom nil)]
    (reset! rules-atom (rules-fn))
    (log/info (str "loaded: " (count @rules-atom) " lexical rule sets."))
    (reset! lexicon-atom (lexicon-fn rules-atom))
    (log/info (str "loaded: " (count (keys @lexicon-atom)) " lexeme keys."))
    (reset! indices-atom (fill-lexicon-indexes-fn @lexicon-atom))
    (log/info (str "loaded: " (count (keys @indices-atom)) " lexicon indices."))
    {:rules rules-atom
     :indices indices-atom
     :lexicon lexicon-atom}))
