(ns menard.reload
  (:require [clojure.tools.logging :as log]))

(defn reload [rules-fn lexicon-fn]
  (let [rules-atom (atom nil)
        lexicon-atom (atom nil)]
    (reset! rules-atom (rules-fn))
    (log/info (str "loaded: " (count @rules-atom) " lexical rule sets."))
    (reset! lexicon-atom (lexicon-fn rules-atom))
    (log/info (str "loaded: " (count (keys @lexicon-atom)) " lexeme keys."))
    {:rules rules-atom
     :lexicon lexicon-atom}))




