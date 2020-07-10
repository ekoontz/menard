(ns menard.reload
  (:require [clojure.tools.logging :as log]))

(defn reload [rules-fn]
  (let [rules-atom (atom nil)]
    (reset! rules-atom (rules-fn))
    (log/info (str "loaded: " (count @rules-atom) " lexical rule sets."))
    {:rules rules-atom}))



