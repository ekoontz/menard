(ns babylon.english.morphology)


(def rules
  (reduce
   concat
   [(-> (str "babylon/english/nouns.edn")
        clojure.java.io/resource
        slurp
        read-string)
    (-> (str "babylon/english/verbs.edn")
        clojure.java.io/resource
        slurp
        read-string)]))

