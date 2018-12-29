(ns babylon.english.morphology)


(def rules
  (concat
   [(-> "babylon/english/nouns.edn"
        clojure.java.io/resource
        slurp
        read-string)
    (-> "babylon/english/verbs.edn"
        clojure.java.io/resource
        slurp
        read-string)]))

