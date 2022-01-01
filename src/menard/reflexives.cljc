(ns menard.reflexives
  (:require [menard.lexiconfn :as l]
            [menard.model :as model]))

(def reflexive-options
  (l/read-and-eval (model/use-path "reflexives.edn")))
