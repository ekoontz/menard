(ns menard.reflexives
  (:require [menard.lexiconfn :refer [read-and-eval]]
            [menard.model :refer [use-path]]))

(def reflexive-options
  (read-and-eval (use-path "reflexives.edn")))
