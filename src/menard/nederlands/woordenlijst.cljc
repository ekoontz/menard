(ns menard.nederlands.woordenlijst
  (:require [menard.nederlands.compile :refer [compile-lexicon]]

            ;; This :require of menard.generate is needed
            ;; for some reason for the
            ;; menard.model/install-the-usual-suspects macro
            ;; to work: otherwise we get a:
            ;; 'Syntax error (ClassNotFoundException) compiling at (menard/nederlands/complete.cljc:12:1).'
            [menard.generate :as generate] 
            [menard.model :refer [create]]))

(def model
  (ref (create "nederlands/models/woordenlijst"
               "woordenlijst"
               compile-lexicon)))

(menard.model/install-the-usual-suspects model)
