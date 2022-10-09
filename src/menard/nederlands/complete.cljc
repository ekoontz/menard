(ns menard.nederlands.complete
  (:require [clojure.tools.logging :as log]

            ;; This :require of menard.generate is needed
            ;; for some reason for the
            ;; menard.model/install-the-usual-suspects macro
            ;; to work: otherwise we get a:
            ;; 'Syntax error (ClassNotFoundException) compiling at (menard/nederlands/complete.cljc:12:1).'
            [menard.generate :as generate] 

            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]
            [menard.parse :as parse]))

(def model
  (ref (create "nederlands/models/complete"
               "complete"
               compile-lexicon)))

(menard.model/install-the-usual-suspects model)
