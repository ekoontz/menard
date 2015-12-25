(ns babel.espanol.morphology.nouns
  (:refer-clojure :exclude [get-in merge resolve]))

(require '[babel.stringutils :refer :all])
(require '[clojure.core :as core])
(require '[clojure.string :as string])
(require '[clojure.string :refer (trim)])
(require '[clojure.tools.logging :as log])
(require '[dag_unify.core :refer (copy dissoc-paths fail? get-in merge ref? strip-refs unifyc)])

