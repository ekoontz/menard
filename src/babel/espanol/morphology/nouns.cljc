(ns babel.espanol.morphology.nouns
  (:refer-clojure :exclude [get-in merge resolve])
  (:require [clojure.string :as string]
            [clojure.string :refer (trim)]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log])
            [dag_unify.core :refer (copy dissoc-paths fail? get-in merge ref? strip-refs unifyc)]))


