(ns menard.español.tenses
  (:require [clojure.java.io :as io :refer [resource]]))

(def finite-tenses
  (-> "español/finite-tenses.edn" resource slurp read-string))

(def tenses
  finite-tenses)
