(ns menard.english.tenses
  (:require [clojure.java.io :as io :refer [resource]]))

(def finite-tenses
  (-> "english/finite-tenses.edn" resource slurp read-string))

(def nonfinite-tenses
  (-> "english/nonfinite-tenses.edn" resource slurp read-string))

(def tenses
  (concat finite-tenses
          nonfinite-tenses))
