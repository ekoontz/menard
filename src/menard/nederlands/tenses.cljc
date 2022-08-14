(ns menard.nederlands.tenses
  (:require [clojure.java.io :as io :refer [resource]]))

(def finite-tenses
  (-> "nederlands/finite-tenses.edn" resource slurp read-string))

(def inf-tense
  (-> "nederlands/infinitive-tense.edn" resource slurp read-string))

(def finite-plus-inf-tense
  (concat finite-tenses
          inf-tense))

