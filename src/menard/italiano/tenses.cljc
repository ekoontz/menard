(ns menard.italiano.tenses
  (:require [clojure.java.io :as io :refer [resource]]))

(def finite-nonaux-tenses
  (-> "italiano/finite-nonaux-tenses.edn" resource slurp read-string))

(def finite-aux-tenses
  (-> "italiano/finite-aux-tenses.edn" resource slurp read-string))

(def finite-tenses
  (concat finite-nonaux-tenses finite-aux-tenses))

(def tenses finite-tenses)

