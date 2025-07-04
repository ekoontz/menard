(ns menard.español.tenses
  (:require [clojure.java.io :as io :refer [resource]]
            [dag_unify.core :refer [unify]]))

(def finite-nonaux-tenses
  (->> (-> "español/finite-nonaux-tenses.edn" resource slurp read-string)
       (map #(unify % {:cat :verb}))))

(def finite-aux-tenses
  (->> (-> "español/finite-aux-tenses.edn" resource slurp read-string)
       (map #(unify % {:cat :verb}))))       

(def finite-tenses
  (concat finite-nonaux-tenses finite-aux-tenses))

(def tenses finite-tenses)

