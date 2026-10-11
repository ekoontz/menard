(ns menard.italiano.tenses
  #?(:clj (:require [clojure.java.io :as io :refer [resource]])))

;; Tense-variant menus for grammar rules' :options (JVM only).
(def finite-tenses
  #?(:clj (-> "italiano/finite-tenses.edn" resource slurp read-string)
     :cljs nil))

(def tenses finite-tenses)
