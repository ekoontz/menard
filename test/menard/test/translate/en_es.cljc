(ns menard.test.translate.en-es
  (:require [dag_unify.core :as u]
            [menard.english :as en]
            [menard.español :as es]
            [menard.translate :as translate]
            [clojure.test :refer [deftest is]]))

(deftest transfer-1
  (-> "yo quiero" translate/es-to-en (= "I want") is))

