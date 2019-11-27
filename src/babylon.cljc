(ns babylon
  (:require [babylon.nederlands :as nl]
            [babylon.english :as en]
            [babylon.translate :as translate]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u :refer [pprint unify]]))

(defn demo []
  (translate/demo))
