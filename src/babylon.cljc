(ns babylon
  (:require [babylon.nederlands :as nl]
            [babylon.english :as en]
            [babylon.translate :as translate]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.diagnostics :refer [pprint]]
            [dag_unify.core :as u :refer [unify]]))

(defn demo []
  (translate/demo))
