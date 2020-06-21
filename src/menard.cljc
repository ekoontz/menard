(ns menard
  (:require [menard.nederlands :as nl]
            [menard.english :as en]
            [menard.translate :as translate]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]))

(defn demo []
  (translate/demo))
