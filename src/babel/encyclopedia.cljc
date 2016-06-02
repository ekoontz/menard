(ns babel.encyclopedia
  ^{:doc "real-world knowledge, expressed
as a map of implications"}
  (:refer-clojure)
  (:require
   [babel.exception :refer [exception]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])))

(def encyc
  {{:pred :house} {:artifact true
                   :buyable true
                   :place true}
   {:animate true} {:living true}
   {:human true} {:animate true}
   {:living true} {:artifact false}
   {:pet true} {:animate true
                :buyable true
                :edible false}})

(def animal {:animate true
             :artifact false
             :living true})
