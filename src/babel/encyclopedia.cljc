(ns babel.encyclopedia ^{:doc "real-world knowledge, expressed as a map of implications"}
  (:refer-clojure))

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
