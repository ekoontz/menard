(ns menard.english.woordenlijst
  (:require #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u]
            [menard.english.compile :refer [compile-lexicon]]
            [menard.lexiconfn :as l]
            [menard.model :refer [create]]
            [menard.parse :as p]
            [menard.serialization :as s]
            [menard.nederlands.tenses :as tenses]))

(def create-model? true)

;; TODO: s/en-model/model/
(when create-model?
  (def en-model
    (ref (create "english/models/woordenlijst"
                 "woordenlijst"
                 compile-lexicon))))
