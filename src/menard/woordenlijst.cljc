(ns menard.woordenlijst
  (:require [dag_unify.core :as u :refer [unify]]))

(def woordenlijst
  (-> "/Users/ekoontz/menard/woordenlijst-nouns.edn"
      slurp
      read-string))

(defn make-dutch [lexeme]
  (let [en (:en lexeme)
        nl (:nl lexeme)]
    (-> lexeme
        (assoc :canonical nl)
        (assoc :sem {:pred en})
        (dissoc :en)
        (dissoc :nl))))


(defn make-english [lexeme]
  (let [en (:en lexeme)
        nl (:nl lexeme)]
    (-> lexeme
        (assoc :canonical en)
        (assoc :sem {:pred en})
        (dissoc :en)
        (dissoc :nl))))

        




