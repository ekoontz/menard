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
        (dissoc :en)
        (dissoc :nl)
        (assoc :canonical nl)
        (assoc :cat :noun)
        (assoc :sem {:pred en})
        (assoc :curriculum :woordenlijst))))

(defn write-woordenlijst []
  (do
    (->> woordenlijst
         (map make-dutch)
         vec
         (spit "/Users/ekoontz/menard/resources/nederlands/lexicon/woordenlijst/nouns.edn")))
    (->> woordenlijst
         (map make-english)
         vec
         (spit "/Users/ekoontz/menard/resources/english/lexicon/woordenlijst/nouns.edn")))

(defn make-english [lexeme]
  (let [en (:en lexeme)
        nl (:nl lexeme)]
    (-> lexeme
        (dissoc :en)
        (dissoc :nl)
        (assoc :canonical en)
        (assoc :cat :noun)
        (assoc :sem {:pred en}))))

