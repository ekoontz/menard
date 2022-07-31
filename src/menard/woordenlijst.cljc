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

(defn make-english [lexeme]
  (let [en (:en lexeme)
        nl (:nl lexeme)]
    (-> lexeme
        (dissoc :en)
        (dissoc :nl)
        (assoc :canonical en)
        (assoc :cat :noun)
        (assoc :sem {:pred en}))))

(defn write-woordenlijst []
  (let [dutch-items
        (->> woordenlijst
             (map make-dutch)
             vec)
        english-items
        (->> woordenlijst
             (map make-english)
             vec)]
    (spit "/Users/ekoontz/menard/resources/nederlands/lexicon/woordenlijst/nouns.edn"
          (zipmap
           (map :canonical dutch-items)
           (map (fn [lexeme]
                  [(dissoc lexeme :canonical)])
                dutch-items)))
    (spit "/Users/ekoontz/menard/resources/english/lexicon/woordenlijst/nouns.edn"
          (zipmap
           (map :canonical english-items)
           (map (fn [lexeme]
                  [(dissoc lexeme :canonical)])
                english-items)))))


