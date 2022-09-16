(ns menard.woordenlijst
  (:require [dag_unify.core :as u :refer [unify]]
            [clojure.tools.logging :as log]))

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
             (map (fn [lexeme]
                    (let [nl (:nl lexeme)]
                      (cond (re-find #"., de$" nl)
                            (->
                             lexeme
                             (assoc :nl (clojure.string/replace
                                         nl
                                         #"^(.*), de$" "$1"))
                             (assoc :cat :noun))

                            (re-find #"., het$" nl)
                            (->
                             lexeme
                             (assoc :nl (clojure.string/replace
                                         nl
                                         #"^(.*), het$" "$1"))
                             (assoc :agr {:gender :neuter})
                             (assoc :cat :noun))
                            
                            ;; throw away lexemes that don't match
                            ;; any rules
                            :else nil))))
             (filter seq)
             (map make-dutch)
             vec)
        english-items
        (->> woordenlijst
             (map make-english)
             vec)]
    (log/info (str "writing out.."))

    (clojure.pprint/pprint
     (zipmap (map :canonical dutch-items)
             (map (fn [lexeme]
                    [(dissoc lexeme :canonical)])
                  dutch-items))
     (clojure.java.io/writer 
      "/Users/ekoontz/menard/resources/nederlands/lexicon/woordenlijst/nouns.edn"))

    (clojure.pprint/pprint
     (zipmap (map :canonical english-items)
             (map (fn [lexeme]
                    [(dissoc lexeme :canonical)])
                  english-items))
     (clojure.java.io/writer 
      "/Users/ekoontz/menard/resources/english/lexicon/woordenlijst/nouns.edn"))))






