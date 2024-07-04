(ns menard
  (:require [dag_unify.core :as u]
            [menard.translate :as translate]))

(defn demo []
  (translate/demo))

(defn derivation-of [lexeme]
  (menard.lexiconfn/display-derivation (u/get-in lexeme [:menard.lexiconfn/derivation])))

(defn pprint [lexeme]
  (u/pprint (merge (dissoc lexeme :menard.lexiconfn/derivation)
                   {:derivation (derivation-of lexeme)})))
