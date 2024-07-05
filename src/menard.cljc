(ns menard
  (:require [dag_unify.core :as u]
            [menard.translate :as translate]))

(defn demo []
  (translate/demo))

(defn derivation-of [lexeme]
  (menard.lexiconfn/display-derivation (u/get-in lexeme [:menard.lexiconfn/derivation])))

(defn pprint [lexeme]
  (let [derivation (u/get-in lexeme [:menard.lexiconfn/derivation])
        head-derivation (u/get-in lexeme [:head-derivation])
        comp-derivation (u/get-in lexeme [:comp-derivation])]
    (-> lexeme
        (dissoc :menard.lexiconfn/derivation)
        (dissoc :head-derivation)
        (dissoc :comp-derivation)        
        ((fn [lexeme]
           (merge
            lexeme
            (cond (seq derivation)
                  {:derivation (menard.lexiconfn/display-derivation derivation)}
                  (seq head-derivation)
                  {:head-derivation (menard.lexiconfn/display-derivation head-derivation)}
                  (seq comp-derivation)
                  {:comp-derivation (menard.lexiconfn/display-derivation comp-derivation)}))))
        u/pprint)))
