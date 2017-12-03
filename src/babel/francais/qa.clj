(ns babel.francais.qa
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.francais :refer [analyze generate parse]]
   [babel.francais.grammar :refer [medium]]
   [babel.korma :as korma]
   [babel.francais.morphology :refer [fo]]
   [babel.parse :as parse]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [dag_unify.core :as unify :refer [deserialize get-in ref? strip-refs unify]]
   [korma.core :as db]))

;; DELETE FROM expression WHERE id IN (SELECT id FROM (SELECT id,surface FROM expression WHERE language='fr' AND structure->'synsem'->'sem'->'subj'->>'pred' = 'Juan') AS foo);
;; DELETE FROM expression WHERE id IN (SELECT id FROM (SELECT id,surface FROM expression WHERE language='fr' AND surface ILIKE '% aimir%') AS foo);
;; DELETE FROM expression WHERE id IN (SELECT id FROM (SELECT id,surface FROM expression WHERE language='fr' AND surface ILIKE '% appelir%') AS foo);
;; DELETE FROM expression WHERE id IN (SELECT id FROM (SELECT id,surface FROM expression WHERE language='fr' AND surface ILIKE '% apportir%') AS foo);
;; DELETE FROM expression WHERE id IN (SELECT id FROM (SELECT id,surface FROM expression WHERE language='fr' AND surface ILIKE '% assurir%') AS foo);

(def total-errors (atom 0))
(def total (atom 0))

(defn read-expressions [& [this-many]]
  (let [this-many (if this-many (Integer. this-many))
        map (if this-many map pmap)
        results (db/exec-raw [(str "SELECT target.serialized::text AS target,target.surface
                                      FROM expression AS target
                                     WHERE target.language=?
                                  ORDER BY target.surface")
                              ["fr"]]
                             :results)]
    (count
     (map (fn [result]
            (let [structure (deserialize (read-string (:target result)))
                  surface (:surface result)]
              (log/info (str "surface: " surface))
              (let [parsed (parse surface)]
                (swap! total (fn [current] (+ 1 current)))
                (if (= (count parsed) 0)
                  (do (log/error (str "could not parse: '" surface "' with"
                                      " root: " (get-in structure [:root :français :français])
                                      " and tense: " (get-in structure [:synsem :sem :tense])
                                      " and infl: " (get-in structure [:synsem :infl])))
                      (let [spec
                            {:synsem {:infl (get-in structure [:synsem :infl])
                                      :agr (get-in structure [:synsem :agr])
                                      :sem {:tense :past
                                            :subj (get-in structure [:synsem :sem :subj])
                                            :pred (get-in structure [:synsem :sem :pred])}}
                             :root {:français {:français (get-in structure [:root :français :français])}}}]
                        (if false (log/error (str "we think it should be:'"
                                        (fo (generate spec
                                                      )
                                            )
                                        "'."
                                        )
                                   )))
                      (swap! total-errors (fn [current] (+ 1 current)))
                      (log/error (str "errors so far: " @total-errors " out of "
                                      @total "; error rate=" (string/replace (str (/ @total-errors (+ 0.0 @total)) "00")
                                                                       #"^(....).*" "$1"))))
                  (log/info (str "parse results: " (count parsed)))))))
          (if this-many
            (take this-many (shuffle results))
            results)))
    (log/info (str "final error ratio:" @total-errors " out of "
                   @total "; error rate=" (string/replace (str (/ @total-errors (+ 0.0 @total)) "00")
                                                          #"^(....).*" "$1")))))






    


