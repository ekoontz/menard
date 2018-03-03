(ns babel.lexicon_reader
  (:require
   [babel.korma :refer [init-db]]
   [clojure.tools.logging :as log]
   [korma.core :refer [exec-raw]]))

(defn read-lexicon [language]
  (init-db)
  (log/info (str "reading lexicon for language: " language))
  (let [lexemes
        (-> (exec-raw [(str "SELECT canonical, serialized FROM lexeme "
                            " WHERE language=?")
                       [language]]
                      :results))]
    (log/info (str "found: " (count lexemes) " lexemes for language: " language " in database."))
    (zipmap
     (map :canonical lexemes)
     (map (fn [lexeme]
            (->> (-> lexeme
                     :serialized
                     .getArray
                     vec)
                 (map read-string)
                 (map dag_unify.core/deserialize)))
          lexemes))))
