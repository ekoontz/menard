(ns babel.lexicon_reader
  (:require
   [babel.korma :refer [init-db]]
   [babel.writer :refer [write-lexicon]]
   [clojure.data.json :as json :refer [read-str]]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [korma.core :refer [exec-raw]]))

(declare json-read-str)
(declare json-value-converter)

(defn coalesce-into-map
  "take a list of maps and return a map keyed by the keys."
  [keys values])

(defn read-lexicon [language]
  (init-db)
  (log/info (str "reading lexicon for language: " language))
  (let [lexemes
        (-> (exec-raw [(str "SELECT canonical, serialized FROM lexeme "
                            " WHERE language=?")
                       [language]]
                      :results))]
    (log/info (str "found: " (count lexemes) " lexemes for language : " language " in database."))
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

(defn json-read-str [json]
  (json/read-str json
                 :key-fn keyword
                 :value-fn json-value-converter))

(defn json-value-converter [k v]
  (println (str "k: " k))
  (println (str "v: " v))
  (cond
    (and (or (= k :english)
             (= k :espanol)
             (= k :français)
             (= k :italiano)
             (= k "english")
             (= k "espanol")
             (= k "français")
             (= k "italiano")
             (= k "latin")
             (= k :participle)
             (= k :past)
             (= k :past-participle)
             (= k :present)
             (= k :1sing)
             (= k :2sing)
             (= k :3sing)
             (= k :note))
         (not (map? v)))
    (str v)
    
    (and (string? v)
         (= (nth v 0) \:))
    (keyword (string/replace-first v ":" ""))
    
    (string? v)
    (keyword v)

    (= (type v)
       org.postgresql.jdbc.PgArray)
    (vec (.getArray v))

    
    :else v))
