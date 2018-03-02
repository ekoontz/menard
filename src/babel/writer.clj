(ns babel.writer
  (:refer-clojure :exclude [get-in])
  (:require
    [babel.encyclopedia :refer [sem-impl]]
    [babel.generate :refer [generate]]
    [babel.korma :as korma]
    [babel.log :refer [log4j!]]
    [clojure.data.json :as json]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [clojure.tools.namespace.repl :refer [refresh refresh-all]]
    [dag_unify.core :refer [fail? get-in remove-top-values strip-refs serialize unify ref?]]
    [korma.core :refer [exec-raw]]
    [korma.db :refer [transaction]]))

(defn insert-lexeme [canonical lexemes language]
  (log/debug (str "insert-lexeme: canonical=" canonical ", language=" language))
  (exec-raw [(str "INSERT INTO lexeme 
                                 (canonical, structures, language, serialized)
                          VALUES (?,?::jsonb[],?,?::text[])")
             [canonical
              (babel.korma/prepare-array
               (vec (map (fn [lexeme]
                           (json/write-str (dissoc (strip-refs lexeme) :dag_unify.core/serialized)))
                         lexemes)))
              language
              (babel.korma/prepare-array
               (vec (map (fn [lexeme]
                           (str (vec (dag_unify.core/serialize lexeme))))
                         lexemes)))
              ]]))

(defn truncate-lexicon [language]
  (try
    (exec-raw [(str "DELETE FROM lexeme WHERE language=?")
               [language]])
    (catch java.sql.SQLException sqle
      (do
        (log/error (str "Exception when truncating lexicon: " sqle))
        (log/error (str "SQL error: " (.printStackTrace (.getNextException sqle))))))
      
    (catch Exception e
      (do
        (log/error (str "Exception when truncating lexicon: " e))))))

;; (babel.writer/write-lexicon "en" lexicon)
(defn write-lexicon [language lexicon]
  (babel.korma/init-db)

  (transaction
   (truncate-lexicon language)
   
   (let [canonicals (sort (keys lexicon))]
     (loop [remain canonicals
            result 0]
       (let [[canonical & remaining] remain]
         (log/debug (str "(write-lexicon " language ":" canonical ")"))
         (insert-lexeme canonical (get lexicon canonical) language)
         (if (empty? remaining)
           (+ 1 result)
           (recur remaining
                  (+ 1 result))))))))
