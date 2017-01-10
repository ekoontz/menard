;; TODO: we don't use much korma functionality here; might as well simply
;; use a clojure wrapper around JDBC.
;; TODO: rename this file - misleading name 'korma.clj'.
;; It uses korma, but it is not itself part of korma.
;; TODO: clean up and remove verbcoach-specific stuff 
;; (e.g. classes,students,etc).
(ns babel.korma
  (:refer-clojure :exclude [test update])
  (:require [clj-time.coerce :as c]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [dag_unify.core :refer (unify fail?)]
            [korma.db :refer [default-connection defdb postgres]]))

(require '[environ.core :refer [env]])

(defonce _direct_connection (atom nil))

(defn prepare-array
  "Convert an array into (some implementation of) java.sql.Array. 
   For example, if _direct_connection is to a PostgreSQL database via org.postgresql.jdbc, 
   then the implementation type will be org.postgresql.jdbc.PgArray 
    (https://jdbc.postgresql.org/development/privateapi/org/postgresql/jdbc/PgArray.html).
   An implementation of java.sql.Connection.createArrayOf
    (https://docs.oracle.com/javase/7/docs/api/java/sql/Connection.html)
   will be used to do the actual conversion."
  [sequence]
  (if (nil? @_direct_connection)
    (reset! _direct_connection
            (-> korma.db/_default deref
                :pool deref
                :datasource .getConnection)))
  ;; infer type of input sequence based on first element, if any.
  (let [sql-type
        (cond (= java.lang.String (type (first sequence)))
              "text"

              ;; TODO: support other types, in particular, JSON.
              true
              (do
                (log/warn (str "using default of 'text' as seql type for"
                               " sequence of type:" (type sequence)
                               (if (not (empty? sequence))
                                 (str " whose first member is of type:"
                                      (type (first sequence))))
                               "."))
                "text"))]
    (.createArrayOf
     @_direct_connection
     sql-type (into-array sequence))))

(defn init-db []
  (defdb korma-db 
    (let [default "postgres://postgres@localhost:5432/babel"
          database-url (cond
                         (env :database-url)
                         (env :database-url)
                         
                         true default
                         true
                         (do
                           (log/error
                            (str "DATABASE_URL not set in your environment: you must define it; e.g.: " default)
                            (throw (Exception. (str "could not find database name in your database-url."))))))]
      ;; this constructs the actual database connection which is used throughout the code base.
      (postgres
       ;; thanks to Jeroen van Dijk via http://stackoverflow.com/a/14625874
       (let [[_ user password host port db]
             (re-matches #"postgres://(?:([^:]+):?(.*)@)?([^:]+)(?::(\d+))?/(\S+).*"
                         database-url)
             
             redacted-database-url
             (if (and password (not (empty? password)))
               (string/replace database-url
                               (str ":" password)
                               ":******")
               database-url)
             ]
         (if (nil? db)
           (throw (Exception. (str "could not find database name in your database-url: '"
                                   database-url "'"))))
         
         (log/info (str "scanned DATABASE_URL:" redacted-database-url "; found:"
                        "(user,host,db)=(" user "," host "," db ")"))
;         (reset! _direct_connection
;                 (.getConnection (:datasource (korma.db/get-connection korma-db))))
         
         {:db db
          :user user
          :password password
          :host host
          :port (or port "5432")})))))

  
  
  
