;; TODO: we don't use much korma functionality here; might as well simply
;; use a clojure wrapper around JDBC.
;; TODO: rename this file - misleading name 'korma.clj'.
;; It uses korma, but it is not itself part of korma.
;; TODO: clean up and remove verbcoach-specific stuff 
;; (e.g. classes,students,etc).
(ns babel.korma
  (:refer-clojure :exclude [test update])
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [korma.db :refer [default-connection defdb postgres]]))

(require '[environ.core :refer [env]])

(defonce _direct_connection (atom nil))

(declare convert-keys-from-string-to-keyword)

(defn prepare-array
  "Convert a Clojure sequence into (some implementation of) java.sql.Array.

   e.g. convert [{:a 42}] 
        into: #object[org.postgresql.jdbc.PgArray 0x3d73de4b '{'{'a':42}'}']

  Uses an implementation of java.sql.Connection.createArrayOf provided
  by the database driver.  For example, if _direct_connection is to a
  PostgreSQL database via org.postgresql.jdbc, then the implementation
  type will be org.postgresql.jdbc.PgArray
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
  (let [sql-type-and-map-fn
        (cond (string? (first sequence))
              {:type "text"
               :map-fn (fn [x] x)}
              
              (map? (first sequence))
              {:type "jsonb"
               :map-fn json/write-str}

              true
              (do
                (log/warn (str "using default of 'text' as SQL type for"
                               " sequence of type:" (type sequence)
                               (if (not (empty? sequence))
                                 (str " whose first member is of type:"
                                      (type (first sequence))))
                               "."))
                {:type "text"
                 :map-fn (fn [x] x)}))
        sql-type (:type sql-type-and-map-fn)
        map-fn (:map-fn sql-type-and-map-fn)]
    (.createArrayOf
     @_direct_connection
     sql-type (into-array (map map-fn sequence)))))

(defn read-array [sql-array]
    "Convert a Clojure sequence into (some implementation of) java.sql.Array. 

     e.g. turn: #object[org.postgresql.jdbc.PgArray 0x3d73de4b '{{'a':42},{'b':43}}'] 
          into: {:a 42}.

  Uses an implementation of java.sql.Connection.createArrayOf provided
  by the database driver.  For example, if _direct_connection is to a
  PostgreSQL database via org.postgresql.jdbc, then the implementation
  type will be org.postgresql.jdbc.PgArray
    (https://jdbc.postgresql.org/development/privateapi/org/postgresql/jdbc/PgArray.html).
   An implementation of java.sql.Connection.createArrayOf
    (https://docs.oracle.com/javase/7/docs/api/java/sql/Connection.html)
   will be used to do the actual conversion."

  (->> (-> sql-array ;; (type = implementation of SqlArray (e.g. org.postgresql.jdbc.PgArray)
           .getArray ;; java array: (type %) = #object["Ljava.lang.String;" .."
           vec) ;; clojure.lang.PersistentVector

       (map #(try
               ;; if input elements are parseable as JSON, read them, each of which will be a Clojure map.
               ;; otherwise, return the input elements unmodified.
               (json/read-str %) ;; rely on json/read-str to throw an error if input is not JSON
               (catch Exception e %))) ;; % is not JSON-parseable: just return it as-is.

       ;; Input is now a sequence of Clojure-native elements such as maps, but, if an
       ;; element is a map, then its keys are strings.
       ;; Final step, convert the keys from strings e.g. convert "foo" to :foo.
       (map convert-keys-from-string-to-keyword)))

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

(defn convert-keys-from-string-to-keyword [input]
  (cond (map? input)
        (let [keys (map keyword
                        (keys input))
              vals (map convert-keys-from-string-to-keyword
                        (vals input))]
          (zipmap keys vals))
        true input))

