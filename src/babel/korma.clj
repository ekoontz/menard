;; TODO: rename this file - misleading name 'korma.clj'.
;; It uses korma, but it is not itself part of korma.
(ns babel.korma
  (:refer-clojure :exclude [test])
  (:require [clj-time.coerce :as c]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [dag-unify.core :refer (unify fail?)]
            [korma.core :refer :all]
            [korma.db :refer [default-connection defdb postgres]]))

(require '[environ.core :refer [env]])

(defdb korma-db 
  (let [default "postgres://postgres@localhost:5432/babel"
        database-url (cond
                      (env :database-url)
                      (env :database-url)
                      
                      true
                      (do
                        (log/warn
                         (str "DATABASE_URL not set in your environment: defaulting to:" default))
                        default))]
    ;; this constructs the actual database connection which is used throughout the code base.
    (postgres
     ;; thanks to Jeroen van Dijk via http://stackoverflow.com/a/14625874
     (let [[_ user password host port db]
           (re-matches #"postgres://(?:([^:]+):?(.*)@)?([^:]+)(?::(\d+))?/(.+)"
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
             {:db db
              :user user
              :password password
              :host host
              :port (or port "5432")}))))

;; TODO: remove (or move) everything below here: not being used anywhere.

;; http://sqlkorma.com/docs#entities
;; TODO: move to verb.clj or similar: model-type stuff.
;; ^^ WTF, this comment does not make sense..nothing to do with verbs in here.

(declare authentication-codes classes
         question question-submit 
         students-in-class
         student-test tsubmit user verb vgroup)

;; TODO: this 'defentity' stuff is perfunctory
;; boilerplate: remove or at least move to bottom of file
;; where it doesn't hog valuable real estate.
;; (Have to learn how/if defentity creates tables.
;; For now I just use sql (../sql/create.sql)
;;  to create tables).
(defentity authentication-codes
  (table :authentication_codes)
  (pk :id))

(defentity classes
  (pk :id)
  (has-many students-in-class {:fk :class}))

(defentity queue
  (table :queue)
  (pk :id))

(defentity question
  (pk :id)
  (belongs-to student-test {:fk :test})
  (entity-fields :english :italian))

(defentity question-submit
  (table :qsubmit)
  (pk :id)
  (entity-fields :answer :test-submit :question))

(defentity student-test
  (table :test)
  (pk :id)
  (has-many question))

(defentity students-in-class
  (table :students_in_classes)
  (pk :id))

(defentity test-submit
  (table :tsubmit)
  (pk :id)
  (has-many question-submit)
  (entity-fields :test :student))

(defentity user
  (table :vc_user)
  (pk :id))

(defentity verb
  (pk :id)
  (entity-fields :value))

(defentity vgroup
  (pk :id)
  (has-many verb))

(defentity quiz-generation-filter
  (pk :id)
  (table :filter))

(defentity guess
  (pk :id)
  (table :guess))

;; TODO: replace with a (map (fn [..]) (list :classes :filter ..)
(def key-to-table
  {:authentication-codes authentication-codes
   :class classes
   :classes classes
   :filter quiz-generation-filter
   :guess guess
   :question question
   :question-submit question-submit
   :queue queue
   :student user
   :students user
   :students-in-class students-in-class
   :students-in-classes students-in-class
   :students_in_class students-in-class
   :students_in_classes students-in-class
   :tag vgroup
   :test student-test
   :test-submit test-submit
   :tsubmit test-submit
   :user user
   :verb verb})

(defn keyword-to-table [collection-as-key]
  "Map a keyword representing a collection to a PostgreSQL table. In the future, a collection
might even represent arbitrary SQL such as a join of multiple tables or a SELECT .. WHERE
on a table."
  (let [table (key-to-table collection-as-key)]
    (cond (= (type table) clojure.lang.Var$Unbound)
          (throw (.Exception (str "don't know what table this collection is: " collection-as-key)))
          (nil? table)
          (do
            (log/error (str "Sorry, throwing an exception because I don't know how to resolve the keyword: " collection-as-key " to a table."))
            (throw (Exception. (str "don't know what table this collection is: " collection-as-key))))
          true table)))

(defn collection-update [collection]
  (let [map-to-procedure
        {:question (fn [modify-with id]
                     (update question
                             (set-fields modify-with)
                             (where {:id id})))
         :question-submit (fn [modify-with id]
                            (update (key-to-table :question-submit)
                                    (set-fields modify-with)
                                    (where {:id id})))
         :test (fn [modify-with id]
                 (update student-test
                         (set-fields modify-with)
                         (where {:id id})))
         :test-submit (fn [modify-with id]
                        (update (key-to-table :test-submit)
                                (set-fields modify-with)
                                (where {:id id})))
         :verb (fn [modify-with id]
                 (let [modify-with (dissoc (dissoc modify-with :created) :updated)
                       set-the-fields {:value (str modify-with)}]
                   (log/debug (str "updating :verb table with set-fields: " set-fields))
                   (log/info (str "updating :verb table with id: " id))
                   (update verb
                           (set-fields set-the-fields)
                           (where {:id id}))))
         :tag (fn [modify-with id]
                (log/info (str "UPDATE STATEMENT: " (str "UPDATE vgroup SET verbs = '{" (string/join ","
                                                                                                     (:verbs modify-with))
                                                         "}' WHERE id=?") (vec (list id))))
                
                
                ;; TODO: allow updates of name of tag: right now, it just
                ;; handles :verbs of the tag.
                (if (:verbs modify-with)
                  (exec-raw [(str "UPDATE vgroup SET verbs = '{" (string/join ","
                                                                              (:verbs modify-with))
                                  "}' WHERE id=?") (vec (list id))])
                  ;; if :id and :verbs are not set, ignore (for now) with a warning.
                  (log/warn (str "ignoring update of tag collection with modify-with: " modify-with " and id: " id))))}]
    (let [lookup (map-to-procedure collection)]
      (if lookup lookup
          ;; default update function, if (lookup collection) returned nil:
          (fn [modify-with id]
            (update (key-to-table collection)
                    (set-fields modify-with)
                    (where {:id id})))))))

(defn jdbc2joda [time]
  (c/from-long (.getTime time)))

(defn do-each-row [collection]
  ;; return the 'interesting' parts of a row as a map.
  ;; TODO: add a default to be used if no matching key in this map.
  (let [map-collection-to-fn
        {
         :question (fn [row]
                     (merge
                      {:created (jdbc2joda (:created row))}
                      {:_id (:id row)}
                      (if (:updated row)
                        {:updated (jdbc2joda (:updated row))}
                        (log/warn (str "no updated value found for row:" row)))
                      (reduce #(dissoc %1 %2) row
                              '(:_id :updated :created))))

         :question-submit (fn [row]
                            (merge
                             {:_id (:id row)}
                             {:created (jdbc2joda (:created row))}
                             
                             (reduce #(dissoc %1 %2) row
                                     '(:_id :updated :created))))
         
         :tag (fn [row] ;; for the vgroup table, it's simpler: simply convert :id to :_id.
                (merge
                 {:_id (:id row)}
                 {:verbs (if (nil? (:verbs row))
                           []
                           (vec (.getArray (:verbs row))))}
                 (dissoc (dissoc row :id)
                         :verbs)))
         
         :test (fn [row]
                 (merge
                  {:_id (:id row)}
                  {:created (jdbc2joda (:created row))}
                  
                  (reduce #(dissoc %1 %2) row
                          '(:_id :updated :created))))

         :test-submit (fn [row]
                        (merge
                         {:_id (:id row)}
                         {:created (jdbc2joda (:created row))}
                         
                         (reduce #(dissoc %1 %2) row
                                 '(:_id :updated :created))))
         
         :verb (fn [row] ;; for the verb table, parse the :value column into a map, and then
                 ;; merge with the other non-:value columns, and underscore the id.
                 (merge
                  (read-string (:value row))
                  {:_id (:id row)}
                  
                  ;; .getTime: java.sql.Timestamp -> long
                  ;; from-long: long -> Joda DateTime.
                  {:created (jdbc2joda (:created row))}
                  
                  (reduce #(dissoc %1 %2) row
                          (list :created :id :value))))}]
    (let [lookup (collection map-collection-to-fn)]
      (if lookup lookup
          ;; default:
          (fn [row]
            (merge
             (if (:id row) {:_id (:id row)} {})
             (if (:created row) {:created (jdbc2joda (:created row))} {})
             
             (reduce #(dissoc %1 %2) row
                     '(:_id :updated :created))))))))
    
(def table-to-filter
  {:verb (fn [row the-where]
           (log/info (str "the row: " row))
           (log/info (str "the row's value: " (:value row)))
           (log/info (str "the row's value (read-string): " (read-string (:value row))))
           (log/info (str "the-where: " the-where))
           (log/info (str "unify: " (unify (read-string (:value row))
                                           the-where)))
           (not (fail? (unify (read-string (:value row))
                              the-where))))})

(defn fetch [collection & [ the-where order-by]]
  "select from collection; might take an id. For each returned row, return simply the row as a clojure map, but merge it with an extra field for the primary key (id)."
  (let [the-where
        (if the-where the-where nil)
        ;; TODO: get rid of this horrible :_id/:id munging.
        id (cond (:_id the-where) 
                 (Integer. (:_id the-where))

                 (:id the-where)
                 (Integer. (:id the-where))
                 
                 :else nil)
        table (keyword-to-table collection)]
    (if id (log/debug (str "doing fetch in table: " collection " with row id: " id)))
    (log/info (str "table: " table))
    (if-let [collection-map-function
             (do-each-row collection)]
      (map collection-map-function
           (if id
             (let [rows (select table
                                (where {:id id}))]
               (if (nil? rows)
                 (log/warn (str "no row found for id: " id)))
               (if (> (.size rows) 1)
                 (log/warn (str "more than one row matched id: " id)))
               rows)
             
             ;; else, id not given: do a select with a where (or not, if no where).
             (do
               (log/info (str "doing a select from table=" collection " with where=" the-where))
               (if the-where
                 (if (collection table-to-filter)
                   (filter (fn [row]
                             ((collection table-to-filter)
                              row the-where))
                           (select table))
                   (select table
                           (where the-where)))
                 (select table)))))

      ;; else,no collection-mapping function.
      (do
        (log/warn (str "no collection-mapping function for " collection))
        (let [base
              (if the-where
                (if (collection table-to-filter)
                  (filter (fn [row]
                            ((collection table-to-filter)
                             row the-where))
                          (select table))
                  (-> (select* table)
                      (where the-where)))
                (-> (select* table)))]
          (if order-by
            (exec (-> base
                      (order order-by)))
            (exec base)))))))

(defn fetch-one [collection & [ the-where]]
  (first (take 1 (fetch collection the-where))))

(defn fetch-and-modify [collection id & [modify-with remove?]]
  "modify-with: map of key/value pairs with which to modify row whose id is given in params."
  (log/info (str "fetch-and-modify in collection: " collection))
  (log/debug (str "fetch-and-modify id: " id))
  (log/debug (str "modify-with: " modify-with))
  (log/info (str "remove? " remove?))
  (let [id (Integer. id)]
    (if remove?
      (delete (keyword-to-table collection)
              (where {:id id}))

      ;; remove=false: do update instead.
      (do
        (log/debug (str "collection update: modify-with: " modify-with))
        (log/info (str "collection update: id: " id))
        (apply (collection-update collection)
               (list modify-with id))))))

(defn update! [collection id & [modify-with]]
  (fetch-and-modify collection id modify-with false))

;; TODO: document what insert-values is for.
(defn insert-values [table]
  (let [use-map
        {:verb (fn [add-with]
                 {:value (str add-with)})}]
    (if (use-map table)
      (use-map table)
      (fn [add-with]
        add-with))))

(defn insert! [collection & [add-with]]
  "args are collection and map of key/value pairs with which to initialize new row. we simply serialize the map with (str). Any embedded objects will be lost due to serialization, so map should be only of atoms (strings, numbers, etc) or vectors of atoms (vectors of vectors should work too, provided they are eventually atoms at the leaves)"
  ;; We remove :created and :updated, if any, since we'll let postgres handle
  ;; those through its own constraints and triggers.
  (let [add-with (dissoc (dissoc add-with :created) :updated)]
    (insert (keyword-to-table collection)
            (values 
             (apply (insert-values collection)
                    (list add-with))))))

(defn destroy! [collection & [delete-where]]
  (log/info (str "deleting from: " collection " where : " delete-where))
  (delete (keyword-to-table collection)
          (where delete-where)))

(defn object-id [ & args ]
  (Integer. (first args)))

(defn primary-key [map]
  (:_id map))
