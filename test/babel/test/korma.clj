;; TODO: rename this - misleading name 
;; - it uses korma, but it is not itself korma, nor does it test korma per-se.
(ns babel.test.korma
  (:refer-clojure :exclude [test update])
  (:require [babel.korma :refer [init-db prepare-array]]
            [clj-time.coerce :as c]
            [clojure.data.json :as json :refer [write-str]]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [korma.core :refer :all]
            [korma.db :refer [default-connection defdb postgres]]))

(require '[environ.core :refer [env]])

(init-db)

(deftest simple
  (let [result (exec-raw ["SELECT 1 AS retval"] :results)]
    (is (= 1 (:retval (first result))))))

(deftest prepare-vector
  (let [input-vector ["a" "b" "c"]
        output-array (prepare-array input-vector)]
    (is (= (vec (.getArray output-array))
           input-vector))))

(deftest prepare-json
  (let [input [{:foo 42}]]
    (= {"foo" 42}
       (clojure.data.json/read-str
        (first
         (vec
          (.getArray
           (:retval
            (first
             (exec-raw [(str "SELECT ?::jsonb[] AS retval")
                        [(prepare-array [{:foo 42}])]] :results))))))))))


