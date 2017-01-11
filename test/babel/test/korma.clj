;; TODO: rename this - misleading name 
;; - it uses korma, but it is not itself korma, nor does it test korma per-se.
(ns babel.test.korma
  (:refer-clojure :exclude [test update])
  (:require [babel.korma :refer [init-db prepare-array read-array]]
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

(deftest prepare-vector-of-strings
  (let [input ["a" "b" "c"]]
    (is (= input
           (read-array (prepare-array input))))))

(deftest prepare-vector-of-maps
  (let [input [{:foo 42}
               {:bar 43}
               {:foo 44, :bar 45}]]
    (is
     (= input
        (read-array (prepare-array input))))))

