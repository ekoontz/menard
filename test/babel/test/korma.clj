;; TODO: rename this - misleading name 
;; - it uses korma, but it is not itself korma.
(ns babel.test.korma
  (:refer-clojure :exclude [test update])
  (:require [babel.writer :as writer]
            [clj-time.coerce :as c]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [korma.core :refer :all]
            [korma.db :refer [default-connection defdb postgres]]))

(require '[environ.core :refer [env]])

(deftest simple
  (let [result (exec-raw ["SELECT 1 AS retval"] :results)]
    (is (= 1 (:retval (first result))))))






