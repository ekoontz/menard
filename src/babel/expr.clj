(ns babel.expr
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [babel.reader :as reader]
   [clojure.data.json :refer [write-str]]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]))

(def routes
  (compojure/routes
   (GET "/" request
        {:status 200
         :body (write-str {:foo 42
                           :bar 43})})

   (GET "/:expr" request
        (let [expr (:expr (:route-params request))]
          (log/info (str "expr(1):" expr))
          (log/info (str "expr(2):" (Integer. expr)))
          {:status 200
           :body (write-str (reader/id2expression (Integer. expr)))}))))


   

