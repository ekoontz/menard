(ns babel.say
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [clojure.data.json :refer [write-str]]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
))

(def routes
  (compojure/routes

   (GET "/" request
        (let [to (:to (:params request))
              expr (:expr (:params request))
              response (str "ciao; tu hai detto: '" expr "'")]
          (log/info (str "client is talking to: " to))
          (log/info (str " and saying: " expr))
          (log/info (str "response: " response))
          {:status 200
           :body (write-str {:yousaid expr
                             :response response})
           :headers {"Content-Type" "text/json;charset=utf-8"}}))))
