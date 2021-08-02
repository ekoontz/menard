(ns server
  (:require
   [clojure.tools.logging :as log]
   [clojure.data.json :as json :refer [write-str]]
   [config.core :refer [env]]
   [menard.lambda.handlers :as handlers]
   [reitit.ring :as reitit-ring]
   [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
   [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

(defonce origin
  (do (log/info (str "environment ORIGIN: " (env :origin)))
      (or (env :origin) "/")))

(defn json-response
  "Call a handler on a request, which returns a clojure data structure.
   Then call clojure.data.json/write-str to turn that structure into JSON
   so the client's browser can parse it."
  [_request handler]
  {:status 200
   :headers {"Content-Type" "application/json"
             "Access-Control-Allow-Origin" origin
             "Access-Control-Allow-Credentials" "true"}
   :body (-> _request handler write-str)})

(declare parse-nl)
(declare generate-nl-by-spec)
(declare generate-nl-with-alternations)

(def routes
  [["/parse"
    {:get {:handler (fn [request] (json-response request parse-nl))}}]

   ["/generate"
    {:get {:handler (fn [request] (json-response request generate-nl-by-spec))}}]

   ["/generate-with-alts"
    {:get {:handler (fn [request] (json-response request generate-nl-with-alternations))}}]])

(def middleware
  [#(wrap-defaults % (assoc site-defaults :session false))])

(def app
  (reitit-ring/ring-handler
   (reitit-ring/router routes)
   (reitit-ring/routes
    (reitit-ring/create-default-handler))
   {:middleware middleware}))

(defn generate-nl-by-spec
  [request]
  (let [spec (-> request :query-params (get "q"))]
    (handlers/generate-nl-by-spec spec)))

(defn generate-nl-with-alternations
  [request]
  (let [spec (-> request :query-params (get "spec"))
        alternates (-> request :query-params (get "alts"))]
    (handlers/generate-nl-with-alternations spec alternates)))

(defn parse-nl [request]
  (let [string-to-parse (-> request :query-params (get "q"))]
    (handlers/parse-nl string-to-parse)))

(defn -main [& args]
  (let [port (or (env :port) 3000)]
    (run-jetty app {:port port :join? false})))

