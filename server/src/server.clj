(ns server
  (:require
   [clojure.tools.logging :as log]
   [clojure.data.json :as json :refer [write-str]]
   [config.core :refer [env]]
   [menard.handlers :as handlers]
   [reitit.ring :as reitit-ring]
   [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
   [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

(defonce origin
  (if-let [env-origin (env :origin)]
    (do (log/info (str "environment's origin: " env-origin))
        env-origin)
    (do (log/info (str "environment had no origin defined: using '/'."))
        "/")))

(def headers {"Content-Type" "application/json"
              "Access-Control-Allow-Origin" origin
              "Access-Control-Allow-Credentials" "true"})

(defn json-response
  "Call clojure.data.json/write-str to turn that structure into JSON
   so the client's browser can parse it."
  [body]
  {:status 200
   :headers headers
   :body (write-str body)})

(def routes
  [["/parse"
    {:get {:handler
           (fn [request]
             (-> request
                 :query-params
                 (get "q")
                 handlers/parse-nl
                 json-response))}}]

   ["/parse-start"
    {:get {:handler
           (fn [request]
             (let [intermediate-result
                   (-> request
                       :query-params
                       (get "q")
                       handlers/parse-nl-start)]
               (log/info (str "intermediate-result: " intermediate-result))
               (let [prelim-result
                     (into {}
                           (->> (keys intermediate-result)
                                (map (fn [k]
                                       [(str k)
                                        (map (fn [x] (-> x dag_unify.serialization/serialize str))
                                             (get intermediate-result k))]))))]
                 (log/info (str "prelim: " prelim-result))
                 (json-response prelim-result))))}}]
   
   ["/generate"
    {:get {:handler
           (fn [request]
             (-> request
                 :query-params
                 (get "q")
                 handlers/generate-nl-by-spec
                 json-response))}}]

   ["/generate-with-alts"
    {:get {:handler
           (fn [request]
             (let [spec (-> request :query-params (get "spec"))
                   alternates (-> request :query-params (get "alts"))]
               (-> (handlers/generate-nl-with-alternations spec alternates)
                   json-response)))}}]

   ["/grammar/:lang"
    {:get {:handler
           (fn [request]
             (let [language (-> request :path-params (get :lang))]
               (->> (handlers/grammar language)
                    (map (fn [rule] (-> rule dag_unify.serialization/serialize str)))
                    json-response)))}}]])
   
(def middleware
  [#(wrap-defaults % (assoc site-defaults :session false))])

(def app
  (reitit-ring/ring-handler
   (reitit-ring/router routes)
   (reitit-ring/routes
    (reitit-ring/create-default-handler))
   {:middleware middleware}))

(defn -main [& args]
  (let [port (or (env :port) 3000)]
    (run-jetty app {:port port :join? false})))

