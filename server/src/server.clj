(ns server
  (:require
   [clojure.tools.logging :as log]
   [clojure.data.json :as json :refer [write-str]]
   [config.core :refer [env]]
   [menard.handlers :as handlers]
   [menard.english :as en]
   [menard.english.complete :as en-complete]
   [menard.english.woordenlijst :as en-woordenlijst]
   [menard.nederlands.basic :as nl-basic]
   [menard.nederlands.complete :as nl-complete]
   [menard.nederlands.woordenlijst :as nl-woordenlijst]
   [nrepl.server :refer [start-server stop-server]]
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

(def cors-headers {"Access-Control-Allow-Origin" origin
                   "Access-Control-Allow-Credentials" "true"})

(def headers (merge cors-headers {"Content-Type" "application/json"}))

(defn json-response
  "Call clojure.data.json/write-str to turn that structure into JSON
   so the client's browser can parse it."
  [body]
  {:status 200
   :headers headers
   :body (write-str body)})

(defn get-target-model [request]
  (-> request :query-params (get "model") handlers/get-target-model))

(defn get-source-model [request]
  (-> request :query-params (get "model") handlers/get-source-model))

(def routes
  [
   ["/generate-with-alts/:lang"
    {:get {:handler
           (fn [request]
             (let [language (-> request :path-params (get :lang))
                   target-model (-> request get-target-model deref)
                   source-model (-> request get-source-model deref)
                   use-fn (cond (= language "nl")
                                handlers/generate-nl-with-alternations
                                true
                                (fn [spec alternates]
                                  (log/warn "unsupported language: " language)
                                  (-> {}
                                      json-response)))
                   spec (-> request :query-params (get "spec"))
                   alternates (-> request :query-params (get "alts"))]
               (-> (use-fn spec alternates target-model source-model)
                   json-response)))}}]

   ["/generate/nl"
    {:get {:handler
           (fn [request]
             (let [target-model (-> request get-target-model deref)
                   source-model (-> request get-source-model deref)
                   spec (-> request :query-params (get "q"))]
               (log/info (str "/generate/nl: requested spec: "
                              spec "; using model named: '" (-> target-model :name) "'"))
               (-> (handlers/generate-nl-and-en-by-spec spec target-model source-model)
                   json-response)))}}]

   ["/grammar/:lang"
    {:get {:handler
           (fn [request]
             (let [language (-> request :path-params (get :lang))]
               (->> (handlers/grammar language)
                    json-response)))}}]

   ["/generate/en"
    {:get {:handler
           (fn [request]
             (let [source-model (-> request get-source-model deref)
                   spec (-> request :query-params (get "spec"))]
               (log/debug (str "generate/en spec: " spec))
               (let [response (handlers/generate-en spec model)]
                 (-> spec handlers/generate-en json-response))))}}]

   ["/morphology/:lang"
    {:get {:handler
           (fn [request]
             (let [language (-> request :path-params (get :lang))]
               (->> language handlers/morphology json-response)))}}]

   ["/parse-start/:lang"
    {:get {:handler
           (fn [request]
             (let [language (-> request :path-params (get :lang))
                   parse-all (cond (= language "en")
                                   handlers/parse-en-all
                                   :else
                                   handlers/parse-nl-all)
                   parse-start (cond (= language "en")
                                     handlers/parse-en-start
                                     :else
                                     handlers/parse-nl-start)
                   query-params (-> request :query-params)
                   do-all? (-> query-params (get "all"))
                   model (-> query-params (get "model") handlers/get-target-model deref)
                   intermediate-result
                   (if (and do-all? (seq do-all?))
                     (-> query-params
                         (get "q")
                         (parse-all model))
                     (-> query-params
                         (get "q")
                         (parse-start model)))]
               (log/info (str "ran parse-start with model named: " (-> model :name)))
               (json-response intermediate-result)))}}]
   
   ["/analyze/:lang"
    {:get {:handler
           (fn [request]
             (let [language (-> request :path-params (get :lang))
                   query-params (-> request :query-params)
                   matching-lexemes
                   (handlers/analyze
                    (-> query-params
                        (get "q"))
                    language)]
               (->> matching-lexemes
                    (map dag_unify.serialization/serialize)
                    (map str)
                    json-response)))}}]

   ["/rule/:lang"
    {:get {:handler
           (fn [request]
             (let [language (-> request :path-params (get :lang))
                   query-params (-> request :query-params)
                   matching-rules
                   (-> query-params
                       (get "q")
                       (handlers/rules language))]
               (->> matching-rules
                    (map dag_unify.serialization/serialize)
                    (map str)
                    json-response)))}}]

   ;; deprecated: use /generate-with-alts/nl instead:
   ["/generate-with-alts"
    {:get {:handler
           (fn [request]
             (log/debug (str "/generate-with-alts with request: " request))
             (let [spec (-> request :query-params (get "spec"))
                   alternates (-> request :query-params (get "alts"))
                   target-model (-> request get-target-model deref)
                   source-model (-> request get-source-model deref)]
               (log/info (str "/generate-with-alts: language: nl; requested spec: "
                              spec "; using model named: '" (-> target-model :name) "'"))
               (-> (handlers/generate-nl-with-alternations spec alternates target-model source-model)
                   json-response)))}}]

   ;; deprecated: use /parse/nl instead:
   ["/parse"
    {:get {:handler
           (fn [request]
             (-> request
                 :query-params
                 (get "q")
                 handlers/parse-nl
                 json-response))}}]
   ])

(def middleware
  [#(wrap-defaults % (assoc site-defaults :session false))])

(def app
  (reitit-ring/ring-handler
   (reitit-ring/router routes)
   (reitit-ring/routes
    (reitit-ring/create-default-handler))
   {:middleware middleware}))

(defonce the-server (start-server :port 7888))

(defn -main [& args]
  (let [port (or (env :port) 3000)]
    (run-jetty app {:port port :join? false})))

