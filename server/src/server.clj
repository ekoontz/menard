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

(def routes
  [
   ["/generate-with-alts/:lang"
    {:get {:handler
           (fn [request]
             (let [language (-> request :path-params (get :lang))
                   use-fn (cond (= language "nl")
                                handlers/generate-nl-with-alternations
                                true
                                (fn [spec alternates]
                                  (log/warn "unsupported language: " language)
                                  (-> {}
                                      json-response)))
                   spec (-> request :query-params (get "spec"))
                   alternates (-> request :query-params (get "alts"))]
               (-> (use-fn spec alternates)
                   json-response)))}}]

   ["/grammar/:lang"
    {:get {:handler
           (fn [request]
             (let [language (-> request :path-params (get :lang))]
               (->> (handlers/grammar language)
                    (map (fn [rule] (-> rule dag_unify.serialization/serialize str)))
                    json-response)))}}]

   ["/generate/en"
    {:get {:handler
           (fn [request]
             (let [spec (-> request :query-params (get "spec") read-string dag_unify.serialization/deserialize)]
               (log/debug (str "generate/en spec: " spec))
               (let [response (handlers/generate-en spec)]
                 (log/debug (str "generate/en response: " response))
                 (-> response
                     json-response))))}}]

   ["/morphology/:lang"
    {:get {:handler
           (fn [request]
             (let [language (-> request :path-params (get :lang))]
               (let [retval
                     (->> (handlers/morphology language)
                          (map (fn [{[generate-from generate-to] :g
                                     [parse-from parse-to] :p
                                     u :u}]
                                 ;; regexes like: #"ot$" have to be
                                 ;; converted into strings: "ot$" via (str).
                                 ;; The inverse of this is
                                 ;; clojure.core/re-pattern (converts a
                                 ;; string e.g. "ot$" into a regex #"ot$".
                                 {:u u
                                  :g [(str generate-from) generate-to]
                                  :p [(str parse-from) parse-to]})))]
                 (->> retval
                      json-response))))}}]
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
                   intermediate-result
                   (if (and do-all? (seq do-all?))
                     (-> query-params
                         (get "q")
                         parse-all)
                     (-> query-params
                         (get "q")
                         parse-start))]
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

   ;; deprecated: use /generate/nl instead:
   ["/generate"
    {:get {:handler
           (fn [request]
             (-> request
                 :query-params
                 (get "q")
                 handlers/generate-nl-by-spec
                 json-response))}}]

   ;; deprecated: use /generate-with-alts/nl instead:
   ["/generate-with-alts"
    {:get {:handler
           (fn [request]
             (let [spec (-> request :query-params (get "spec"))
                   alternates (-> request :query-params (get "alts"))]
               (-> (handlers/generate-nl-with-alternations spec alternates)
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

(defn -main [& args]
  (let [port (or (env :port) 3000)]
    (run-jetty app {:port port :join? false})))

