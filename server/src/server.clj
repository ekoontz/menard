(ns server
  (:require
   [clojure.tools.logging :as log]
   [clojure.data.json :as json :refer [write-str]]
   [clojure.java.io :as io :refer [resource]]
   [config.core :refer [env]]
   [menard.exception :refer [exception]]
   [menard.handlers :as handlers]
   [menard.english.complete :as en]
   [menard.español :as es]
   [menard.lexiconfn :as l]
   [menard.model :refer [create-model-from-filesystem
                         current-ms get-info-of-files]]
   [menard.nederlands.basic :as nl-basic]
   [menard.nederlands.compile :refer [compile-lexicon]]
   [menard.nederlands.complete :as nl-complete]
   [menard.nederlands.woordenlijst :as nl-woordenlijst]
   [nrepl.server :refer [start-server stop-server]]
   [reitit.ring :as reitit-ring]
   [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
   [ring.adapter.jetty :refer [run-jetty]]
   [clojure.core.async :refer [go-loop]]
   [java-time :as jt])
   
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
             (let [model (-> request get-source-model deref)
                   spec (-> request :query-params (get "spec"))]
               (log/debug (str "generate/en spec: " spec))
               (let [response (handlers/generate-en spec model)]
                 (-> response json-response))))}}]

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
                   get-model-fn (cond (= language "en")
                                      handlers/get-source-model
                                      :else
                                      handlers/get-target-model)
                   model (-> query-params (get "model") get-model-fn deref)
                   intermediate-result
                   (if (and do-all? (seq do-all?))
                     (-> query-params
                         (get "q")
                         (parse-all (-> model :grammar)))
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

(defn load-model [model & [reload?]]
  (when (or (nil? @model) (true? reload?))
    (try
      (log/info (str (when @model "re") "loading model: " (:name @model)))
      (let [loaded (create-model-from-filesystem (:spec @model) compile-lexicon)]
        (dosync
         (ref-set model loaded))
        (log/info (str "loaded model: " (:name @model))))
      (catch Exception e (do
                           (log/info (str "Failed to load model; the error was: '" (str e) "'. Will keep current model as-is and wait 10 seconds and see if it's fixed then."))))))
  (if (nil? @model)
    (log/error (str "load-model: model couldn't be loaded. Tried both built-in jar and filesystem.")))
  @model)

(defn start-reload-loop []
  (go-loop []
    (let [models [nl-complete/model
                  en-complete/model
                  en-woordenlijst/model
                  nl-woordenlijst/model]]
      (doall
       (->> models
            (map (fn [model]
                   (let [last-file-check (get (-> model deref) :last-checked 0)]
                     (log/info (str "checking model last modified at: "  (jt/java-date last-file-check) " with model name: " (-> model deref :name) " for changes.."))
                     ;; TODO: check model config files rather than <path> <filename-pattern>:
                     (let [en-file-infos (get-info-of-files "../resources/english" "**{.edn}")
                           nl-file-infos (get-info-of-files "../resources/nederlands" "**{.edn}")
                           general-file-infos (get-info-of-files "../resources" "*{.edn}")
                           most-recently-modified-info
                           (->>
                            (concat en-file-infos
                                    nl-file-infos
                                    general-file-infos)
                            (sort (fn [a b] (> (:last-modified-time-ms a) (:last-modified-time-ms b))))
                            first)
                           
                           last-file-modification
                           (:last-modified-time-ms most-recently-modified-info)]
                       (when (> last-file-modification last-file-check)
                         (do
                           (log/info (str
                                      "start-reload-loop: reloading model: " (-> model deref :name) ": most recently modified file was: "
                                      (:parent most-recently-modified-info) "/"
                                      (:filename most-recently-modified-info)
                                      " at: "
                                      (:last-modified-time most-recently-modified-info)))
                           (dosync (ref-set model
                                            (merge
                                             (load-model model true)
                                             {:last-checked last-file-modification}))))))))))))

    (Thread/sleep 10000)
    (recur)))

