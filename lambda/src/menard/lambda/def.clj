;; defines the lambdas we deploy to the AWS Lambda service:
(ns menard.lambda.def
  (:gen-class)
  (:require
   [clojure.tools.logging :as log]
   [fierycod.holy-lambda.core :as h]
   [menard.handlers :as handlers]
   [menard.english :as en]
   [menard.english.complete :as en-complete]
   [menard.english.woordenlijst :as en-woordenlijst]
   [menard.nederlands.basic :as nl-basic]
   [menard.nederlands.complete :as nl-complete]
   [menard.nederlands.woordenlijst :as nl-woordenlijst]))

(defonce origin "https://hiro-tan.org")

(defonce headers {"Content-Type" "application/json"
                  "Access-Control-Allow-Origin" origin
                  "Access-Control-Allow-Credentials" "true"})

(defn json-response [body]
  {:statusCode 200
   :headers headers
   :body body
   :isBase64Encoded false})

;; TODO: move to menard/handlers.
(defn get-target-model [& [given-model-name]]
  (let [model-name (or given-model-name "complete-model")]
    (cond (= "woordenlijst-model" model-name)
          nl-woordenlijst/model
          (= "woordenlijst" model-name)
          nl-woordenlijst/model

          (= "basic-model" model-name)
          nl-basic/model
          (= "basic" model-name)
          nl-basic/model

          given-model-name
          (do
            (log/warn (str "request-supplied target-model: '" given-model-name "' doesn't exist: falling back to nl-complete/model."))
            nl-complete/model)

          :else nl-complete/model)))

;; TODO: move to menard/handlers.
(defn get-source-model [& [given-model-name]]
  (let [model-name (or given-model-name "complete-model")]
    (cond (= "woordenlijst-model" model-name)
          en-woordenlijst/en-model
          (= "woordenlijst" model-name)
          en-woordenlijst/en-model

          given-model-name
          (do
            (log/warn (str "request-supplied source-model: '" given-model-name "' doesn't exist: falling back to (legacy) en/model."))
            en/model)

          :else en/model)))

(h/deflambda AnalyzeEN
  [event context]
  (let [matching-lexemes
        (-> event
            :queryStringParameters
            :q
            ((fn [q] (handlers/analyze q "en"))))]
    (->> matching-lexemes
         (map dag_unify.serialization/serialize)
         (map str)
         json-response)))


(h/gen-main [#'AnalyzeEN])
