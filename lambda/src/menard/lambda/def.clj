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

(h/deflambda AnalyzeNL
  [event context]
  (let [matching-lexemes
        (-> event
            :queryStringParameters
            :q
            ((fn [q] (handlers/analyze q "nl"))))]
    (->> matching-lexemes
         (map dag_unify.serialization/serialize)
         (map str)
         json-response)))

(h/deflambda Parse
  [event context]
  (-> event
      :queryStringParameters
      :q
      handlers/parse-nl
      json-response))

(h/deflambda ParseNLStart
  [event context]
  (-> event
      :queryStringParameters
      :q
      handlers/parse-nl-start
      json-response))

(h/deflambda ParseENStart
  [event context]
  (-> event
      :queryStringParameters
      :q
      handlers/parse-en-start
      json-response))

(h/deflambda GenerateNL
  [event context]
  (let [spec (-> event :queryStringParameters :q)
        model (-> event :queryStringParameters :model)
        target-model (get-target-model model)
        source-model (get-source-model model)]
    (json-response (handlers/generate-nl-and-en-by-spec spec target-model source-model))))

(h/deflambda GenerateEN
  [event context]
  (let [spec (-> event :queryStringParameters :spec)]
    (json-response (handlers/generate-en spec))))

(h/deflambda GenerateWithAlts
  [event context]
  (let [spec (-> event :queryStringParameters :spec)
        alternates (-> event :queryStringParameters :alts)
        model (-> event :queryStringParameters :model)
        target-model (get-target-model model)
        source-model (get-source-model model)]
    (json-response (handlers/generate-nl-with-alternations spec alternates target-model source-model))))

(h/deflambda GrammarEN
  [event context]
  (json-response (handlers/grammar "en")))

(h/deflambda GrammarNL
  [event context]
  (json-response (handlers/grammar "nl")))

(h/deflambda MorphologyEN
  [event context]
  (json-response (handlers/morphology "en")))

(h/deflambda MorphologyNL
  [event context]
  (json-response (handlers/morphology "nl")))

(h/deflambda RuleEN
  [event context]
  (let [matching-rules
        (-> event
            :queryStringParameters
            :q
            ((fn [q] (handlers/rules q "en"))))]
    (->> matching-rules
         (map dag_unify.serialization/serialize)
         (map str)
         json-response)))

(h/deflambda RuleNL
  [event context]
  (let [matching-rules
        (-> event
            :queryStringParameters
            :q
            ((fn [q] (handlers/rules q "nl"))))]
    (->> matching-rules
         (map dag_unify.serialization/serialize)
         (map str)
         json-response)))

;; TODO: deprecate and remove 'Generate in favor of 'GenerateNL.
(h/gen-main [#'AnalyzeEN
             #'AnalyzeNL
             #'Parse
             #'Generate
             #'GenerateEN
             #'GenerateNL
             #'GenerateWithAlts
             #'GrammarEN
             #'GrammarNL
             #'MorphologyEN
             #'MorphologyNL
             #'ParseNLStart
             #'RuleEN
             #'RuleNL])
