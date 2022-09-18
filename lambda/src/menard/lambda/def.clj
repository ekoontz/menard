;; defines the lambdas we deploy to the AWS Lambda service:
(ns menard.lambda.def
  (:gen-class)
  (:require
   [clojure.tools.logging :as log]
   [fierycod.holy-lambda.core :as h]
   [menard.handlers :as handlers :use [get-source-model get-target-model]]
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
  (let [model-name (or (-> event :queryStringParameters :model) "complete-model")
        model (-> model-name handlers/get-target-model deref)]
    (-> event
        :queryStringParameters
        :q
        (#(handlers/parse-nl-start % model))
        json-response)))

(h/deflambda ParseENStart
  [event context]
  (let [model-name (or (-> event :queryStringParameters :model) "complete-model")
        model (-> model-name handlers/get-source-model deref)]
    (-> event
        :queryStringParameters
        :q
        (#(handlers/parse-en-start % model))
        json-response)))

(h/deflambda GenerateNL
  [event context]
  (let [spec (-> event :queryStringParameters :q)
        model (-> event :queryStringParameters :model)
        target-model (get-target-model model)
        source-model (get-source-model model)]
    (log/info (str "GenerateNL: got source model: type: " (type model)))    
    (json-response (handlers/generate-nl-and-en-by-spec spec target-model source-model))))

(h/deflambda GenerateEN
  [event context]
  (let [spec (-> event :queryStringParameters :spec)
        model (-> event :queryStringParameters :model)
        source-model (get-source-model model)]
    (log/info (str "GenerateEN: got source model: type: " (type model)))
    (json-response (handlers/generate-en spec source-model))))

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
