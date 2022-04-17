;; defines the lambdas we deploy to the AWS Lambda service:
(ns menard.lambda.def
  (:gen-class)
  (:require
   [fierycod.holy-lambda.core :as h]
   [menard.handlers :as handlers]))

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

(h/deflambda Generate
  [event context]
  ;; TODO: use 'spec' rather than 'q':
  (let [q (-> event :queryStringParameters :q)]
    (json-response (handlers/generate-nl-by-spec q))))

(h/deflambda GenerateEN
  [event context]
  (let [spec (-> event :queryStringParameters :spec)]
    (json-response (handlers/generate-en spec))))

(h/deflambda GenerateWithAlts
  [event context]
  (let [spec (-> event :queryStringParameters :spec)
        alternates (-> event :queryStringParameters :alts)]
    (json-response (handlers/generate-nl-with-alternations spec alternates))))

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

(h/gen-main [#'AnalyzeEN
             #'AnalyzeNL
             #'Parse
             #'Generate
             #'GenerateEN
             #'GenerateWithAlts
             #'GrammarEN
             #'GrammarNL
             #'MorphologyEN
             #'MorphologyNL
             #'ParseNLStart
             #'RuleEN
             #'RuleNL])



