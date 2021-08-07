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
  
(h/deflambda Parse
  [event context]
  (-> event
      :queryStringParameters
      :q
      handlers/parse-nl
      json-response))

(h/deflambda Generate
  [event context]
  (let [q (-> event :queryStringParameters :q)]
    (json-response (handlers/generate-nl-by-spec q))))

(h/deflambda GenerateWithAlts
  [event context]
  (let [spec (-> event :queryStringParameters :spec)
        alternates (-> event :queryStringParameters :alts)]
    (json-response (handlers/generate-nl-with-alternations spec alternates))))

(h/gen-main [#'Parse
             #'Generate
             #'GenerateWithAlts])
