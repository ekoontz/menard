;; defines the lambdas we deploy to the AWS Lambda service:
(ns menard.lambda.def
  (:gen-class)
  (:require
   [fierycod.holy-lambda.core :as h]
   [menard.handlers]))

(defonce origin "https://hiro-tan.org")

(defonce headers {"Content-Type" "application/json"
                  "Access-Control-Allow-Origin" origin
                  "Access-Control-Allow-Credentials" "true"})
  
(h/deflambda Parse
  [event context]
  (let [q (-> event :queryStringParameters :q)]
    {:statusCode 200
     :headers headers
     :body (handlers/parse-nl q)
     :isBase64Encoded false}))

(h/deflambda Generate
  [event context]
  (let [q (-> event :queryStringParameters :q)]
    {:statusCode 200
     :headers headers
     :body (handlers/generate-nl-by-spec q)
     :isBase64Encoded false}))

(h/deflambda GenerateWithAlts
  [event context]
  (let [spec (-> event :queryStringParameters :spec)
        alternates (-> event :queryStringParameters :alts)]
    {:statusCode 200
     :headers headers
     :body (handlers/generate-nl-with-alternations spec alternates)
     :isBase64Encoded false}))

(h/gen-main [#'Parse
             #'Generate
             #'GenerateWithAlts])
