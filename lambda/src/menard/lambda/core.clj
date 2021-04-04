(ns menard.lambda.core
  (:gen-class)
  (:require
   [fierycod.holy-lambda.core :as h]
   [menard.lambda.handlers
    :refer [generate-nl-by-spec
            generate-nl-with-alternations
            parse-nl]]))

(h/deflambda ParseNL
  [event context]
  (let [q (-> event :queryStringParameters :q)]
    {:statusCode 200
     :headers {"Content-Type" "application/json"
               "Access-Control-Allow-Origin" "https://hiro-tan.org"
               "Access-Control-Allow-Credentials" "true"}
     :body (parse-nl q)
     :isBase64Encoded false}))

(h/deflambda GenerateNL
  [event context]
  (let [q (-> event :queryStringParameters :q)]
    {:statusCode 200
     :headers {"Content-Type" "application/json"
               "Access-Control-Allow-Origin" "https://hiro-tan.org"
               "Access-Control-Allow-Credentials" "true"}
     :body (generate-nl-by-spec q)
     :isBase64Encoded false}))

(h/deflambda GenerateWithAltsNL
  [event context]
  (let [spec (-> event :queryStringParameters :spec)
        alternates (-> event :queryStringParameters :alts)]
    {:statusCode 200
     :headers {"Content-Type" "application/json"
               "Access-Control-Allow-Origin" "https://hiro-tan.org"
               "Access-Control-Allow-Credentials" "true"}
     :body (generate-nl-with-alternations spec alternates)
     :isBase64Encoded false}))

(h/gen-main [#'ParseNL
             #'GenerateNL
             #'GenerateWithAltsNL])
