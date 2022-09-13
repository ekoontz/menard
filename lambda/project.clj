(defproject menard-lambda "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [fierycod/holy-lambda "0.0.7"]
                 [menard "1.6.2"]]
  :global-vars {*warn-on-reflection* true}
  :main ^:skip-aot menard.lambda.def
  :uberjar-name "menard-lambda.jar"
  :profiles {:uberjar {:aot :all}})

