(defproject nlquiz-lambda "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.2-rc1"]
                 [fierycod/holy-lambda "0.0.7-SNAPSHOT"]
                 [menard "1.4.1-SNAPSHOT"]]
  :global-vars {*warn-on-reflection* true}
  :main ^:skip-aot nlquiz-lambda.core
  :profiles {:uberjar {:aot :all}}
  :uberjar-name "output.jar")
