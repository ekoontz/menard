(defproject server "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.logging "1.1.0"]
                 [clojure.java-time "0.3.3"]
                 [menard "1.6.4-SNAPSHOT"]
                 [metosin/reitit "0.5.18"]
                 [metosin/reitit-ring "0.5.18"]
                 [ring-server "0.5.0"]
                 [ring "1.8.0"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-devel "1.7.1"]
                 [org.clojure/data.json "0.2.7"]
                 [yogthos/config "1.1.6"]
                 [org.clojure/core.async "0.4.474"]
                 [ring/ring-jetty-adapter "1.7.1"]
                 [nrepl "0.9.0"]]
  :ring {:init server/start-reload-loop
         :handler server/app}
  :plugins [[lein-ring "0.12.5"]])
