(defproject server "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/tools.logging "1.1.0"]
                 [menard "1.5.0"]
                 [metosin/reitit "0.5.10"]
                 [ring-server "0.5.0"]
                 [ring "1.8.0"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-devel "1.7.1"]
                 [org.clojure/data.json "0.2.7"]
                 [yogthos/config "1.1.6"]
                 [org.clojure/core.async "0.4.474"]
                 [ring/ring-jetty-adapter "1.7.1"]
                 [nrepl "0.9.0"]]
  :ring {:init menard.nederlands/start-reload-loop
         :handler server/app}
  :plugins [[lein-ring "0.12.5"]])
