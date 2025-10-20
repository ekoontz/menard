(defproject menard "1.9.12"
  :description "A library for natural language generation and parsing"
  :url "http://github.com/ekoontz/menard"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[dag_unify "1.11.1"]
                 [log4j/log4j "1.2.17"]
                 [org.clojure/clojure "1.11.3"]
                 [org.clojure/data.json "1.0.0"]
                 [org.clojure/tools.logging "1.2.4"]
                 [clojure.java-time "0.3.3"]
                 [org.clojure/core.async "1.5.648"]
                 [babashka/fs "0.1.2"]
                 [yogthos/config "1.1.6"]]
  :resource-paths ["resources"]
  :jvm-opts ["-Djdk.attach.allowAttachSelf"])



