(defproject babylon "0.0.1-SNAPSHOT"
  :description "A library for natural language generation and parsing"
  :url "http://github.com/ekoontz/babylon"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[dag_unify "1.6.6-SNAPSHOT"]
                 [log4j/log4j "1.2.17"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.logging "0.4.0"]]
  :resource-paths ["resources"]
  :plugins [[cider/cider-nrepl "0.18.0"]
            [lein-environ "1.0.0"]
            [lein-localrepo "0.4.0"]
            [lein-pprint "1.1.1"]])




