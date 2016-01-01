(defproject babel "0.1.0-SNAPSHOT"
  :description "A Clojure library for generation of expressions from grammars and lexicons."
  :url "http://github.com/ekoontz/babel"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[clj-time "0.7.0"]
                 [clojail "1.0.6"]
                 [compojure "1.1.6"]
                 [dag_unify "0.1.0-SNAPSHOT"]
                 [environ "1.0.0"]
                 [hiccup "1.0.5"]
                 [javax.servlet/servlet-api "2.5"]
                 [korma "0.4.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jdmk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.postgresql/postgresql "9.4-1204-jdbc42"]
                 [ring/ring-codec "1.0.0"]
                 [ring/ring-jetty-adapter "1.1.0"]
                 [ring/ring-devel "1.1.0"]
                 [ring-basic-authentication "1.0.1"]]
  :resource-paths ["resources"]
  :plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]
            [lein-cljsbuild "1.1.2"]
            [lein-doo "0.1.6"]
            [lein-environ "1.0.0"]
            [lein-localrepo "0.4.0"]
            [lein-pprint "1.1.1"]
            [lein-ring "0.9.3"]]

  ;; run clojure tests with "lein test"
  ;; run clojurescript tests with "lein doo slimer test"
  :cljsbuild {:builds [{:id "test"
                        :source-paths ["src" "test"
                                       "src/babel/espanol"
                                       ]
                        :compiler {:output-to "out/testable.js"
                                   ;; you must have {:optimizations :whitespace}
                                   ;; to avoid "ReferenceError: Can't find variable: goog"
                                   :optimizations :whitespace}}]}
  :doo {:paths {:phantom "phantomjs --debug=true --web-security=false  --disk-cache=true --webdriver=127.0.0.1:8910"
                :slimer "slimerjs --ignore-ssl-errors=true"
                :karma "karma --port=9881 --no-colors"
                :rhino "java -jar /Users/ekoontz/Downloads/rhino1_7R4/js-14.jar -strict"
                :node "node --trace-gc --trace-gc-verbose"}}
  
  :ring {:handler babel.core/app})



