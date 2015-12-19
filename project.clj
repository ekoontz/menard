(defproject babel "0.1.0-SNAPSHOT"
  :description "A Clojure library for generation of expressions from grammars and lexicons."
  :url "http://github.com/ekoontz/babel"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[clj-time "0.7.0"]
                 [clojail "1.0.6"]
                 [compojure "1.1.6"]
                 [dag-unify "0.1.0-SNAPSHOT"]
                 [environ "1.0.0"]
                 [hiccup "1.0.5"]
                 [javax.servlet/servlet-api "2.5"]
                 [korma "0.4.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jdmk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [org.clojure/clojure "1.7.0"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.postgresql/postgresql "9.4-1204-jdbc42"]
                 [ring/ring-jetty-adapter "1.1.0"]
                 [ring/ring-devel "1.1.0"]
                 [ring-basic-authentication "1.0.1"]]
  :resource-paths ["resources"]
  :plugins [[lein-environ "1.0.0"]
            [lein-localrepo "0.4.0"]
            [lein-pprint "1.1.1"]
            [cider/cider-nrepl "0.10.0-SNAPSHOT"]
            [lein-ring "0.9.3"]]

  :ring {:handler babel.core/app})



