(defproject babel "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[clj-time "0.7.0"]
                 [compojure "1.1.6"]
                 [dag-unify "0.1.0-SNAPSHOT"]
                 [environ "1.0.0"]
                 [hiccup "1.0.5"]
                 [javax.servlet/servlet-api "2.5"]
                 [korma "0.4.1"]
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/data.json "0.2.5"]
                 [org.postgresql/postgresql "9.4-1201-jsonb-jdbc41-SNAPSHOT"]
                 [ring/ring-jetty-adapter "1.1.0"]
                 [ring/ring-devel "1.1.0"]
                 [ring-basic-authentication "1.0.1"]]
  :resource-paths ["resources"]
  :repositories {"eugene" "http://hiro-tan.org/~ekoontz/mvn/repository"}
  :plugins [[lein-environ "1.0.0"]
            [lein-localrepo "0.4.0"]
            [lein-pprint "1.1.1"]
            [lein-ring "0.9.3"]])






