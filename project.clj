(defproject babel "3.2.11-SNAPSHOT"
  :description "A library for natural language generation and parsing"
  :url "http://github.com/ekoontz/babel"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[cljstache "2.0.0"]
                 [clj-time "0.11.0"]
                 [clojail "1.0.6"]
                 [compojure "1.1.6"]
                 [dag_unify "1.6.2"]
                 [environ "1.0.0"]
                 [korma "0.4.3"]
                 [log4j/log4j "1.2.17"]
                 [org.clojure/core.cache "0.7.1"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.293"]
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [org.clojure/tools.logging "0.4.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.postgresql/postgresql "42.1.4"]]

  :filespecs [{:type :path :path "compiled"}]
  :resource-paths ["resources"]
  :plugins [[cider/cider-nrepl "0.18.0"]
            [lein-cljsbuild "1.1.2"]
            [lein-doo "0.1.6"]
            [lein-environ "1.0.0"]
            [lein-localrepo "0.4.0"]
            [lein-pprint "1.1.1"]
            [lein-ring "0.9.7"]]

  ;; run clojure tests with "lein test"
  ;; run clojurescript tests with "lein doo slimer test"
  :cljsbuild {:builds [{:id "test"
                        :source-paths ["src" "test"]
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
;; this hook doesn't work yet.
;;  :hooks [leiningen.cljsbuild])

;; TODO: split into:
;; 
;; babel.core: generation, parsing, lexical compilation, encyclopedia and anything else not covered below
;; babel.format: translation to/from json and html
;; babel.db: SQL support (mainly Postgres/JSONB specific)
;; babel.http: workbook, compojure http routes, querying from GETs, updating from POSTs, PUTs and PATCHes. all the ring/* and servlet-api :dependencies go here.
;;
;; Language-specific: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes

;; babel.ab (Abkhaz)
;; ..
;; babel.zu (Zulu)
;;




