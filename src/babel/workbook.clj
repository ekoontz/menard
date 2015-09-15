(ns babel.workbook
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [babel.engine :refer [generate]]
   [babel.english.grammar :as en]
   [babel.espanol.writer :as es]
   [babel.francais.grammar :as fr]
   [babel.italiano.grammar :as it]
   [babel.over :refer [over]]
   [babel.html :as html]
   [babel.korma :as korma]
   [babel.parse :as parse]
   [babel.pos :as pos]
   [babel.reader :as reader]
   [babel.test.fr :as frtest]
   [clojail.core :refer [sandbox]]
   [clojail.testers :refer :all]
   [clojure.core :exclude [get-in]]
   [clojure.core :as core] ;; This allows us to use core's get-in by doing "(core/get-in ..)"
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [dag-unify.core :refer [get-in remove-false strip-refs]]
   [hiccup.core :refer [html]]
))

;; TODO: add other languages (English and Spanish)
(defn parse [string]
  (concat
   (fr/parse string)
   (it/parse string)))

(defn expr [id]
  (reader/id2expression (Integer. id)))

;; this does some sample runtime behavior (generates sentences)
;; which allow certain things to get initialized so that remote (i.e. HTTP
;; requests to the same things will not fail with cryptic 'undefined'
;; stack traces.
;(def avoid-init-errors (nounphrase))
(def avoid-init-errors true)

;; TODO: add user convenience functions: functions that might be useful for workbook users.

;; Sandbox specification derived from:
;;    https://github.com/flatland/clojail/blob/4d3f58f69c2d22f0df9f0b843c7dea0c6a0a5cd1/src/clojail/testers.clj#L76
;;    http://docs.oracle.com/javase/6/docs/api/overview-summary.html
;;    http://richhickey.github.com/clojure/api-index.html
(def workbook-sandbox
  (sandbox
   (conj
    clojail.testers/secure-tester-without-def
    (blacklist-nses '[
                      clojure.main
                      java
                      javax
                      org.omg
                      org.w3c
                      org.xml
                      ])
    (blacklist-objects [
                        clojure.lang.Compiler
                        clojure.lang.Ref
                        clojure.lang.Reflector
                        clojure.lang.Namespace
                        clojure.lang.Var clojure.lang.RT
                        ]))
   :refer-clojure false
   ;; using 60000 for development: for production, use much smaller value.
   :timeout 60000
;   :timeout 15000
   :namespace 'babel.workbook))


;; TODO: some exceptions from evaluating a string should be shown to
;; the user for diagnostics rather than just logging them.
;; (e.g. those thrown by (max-lengths).)
(defn workbookq [expr notused]
  (let [expr (string/trim expr)]
    ;; TODO: add timing information for each evaluation.
    (log/info (str "workbookq: evaluating expression: \"" expr "\""))
    (if expr
      (let [output
            (string/join " "
                         (let [loaded
                               (try
                                 (workbook-sandbox (read-string expr))
                                 ;; TODO: how can I show the stack trace for the
                                 ;; attempt to process the expression?
                                 (catch Exception e
                                   (log/error (str "failed to sandbox-load-string: " expr ":" e))
                                   (log/error (str "stack: " (.printStackTrace e)))
                                   (str e)))]
                           (list
                            (str
                             "<div class='evalinput'>"
                             expr
                             "</div>"
                             "<div class='evalresult'>"
                             (cond

                              (set? loaded)
                              (html/tablize loaded)

                              (or (set? loaded) (seq? loaded)(vector? loaded))
                              (str "<ol class='workbook'>"
                                   (string/join " "
                                                (map (fn [elem]
                                                       (str "<li>" (html/tablize elem) "</li>"))
                                                     (seq loaded)))
                                   "</ol>")

                              (= (type loaded)
                                 clojure.lang.LazySeq)
                              (string/join " "
                                           (map (fn [elem]
                                                  (html/tablize elem))
                                                (seq loaded)))

                              (or (list? loaded) (set? loaded)
                                  (= (type loaded) clojure.lang.Cons))
                              (string/join " "
                                           (map (fn [elem]
                                                  (html/tablize elem))
                                                loaded))

                              (= (type loaded) clojure.lang.Var)
                              (str (eval loaded))

                              (and (map? loaded)
                                   (= (keys loaded) '(:plain)))
                              (str "<div style='font-family:monospace'>" (strip-refs (:plain loaded)) "</div>")

                              (map? loaded)
                              (html/tablize loaded)

                              (= (type loaded) nil)
                              (str "<b>nil</b>")
                              :else
                              ;; nothing formattable: just stringify result of
                              ;; evaluation.
                              (str "<div style='font-family:monospace'>" loaded " (<b>" (type loaded) "</b>)" "</div>"))
                             "</div>"))))]
        (log/info (str "workbookq: done evaluating: \"" expr "\""))
        output))))

(defn workbook-ui [request]
  (let [search-query (get (get request :query-params) "search")]
    (html
     [:div#workbook-ui {:class "quiz-elem"}
      [:h2 "Workbook"]
      [:div#searchbar
       [:textarea {:cols 80 :rows 4 :id "workbookq" }
        (if search-query
          search-query
          "(+ 1 1)")
        ]
       [:button {:onclick "workbook()"} "evaluate"]]
      [:div#workbooka
       (if search-query
         (workbookq search-query))]])))

(def routes
  (compojure/routes

   (GET "" request
        {:status 302
         :body (html/page "Workbook" (workbook-ui request) request)
         :headers {"Location" "/workbook/"}})

   (GET "/" request
        {:status 200
         :body (html/page "Workbook" (workbook-ui request) request)
         :headers {"Content-Type" "text/html;charset=utf-8"}})

   (GET "/q/" request
        {:status 200
         :body (workbookq (get (get request :query-params) "search")
                          (get (get request :query-params) "attrs"))
         :headers {"Content-Type" "text/html;charset=utf-8"}})
  ))

;; this def is needed to avoid initialization errors when evaluating within the workbook
;; e.g.: evaluating things like:
;;(generate {:synsem {:subcat '()
;;                                          :infl :imperfect
;;                                          :sem {:subj {:pred :I} :pred :be}}}
;;                                @fr/small)
(def foo
  (do
    (generate {:synsem {:subcat '()
                      :infl :imperfect
                        :sem {:pred :be
                              :subj {:pred :I}}}}
              @fr/small)
    ;; needed to avoid
    ;;INFO  56:12,470 com.mchange.v2.c3p0.C3P0Registry:
    ;; jdk1.5 management interfaces unavailable... JMX support disabled.
    ;; java.security.AccessControlException: access denied
    ;; ("javax.management.MBeanServerPermission" "createMBeanServer")
    (try (expr 0)
         (catch Exception e
           (log/error (str "ignoring exception while trying to do (expr 0): "
                           e))))))

