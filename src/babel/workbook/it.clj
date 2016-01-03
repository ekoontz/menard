(ns babel.workbook.it
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   [babel.engine :as engine]
   [babel.forest :refer [lightning-bolt]]
   [babel.italiano.grammar :refer :all]
   [babel.italiano.lexicon :refer :all]
   [babel.italiano.morphology :as morph :refer [fo]]
   [babel.italiano.writer :refer [expression]]
   [babel.html :as html]
   [babel.korma :as korma]
   [babel.over :as over]
   [babel.parse :as parse]
   [babel.pos :as pos]
   [babel.reader :as reader]
   [babel.writer :as writer :refer [reload]]
   [clojail.core :refer [sandbox]]
   [clojail.testers :refer :all]
   [clojure.core :exclude [get-in]]
   [clojure.core :as core] ;; This allows us to use core's get-in by doing "(core/get-in ..)"
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [dag_unify.core :refer [fail-path-between get-in remove-false strip-refs unify]]
   [hiccup.core :refer [html]]
))

(defn expr [id]
  (reader/id2expression (Integer. id)))

;; this def is needed to avoid initialization errors when evaluating within the workbook
;; e.g.: evaluating things like:
;;(generate {:synsem {:subcat '()
;;                                          :infl :imperfect
;;                                          :sem {:subj {:pred :I} :pred :be}}}
;;                                fr/small)

(def foo (expression {:synsem {:cat :verb}}))
;(def foo (lightning-bolt nil nil nil))
(def foo2 (expression {:synsem {:sem {:pred :have-fun}}}))

(def rules (:grammar-map medium))

;; TODO: do morphological analysis
;; do find non-infinitives (e.g. find 'parler' given 'parle')
;; and then apply conjugated parts to lexeme
;; i.e. if input is 'parle', return
;; list of lexemes; for each, [:synsem :agr :person] will be
;; 1st, 2nd, or 3rd, and for all, number will be singular.
(defn lookup [lexeme]
  (get (:lexicon medium) lexeme))

(defn over
  ([arg1]
   (over/over (vals (:grammar-map medium)) (lookup arg1)))
  ([grammar arg1]
   (over/over grammar (lookup arg1)))
  ([grammar arg1 arg2]
   (cond (string? arg1)
         (over grammar (lookup arg1)
               arg2)

         (string? arg2)
         (over grammar arg1 (lookup arg2))

         true
         (over/over grammar arg1 arg2))))

;(def fooexpr (expr 1))

;(def foo2 (lookup "je"))
;(def foo3 (lookup "me"))

(def workbook-sandbox-it
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
   :namespace 'babel.workbook.it))


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
                                 (binding [*read-eval* true]
                                   (workbook-sandbox-it (binding [*read-eval* true] (read-string expr))))
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
      [:h2 "Italian Workbook"]

      [:div.hints
       [:h3 "Try:"]
       [:div "(expr X)"]
       [:div "(parse 'je parle')"]

       ]

      [:div#searchbar
       [:textarea {:cols 50 :rows 4 :id "workbookq" }
        (if search-query
          search-query
          "(+ 1 1)")
        ]
       [:button {:onclick "workbook('/workbook/it')"} "evaluate"]]
      [:div#workbooka
       (if search-query
         (workbookq search-query))]])))

(def routes
  (compojure/routes

   (GET "" request
        {:status 302
         :headers {"Location" "/workbook/"}})

   (GET "/" request
        {:status 200
         :body (html/page "Italian Workbook" (workbook-ui request) request)
         :headers {"Content-Type" "text/html;charset=utf-8"}})

   (GET "/q/" request
        {:status 200
         :body (workbookq (get (get request :query-params) "search")
                          (get (get request :query-params) "attrs"))
         :headers {"Content-Type" "text/html;charset=utf-8"}})
  ))

(defn generate [spec]
  (engine/generate spec medium))
