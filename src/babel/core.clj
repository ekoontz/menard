(ns babel.core
  (:require
   [babel.english.workbook :as en]
   [babel.espanol.workbook :as es]
   [babel.expr :as expr]
   [babel.francais.workbook :as fr]
   [babel.html :as html]
   [babel.italiano.workbook :as it]
   [babel.reader :as reader]
   [babel.say :as say]
   ;; https://github.com/clojure-emacs/cider#installation
   ;; commented out because it's not clear how to use below.
   ;;   [clojure.tools.nrepl.server :as nrepl-server]
   ;;   [cider.nrepl :refer (cider-nrepl-handler)]

   [clojure.tools.logging :as log]
   [compojure.core :refer [context defroutes GET PUT POST DELETE ANY]]
   [compojure.handler :as handler]
   [compojure.route :as route]
   [ring.util.response :as resp]))

(defroutes main-routes
  (GET "/" request
       (resp/redirect "/workbook/it"))
  (context "/say" []
           say/routes)
  (context "/workbook/en" []
           en/routes)
  (context "/workbook/es" []
           es/routes)
  (context "/workbook/fr" []
           fr/routes)
  (context "/workbook/it" []
           it/routes)
  (GET "/expr/:expr" request
       (let [expr (:expr (:route-params request))]
         (log/info (str "expr(1):" expr))
         (log/info (str "expr(2):" (Integer. expr)))
         {:status 200
          :body (html/page "Expr" (html/tablize (reader/id2expression (Integer. expr))))}))
  (context "/expr" []
           expr/routes)

  (route/resources "/"))

(def app
  (handler/site 
   main-routes))
