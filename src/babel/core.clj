(ns babel.core
  (:require
   [babel.workbook :as workbook]
   [babel.francais.workbook :as fr]
   [babel.english.workbook :as en]
   [babel.espanol.workbook :as es]
   [babel.italiano.workbook :as it]

   ;; https://github.com/clojure-emacs/cider#installation
   ;; commented out because it's not clear how to use below.
   ;;   [clojure.tools.nrepl.server :as nrepl-server]
   ;;   [cider.nrepl :refer (cider-nrepl-handler)]

   [compojure.core :refer [context defroutes GET PUT POST DELETE ANY]]
   [compojure.handler :as handler]
   [compojure.route :as route]
   [ring.util.response :as resp]))

(defroutes main-routes
  (GET "/" request
       (resp/redirect "/workbook"))
  (context "/workbook/en" []
           en/routes)
  (context "/workbook/es" []
           es/routes)
  (context "/workbook/fr" []
           fr/routes)
  (context "/workbook/it" []
           it/routes)
  (context "/workbook" []
           workbook/routes)
  (route/resources "/"))

(def app
  (handler/site 
   main-routes))
