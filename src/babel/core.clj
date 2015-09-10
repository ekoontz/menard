(ns babel.core
  (:require
   [babel.workbook :as workbook]
   [compojure.core :refer [context defroutes GET PUT POST DELETE ANY]]
   [compojure.handler :as handler]
   [compojure.route :as route]
   [ring.util.response :as resp]))

(defroutes main-routes
  (GET "/" request
       (resp/redirect "/workbook"))
  (context "/workbook" []
           workbook/routes)
  (route/resources "/"))

(def app
  (handler/site 
   main-routes))
