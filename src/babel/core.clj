(ns babel.core
  (:require
   [babel.workbook :as workbook]
   [compojure.core :refer [context defroutes GET PUT POST DELETE ANY]]
   [compojure.handler :as handler]
   [compojure.route :as route]
   ))

(defroutes main-routes
  (context "/" []
           workbook/routes)
  (route/resources "/"))

(def app
  (handler/site 
   main-routes))
