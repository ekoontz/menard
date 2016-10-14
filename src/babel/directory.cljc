(ns babel.directory
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.engine :as engine]
   [babel.english :as en]
   [babel.espanol :as es]
   [babel.francais :as fr]
   [babel.latin :as la]
   [babel.italiano :as it]
   [dag_unify.core :refer [get-in]]))

(def models
  {"en" (conj (babel.english.grammar/small)
              {:generate-fn (fn [spec]
                              (en/generate spec))})
   "la" babel.latin/model})
                 




  





  
   
   
   
