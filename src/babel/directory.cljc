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
  {"en" (babel.english.grammar/small)
   "es" babel.espanol.grammar/small
   "fr" babel.francais.grammar/medium
   "it" (babel.italiano.grammar/medium)
   "la" babel.latin/model})

(defn generate [language spec]
  (engine/generate spec (get models language)))
                 




  





  
   
   
   
