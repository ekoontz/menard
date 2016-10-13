(ns babel.directory
  (:require
   [babel.english :as en]
   [babel.espanol :as es]
   [babel.francais :as fr]
   [babel.latin :as la]
   [babel.italiano :as it]))

(def models
  {"en" (babel.english.grammar/small)
   "es" babel.espanol.grammar/small
   "fr" babel.francais.grammar/medium
   "it" (babel.italiano.grammar/medium)
   "la" babel.latin/model})






  
   
   
   
