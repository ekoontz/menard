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
  (let [fo (or (:fo (get models language))
               (:morph (get models language))) ;; TODO: adopt :morph or :fo as the standard and stick to it.
        expression (engine/generate spec (get models language))]
    {:surface (fo expression)
     :semantics (get-in expression [:synsem :sem])}))

(defn demo []
  (take 5 (repeatedly #(println 
                        (let [latin (generate "la" {:synsem {:sem {:subj {:pred (first (take 1 (shuffle [:I :tu :lui :lei :noi :voi :loro])))}
                                                                   :tense :past :aspect :progressive}}})]
                          {:latin (:surface latin)
                           :english (:surface (generate "en" {:synsem {:sem (:semantics latin)}}))})))))

                 




  





  
   
   
   
