(ns babel.directory
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.engine :as engine]
   [babel.english :as en]
   [babel.espanol :as es]
   [babel.francais :as fr]
   [babel.latin :as la]
   [babel.italiano :as it]
   [clojure.tools.logging :as log]
   [dag_unify.core :refer [get-in]]))

(def models
  (let [en (promise)
        es (promise)
        fr (promise)
        la (promise)
        it (promise)]
    {:en (fn []
           (if (realized? en)
             en
             (deliver en 
                      (do (log/info (str "starting to load english model!"))
                          (let [model (babel.english.grammar/small)]
                            (log/info (str "finished loading english model!"))
                            (conj model
                                  {:generate-fn (fn [spec]
                                                  (en/generate spec :model model))}))))))
     
     :fr (fn []
           (if (realized? fr)
             fr
             (deliver fr
                      (do (log/info (str "starting to load French model!"))
                          (let [model (babel.francais.grammar/medium)]
                            (log/info (str "finished loading French model!"))
                            (conj model
                                  {:generate-fn (fn [spec]
                                                  (fr/generate spec :model model))}))))))
     :la (fn []
           (if (realized? la)
             la
             (deliver la
                      (do (log/info (str "starting to load latin model!"))
                          (let [model (la/model)]
                            (log/info (str "finished loading latin model!"))
                            (conj model
                                  {:generate-fn (fn [spec]
                                                  (la/generate spec model))}))))))
                      
     :it (fn []
           (if (realized? it)
             it
             (deliver it
                      (let [model (babel.italiano.grammar/small)]
                        (conj model
                              {:generate-fn (fn [spec]
                                              (it/generate spec :model model))})))))}))



