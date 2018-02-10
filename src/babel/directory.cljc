(ns babel.directory
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.english :as en]
   [babel.espanol :as es]
   [babel.francais :as fr]
   [babel.latin :as la]
   [babel.italiano :as it]
   [clojure.tools.logging :as log]
   [dag_unify.core :refer [get-in unify]]))

(def models
  {:en (delay
        (do (log/debug (str "starting to load english model.."))
            (let [model (babel.english.grammar/medium)]
              (log/info (str "finished loading english model: "
                             (:name model)))
              (merge
               model
               {:semantic-correspondence {:it [[:obj :null]
                                               [:obj :number]
                                               [:shared-with-obj]
                                               [:subj :null]
                                               [:subj :number]]}}))))
   :es (delay
        (do (log/debug (str "starting to load Español model.."))
            (let [model (babel.espanol.grammar/small)]
              (log/debug (str "finished loading Espanol model."))
              (conj model
                    {:generate-fn (fn [spec]
                                    (es/generate spec :model model))
                     :tenses babel.espanol.grammar/tenses}))))
   :fr (delay
        (do (log/debug (str "starting to load French model.."))
            (let [model (babel.francais.grammar/medium)]
              (log/debug (str "finished loading French model."))
              (conj model
                    {:generate-fn (fn [spec]
                                    (fr/generate spec :model model))
                     :tenses babel.francais.grammar/tenses}))))
   :la (delay
        (do (log/info (str "starting to load latin model.."))
            (let [model (la/model)]
              (log/info (str "finished loading latin model."))
              (conj model
                    {:generate-fn (fn [spec]
                                    (la/generate spec model))}))))
   :it (delay
        (let [model (babel.italiano.grammar/medium)]
          (conj model
                {:generate-fn (fn [spec]
                                (it/generate spec :model model))
                 :tenses babel.italiano.grammar/tenses})))})
