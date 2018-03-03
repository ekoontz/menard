(ns babel.directory
  (:require [babel.english.grammar :as en]
            [babel.espanol.grammar :as es]
            [babel.francais.grammar :as fr]
            [babel.italiano.grammar :as it]
            [babel.korma :refer [init-db]]
            [babel.latin :as la]
            [clojure.tools.logging :as log]
            [korma.db :refer [transaction]]))
(def models
  {:en (atom (delay (en/model)))
   :es (atom (delay (es/small)))
   :fr (atom (delay (fr/medium)))
   :it (atom (delay (it/model)))
   :la (atom (delay (la/model)))})

(defn refresh-models []
  (do
    (log/info (str "refreshing models.."))
    (swap! (:en models) (fn [old-model] (delay (en/model))))
    (swap! (:es models) (fn [old-model] (delay (es/small))))
    (swap! (:fr models) (fn [old-model] (delay (fr/medium))))
    (swap! (:it models) (fn [old-model] (delay (it/model))))
    (swap! (:la models) (fn [old-model] (delay (la/model))))
    (log/info (str "refreshed models."))))

(defn write-lexicons []
  (init-db)
  (transaction
   (print (str "it.."))
   (println (it/write-lexicon))
   
   (print (str "en.."))
   (println (en/write-lexicon))
   
   (print (str "es.."))
   (println (es/write-lexicon))
   
   (print (str "fr.."))
   (println (fr/write-lexicon))))
