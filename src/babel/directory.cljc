(ns babel.directory
  (:require [babel.english.grammar :as en]
            [babel.espanol.grammar :as es]
            [babel.francais.grammar :as fr]
            [babel.italiano.grammar :as it]
            [babel.italiano.lexicon :as it-lex]
            [babel.korma :as db]
            [babel.latin :as la]
            [babel.lexiconfn :refer [write-lexicon]]
            [clojure.tools.logging :as log]
            [korma.db :refer [transaction]]))
(def models
  {:en (atom (delay (en/model)))
   :es (atom (delay (es/small)))
   :fr (atom (delay (fr/model)))
   :it (atom (delay (it/model)))
   :la (atom (delay (la/model)))})

(defn refresh-models []
  (do
    (log/info (str "refreshing models.."))
    (swap! (:en models) (fn [old-model] (delay (en/model))))
    (swap! (:es models) (fn [old-model] (delay (es/small))))
    (swap! (:fr models) (fn [old-model] (delay (fr/model))))
    (swap! (:it models) (fn [old-model] (delay (it/model))))
    ;; (swap! (:it models) (fn [old-model] (delay (grammar/model-reloaded))))

    (swap! (:la models) (fn [old-model] (delay (la/model))))
    (log/info (str "refreshed models."))))

(defn write-lexicons []
  (db/init-db)
  (transaction
   (print (str "it.."))
   (println (write-lexicon "it" (it-lex/compile-lexicon)))
   
   (print (str "en.."))
   (println (write-lexicon "en" (en/compile-lexicon)))
   
   (print (str "es.."))
   (println (write-lexicon "es" (es/compile-lexicon)))
   
   (print (str "fr.."))
   (println (write-lexicon "fr" (fr/compile-lexicon)))))

