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

;; babel.directory provides a centralized way to discover and access language models.
;; 
;; To sync your language model after changing lexicon sources:
;;
;; 1. Write .edn source files to database:
;;     (write-lexicon "it" (babel.italiano.lexicon/compile-lexicon))
;; 2. Update in-memory models from database:
;;    (babel.directory/refresh-models)
;; 3. Make model easily available within your own namespace:
;;    (def model @@(get models :it))
;; 4. Use new model via your local variable 'model':
;;    (-> model (get :lexicon) (get "uomo") first)

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
    (swap! (:la models) (fn [old-model] (delay (la/model))))
    (log/info (str "refreshed models."))))

(defn write-lexicons []
  (db/init-db)
  (transaction
   
   (print (str "en.."))
   (println (write-lexicon "en" (en/compile-lexicon)))
   
   (print (str "es.."))
   (println (write-lexicon "es" (es/compile-lexicon)))
   
   (print (str "fr.."))
   (println (write-lexicon "fr" (fr/compile-lexicon)))

   (print (str "it.."))
   (println (write-lexicon "it" (it-lex/compile-lexicon)))))
