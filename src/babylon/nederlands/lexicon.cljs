(ns babylon.nederlands.lexicon
  (:require-macros [babylon.nederlands])
  (:require [babylon.nederlands :as nl]
            [cljslog.core :as log]))

(def lexicon-atom (atom nil))

(defn lexicon []
  (if (nil? @lexicon-atom)
    (do (swap! lexicon-atom
               (fn []
                 (-> (nl/read-compiled-lexicon)
                     babylon.lexiconfn/deserialize-lexicon              
                     vals
                     flatten)))
        @lexicon-atom)
    @lexicon-atom))

(def lexeme-map-atom (atom nil))

;; note that we exclude [:exception]s from the lexemes that we use for
;; generation since they are only to be used for parsing.
;; TODO: this is duplicated in babylon/nederlands.cljc (see def verb-lexicon).
(defn lexeme-map []
    (log/info (str "inside the lexeme-map function...(in babylon)!!"))
    (if (nil? @lexeme-map-atom)
      (do (swap! lexeme-map-atom
                 (fn []
                   {:verb (->> (lexicon)
                               (filter #(= :verb (u/get-in % [:cat])))
                               (filter #(not (u/get-in % [:exception]))))
                    :det (->> (lexicon)
                              (filter #(= :det (u/get-in % [:cat]))))
                    :intensifier (->> (lexicon)
                                      (filter #(= :intensifier (u/get-in % [:cat]))))
                    :noun (->> (lexicon)
                               (filter #(= :noun (u/get-in % [:cat])))
                               (filter #(not (u/get-in % [:exception]))))
                    :top (lexicon)
                    :adjective (->> (lexicon)                                                          
                                    (filter #(= :adjective (u/get-in % [:cat]))))})))
      @lexeme-map-atom))

