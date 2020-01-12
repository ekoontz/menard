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
