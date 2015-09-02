(ns es)

(require '[babel.writer :refer [fill-by-spec fill-verb write-lexicon]])
(require '[babel.english :as en])
(require '[babel.espanol :as es :refer [verbs]])
(require '[babel.korma :refer :all])
(require '[babel.repair :refer [process]])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer [unify]])
(require '[korma.core :refer :all])
(require '[korma.db :refer :all])

(defn todos [ & [count]]
  (let [count (if count (Integer. count) 10)]
    (write-lexicon "es" @es/lexicon)
    (process
     (reduce concat
             (map (fn [verb]
                    (map (fn [tense]
                           {:fill
                            {:spec (unify {:root {:espanol {:espanol verb}}}
                                          tense)
                             :source-model en/small
                             :target-model es/small
                             :count count}})
                         (shuffle
                          [
                           {:synsem {:sem {:aspect :perfect
                                           :tense :past}}}
                           {:synsem {:sem {:tense :conditional}}}
                           {:synsem {:sem {:tense :futuro}}}
                           {:synsem {:sem {:tense :present}}}
                           ])
                         ))
                  (sort verbs)
                  )))))

