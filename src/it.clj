;; Command-line usage:
;;
;; lein run -m it/tout
;; lein run -m it/tout <num>

(ns it)

(require '[babel.writer :refer [fill-by-spec fill-verb write-lexicon]])
(require '[babel.english :as en])
(require '[babel.italiano :as it :refer [verbs]])
(require '[babel.korma :refer :all])
(require '[babel.repair :refer [process]])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer [unify]])
(require '[korma.core :refer :all])
(require '[korma.db :refer :all])

(defn tutti [ & [count]]
  (let [count (if count (Integer. count) 10)]
    (write-lexicon "it" @it/lexicon)
    (process
     (reduce concat
             (map (fn [verb]
                    (map (fn [tense]
                           {:fill
                            {:spec (unify {:root {:italiano {:italiano verb}}}
                                          tense)
                             :source-model en/small
                             :target-model it/small
                             :count count}})
                         (shuffle
                          [
                           {:synsem {:sem {:tense :conditional}}}
                           {:synsem {:sem {:tense :futuro}}}
                           {:synsem {:sem {:tense :present}}}
                           {:synsem {:sem {:aspect :perfect
                                           :tense :past}}}
                           ])
                         ))
                  (sort verbs)
                  )))))

