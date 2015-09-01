;; Command line usage: 'lein run -m repair.fr.tout/fix <count>'
(ns repair.fr.tout)
(require '[babel.writer :refer [write-lexicon]])
(require '[babel.english :as en])
(require '[babel.francais :as fr :refer [parse verbs]])
(require '[babel.francais.morphology :as morph :refer [fo]])
(require '[babel.parse :as parse])
(require '[babel.reader :as reader])
(require '[babel.repair :refer [process]])
;; these requires are for debugging/testing.
(require '[clojure.data.json :as json])
(require '[dag-unify.core :refer [unify deserialize]])

(defn fix [& [count]]
  (let [count (if count count 10)]
    (write-lexicon "fr" @fr/lexicon)
    (process
     (reduce concat
             (map (fn [verb]
                    (map (fn [tense]
                           {:fill
                            {:spec (unify {:root {:français {:français verb}}}
                                          tense)
                             :source-model en/small
                             :target-model fr/small
                             :count count}})
                         (shuffle
                          [
                           {:synsem {:sem {:tense :conditional}}}
                           {:synsem {:sem {:tense :futuro}}}
                           {:synsem {:sem {:infl :past}}}
                           {:synsem {:sem {:tense :present}}}
                           ])
                         ))
                  (sort verbs)
                  )))))


