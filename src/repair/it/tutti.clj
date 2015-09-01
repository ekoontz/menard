;; Command line usage: 'lein run -m repair.it.tout/fix'
(ns repair.it.tutti)
(require '[babel.writer :refer [write-lexicon]])
(require '[babel.english :as en])
(require '[babel.italiano :as it :refer [parse verbs]])
(require '[babel.italiano.morphology :as morph :refer [fo]])
(require '[babel.parse :as parse])
(require '[babel.reader :as reader])
(require '[babel.repair :refer [process]])
;; (morph/fo (deserialize (read-string (:structure (reader/id2expression 45020)))))
;; these requires are for debugging/testing.
(require '[clojure.data.json :as json])
(require '[dag-unify.core :refer [unify deserialize]])

(defn fix [ & [count]]
  (let [count (if count count 10)]
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
                           {:synsem {:sem {:infl :past}}}
                           {:synsem {:sem {:tense :present}}}
                         ])
                         ))
                  (sort verbs)
                  )))))


