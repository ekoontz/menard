;; Command-line usage:
;;
;; lein run -m fr/tout
;; lein run -m fr/tout <num>

(ns fr)

(require '[babel.writer :refer [fill-by-spec fill-verb write-lexicon]])
(require '[babel.english :as en])
(require '[babel.francais :as fr :refer [verbs]])
(require '[babel.korma :refer :all])
(require '[babel.repair :refer [process]])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer [unify]])
(require '[korma.core :refer :all])
(require '[korma.db :refer :all])

(defn tout [ & [count]]
  (let [count (if count (Integer. count) 10)]
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
                           {:synsem {:sem {:tense :present}}}

                           ;; TODO: enable this when ready:
                           ;; {:synsem {:sem {:aspect :perfect
                           ;;                 :tense :past}}}

                           ])
                         ))
                  (sort verbs)
                  )))))

