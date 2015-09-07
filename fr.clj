;; Command-line usage:
;;
;; lein run -m fr/tout
;; lein run -m fr/tout <num>

(ns fr (:refer-clojure :exclude [get-in]))

(require '[babel.writer :refer [fill-by-spec fill-verb process write-lexicon]])
(require '[babel.english :as en])
(require '[babel.francais :as fr :refer [lexicon]])
(require '[babel.korma :refer :all])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer [get-in unify]])
(require '[korma.core :refer :all])
(require '[korma.db :refer :all])

(defn tout [ & [count]]
  (let [count (if count (Integer. count) 10)]
    (write-lexicon "fr" @fr/lexicon)
    (let [
          ;; subset of the lexicon: only verbs which are infinitives and that can be roots:
          ;; (i.e. those that have a specific (non- :top) value for [:synsem :sem :pred])
          root-verbs 
          (zipmap
           (keys @lexicon)
           (map (fn [lexeme-set]
                  (filter (fn [lexeme]
                            (and
                             (= (get-in lexeme [:synsem :cat]) :verb)
                             (= (get-in lexeme [:synsem :infl]) :top)
                             (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                          lexeme-set))
                (vals @lexicon)))]
      (.size (map (fn [verb]
                    (let [root-form (get-in verb [:français :français])]
                      (log/debug (str "generating from root-form:" root-form))
                      (.size (map (fn [tense]
                                    (let [spec (unify {:root {:français {:français root-form}}}
                                                      tense)]
                                      (log/debug (str "generating from: " spec))
                                      (process [{:fill
                                                 {:spec spec
                                                  :source-model en/small
                                                  :target-model fr/small}
                                                 :count count}])))
                                  [{:synsem {:sem {:tense :conditional}}}
                                   {:synsem {:sem {:tense :future}}}
                                   {:synsem {:sem {:tense :present}}}
                                   ;; TODO: enable this when ready:
                                   ;; {:synsem {:sem {:aspect :perfect
                                   ;;                 :tense :past}}}
                                   ]
                                  ))))
                  (reduce concat
                          (map (fn [key]
                                 (get root-verbs key))
                               (sort (keys root-verbs)))))))))


