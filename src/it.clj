;; Command-line usage:
;;
;; lein run -m it/tutti
;; lein run -m it/tutti <num>

(ns it
  (:refer-clojure :exclude [get-in]))

(require '[babel.writer :refer [fill-by-spec fill-verb write-lexicon]])
(require '[babel.english :as en])
(require '[babel.italiano :as it :refer [lexicon]])
(require '[babel.korma :refer :all])
(require '[babel.repair :refer [process]])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer [get-in unify]])
(require '[korma.core :refer :all])
(require '[korma.db :refer :all])

;; subset of the lexicon: only verbs which are infinitives and that can be roots:
;; (i.e. those that have a specific (non- :top) value for [:synsem :sem :pred])
(def root-verbs
  (zipmap
   (keys @lexicon)
   (map (fn [lexeme-set]
          (filter (fn [lexeme]
                    (and
                     (= (get-in lexeme [:synsem :cat]) :verb)
                     (= (get-in lexeme [:synsem :infl]) :top)
                     (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                  lexeme-set))
        (vals @lexicon))))

(defn tutti [ & [count]]
  (let [count (if count (Integer. count) 10)]
    (write-lexicon "it" @it/lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating with this many verbs: " (.size (reduce concat (vals root-verbs)))))
    (.size (map (fn [verb]
                  (let [root-form (get-in verb [:italiano :italiano])]
                    (log/debug (str "generating from root-form:" root-form))
                    (.size (map (fn [tense]
                                  (let [spec (unify {:root {:italiano {:italiano root-form}}}
                                                    tense)]
                                    (log/debug (str "generating from: " spec))
                                    (process [{:fill
                                               {:spec spec
                                                :source-model en/small
                                                :target-model it/small}
                                               :count count}])))
                                (list {:synsem {:sem {:tense :conditional}}}
                                      {:synsem {:sem {:tense :futuro}}}
                                      {:synsem {:sem {:tense :present}}}
                                      {:synsem {:sem {:aspect :perfect
                                                      :tense :past}}})))))
                (reduce concat
                        (map (fn [key]
                               (get root-verbs key))
                             (sort (keys root-verbs))))))))

