(ns babel.italiano.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.english.grammar :as en])
(require '[babel.italiano.grammar :refer [small]])
(require '[babel.italiano.lexicon :refer [lexicon]])
(require '[babel.writer :refer [delete-from-expressions process write-lexicon]])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer (fail? get-in strip-refs unify)])

(defn tutti [ & [count]]
  (let [count (if count (Integer. count) 10)
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
    (if false
      (delete-from-expressions "it" {:synsem {:sem {:subj {:pred :io}}}}))

    (write-lexicon "it" @lexicon)
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
                                                :source-model en/small-plus-vp-pronoun
                                                :target-model small
                                                :count count}}] "it")))
                                (list {:synsem {:sem {:tense :conditional}}}
                                      {:synsem {:sem {:tense :future}}}
                                      {:synsem {:sem {:tense :present}}}
                                      {:synsem {:sem {:aspect :progressive
                                                      :tense :past}}}
                                      {:synsem {:sem {:aspect :perfect
                                                      :tense :past}}})))))
                (reduce concat
                        (map (fn [key]
                               (get root-verbs key))
                             (sort (keys root-verbs))))))))

