(ns babel.english.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.english.grammar :refer [small small-plus-vp-pronoun]])
(require '[babel.english.lexicon :refer [lexicon]])
(require '[babel.writer :refer [delete-from-expressions process write-lexicon]])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer (fail? get-in strip-refs unify)])

(defn all [ & [count]]
  (let [count (if count (Integer. count) 10)
        ;; subset of the lexicon: only verbs which are infinitives and that can be roots:
        ;; (i.e. those that have a specific (non- :top) value for [:synsem :sem :pred])
        root-verbs 
        (zipmap
         (keys @lexicon)
         (map (fn [lexeme-set]
                (filter (fn [lexeme]
                          (and
                           (or true (= (get-in lexeme [:synsem :sem :pred]) :talk)) ;; for development, restrict :pred to a single value.
                           (= (get-in lexeme [:synsem :cat]) :verb)
                           (= (get-in lexeme [:synsem :infl]) :top)
                           (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                        lexeme-set))
              (vals @lexicon)))]

    (write-lexicon "en" @lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating with this many verbs: " (.size (reduce concat (vals root-verbs)))))
    (.size (map (fn [verb]
                  (let [root-form (get-in verb [:english :english])]
                    (log/debug (str "generating from root-form:" root-form))
                    (.size (map (fn [tense]
                                  (let [spec (unify {:root {:english {:english root-form}}}
                                                    tense)]
                                    (.size
                                     (map (fn [gender]
                                            (let [spec (unify spec
                                                              {:comp {:synsem {:agr gender}}})]
                                              (log/trace (str "generating from gender: " gender))
                                              (.size
                                               (map (fn [person]
                                                      (let [spec (unify spec
                                                                        {:comp {:synsem {:agr {:person person}}}})]
                                                        (log/trace (str "generating from person: " person))
                                                        (.size
                                                         (map (fn [number]
                                                                (let [spec (unify spec
                                                                                  {:comp {:synsem {:agr {:number number}}}})]
                                                                  (log/debug (str "generating from spec: " spec))
                                                                  (try
                                                                    (process [{:fill-one-language
                                                                               {:count 1
                                                                                :spec spec
                                                                                :model small
                                                                                }}]
                                                                             "it")
                                                                    (catch Exception e
                                                                      (cond

                                                                       ;; TODO: make this conditional on
                                                                       ;; there being a legitimate reason for the exception -
                                                                       ;; e.g. the verb is "works (nonhuman)" (which takes a non-human
                                                                       ;; subject), but we're trying to generate with
                                                                       ;; {:agr {:person :1st or :2nd}}, for which the only lexemes
                                                                       ;; are human.
                                                                       true

                                                                       (log/warn (str "ignoring exception: " e))
                                                                       false
                                                                       (throw e))))
                                                                  ))
                                                              [:sing :plur]))))
                                                    [:1st :2nd :3rd]))))
                                          (cond (= tense
                                                   {:synsem {:sem {:aspect :perfect
                                                                   :tense :past}}})
                                                [{:gender :masc}
                                                 {:gender :fem}]
                                                true
                                                [:top])))))
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
