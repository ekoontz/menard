(ns babel.italiano.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.english.grammar :as en])
(require '[babel.italiano.grammar :refer [small]])
(require '[babel.italiano.lexicon :refer [lexicon]])
(require '[babel.writer :refer [delete-from-expressions process write-lexicon]])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer (fail? get-in strip-refs unify)])
(require '[clojure.string :refer [join]])

(defn rewrite-lexicon []
  (write-lexicon "it" @lexicon))

(defn camminare [ & [count]]
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

    (delete-from-expressions "it" {:root {:italiano {:italiano "camminare"}}})

    (write-lexicon "it" @lexicon)
    (log/info (str "done writing lexicon."))
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
                             (filter #(= % "camminare")
                                     (sort (keys root-verbs)))))))))


(defn alzarsi [ & [count]]
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
              (vals @lexicon)))

        alzarsi
        (reduce concat
                (map (fn [key]
                       (get root-verbs key))
                     (filter #(= % "alzarsi")
                             (sort (keys root-verbs)))))]

    (delete-from-expressions "it" {:root {:italiano {:italiano "alzarsi"}}})

;    (write-lexicon "it" @lexicon)
;    (log/info (str "done writing lexicon."))
    (log/info (str "generating examples with this many verbs:"
                   (.size alzarsi)))
    (.size (map (fn [verb]
                  (log/trace (str "verb: " (strip-refs verb)))
                  (let [root-form (get-in verb [:italiano :italiano])]
                    (log/debug (str "generating from root-form:" root-form))
                    (.size (map (fn [tense]
                                  (let [spec (unify {:root {:italiano {:italiano root-form}}}
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
                                                                  (process [{:fill
                                                                             {:count 1
                                                                              :spec spec
                                                                              :target-model small
                                                                              :source-model en/small-plus-vp-pronoun
                                                                              }}]
                                                                "it")))
                                                              [:sing :plur]))))
                                                    [:1st :2nd :3rd]))))
                                          (cond (= tense
                                                   {:synsem {:sem {:aspect :perfect
                                                                   :tense :past}}})
                                                [{:gender :masc}
                                                 {:gender :fem}]
                                                true
                                                [:top])))))
                                (list ;{:synsem {:sem {:tense :conditional}}}
                                      ;{:synsem {:sem {:tense :future}}}
                                      ;{:synsem {:sem {:tense :present}}}
                                      ;{:synsem {:sem {:aspect :progressive
                                      ;                :tense :past}}}
                                      {:synsem {:sem {:aspect :perfect
                                                      :tense :past}}}
                                      )))))
                alzarsi))))

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

