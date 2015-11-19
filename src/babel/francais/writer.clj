(ns babel.francais.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.francais.grammar :refer [small medium]])
(require '[babel.francais.lexicon :refer [lexicon]])
(require '[babel.francais.morphology :refer [analyze exception-generator
                                             fo]])
(require '[babel.francais.pos :refer [intransitivize transitivize]])
(require '[babel.writer :as writer :refer [process reload write-lexicon]])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer (fail? get-in strip-refs unify)])

(defn rewrite-lexicon []
  (write-lexicon "fr" @lexicon))

(defn expression [& [spec]]
  (let [spec (if spec spec :top)]
    (writer/expression medium spec)))

(defn tout [ & [count]]
  (let [count (if count (Integer. count) 10)
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
        root-verb-array
        (reduce concat
                (map (fn [key]
                       (get root-verbs key))
                     (sort (keys root-verbs))))]
    (write-lexicon "fr" @lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating examples with this many verbs:"
                   (.size root-verb-array)))

    ;; for debugging, change (pmap) to (map) so logging is easier to read: in-order rather than interleaved by multiple workers.
    (.size (pmap (fn [verb]
                   (log/trace (str "verb: " (strip-refs verb)))
                   (let [root-form (get-in verb [:français :français])]
                     (log/info (str "generating with verb: '" root-form "'"))
                     (.size (map (fn [tense]
                                   (let [spec (unify {:root {:français {:français root-form}}}
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
                                                                                 :model medium
                                                                                 }}]
                                                                              "fr")
                                                                     (catch Exception e
                                                                       (cond
                                                                        
                                                                        ;; TODO: make this conditional on
                                                                        ;; there being a legitimate reason for the exception -
                                                                        ;; e.g. the verb is "funzionare" (which takes a non-human
                                                                        ;; subject), but we're trying to generate with
                                                                        ;; {:agr {:person :1st or :2nd}}, for which the only lexemes
                                                                        ;; are human.
                                                                        false
                                                                        
                                                                        (log/warn (str "ignoring exception: " e))
                                                                        true
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
                                       {:synsem {:sem {:aspect :perfect
                                                       :tense :past}}}
                                       )))))
                 root-verb-array))))
