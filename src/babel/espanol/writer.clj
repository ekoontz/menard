(ns babel.espanol.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.cache :refer (build-lex-sch-cache create-index spec-to-phrases)])
(require '[babel.forest :as forest])
(require '[babel.engine :refer [generate]])
(require '[babel.enrich :refer [enrich]])
(require '[babel.espanol.grammar :refer [small]])
(require '[babel.espanol.lexicon :refer [lexicon]])
(require '[babel.espanol.morphology :as morph])
(require '[babel.espanol.pos :refer :all])
(require '[babel.lexiconfn :refer (compile-lex map-function-on-map-vals unify)])
(require '[babel.log :refer [rlog4j!]])
(require '[babel.parse :as parse])
(require '[babel.ug :refer :all])
(require '[babel.writer :as writer :refer [process write-lexicon]])
(require '[clojure.string :as string])
(require '[clojure.tools.logging :as log])
(require '[clojure.tools.namespace.repl :refer [refresh refresh-all]])
(require '[dag-unify.core :refer (fail? get-in strip-refs)])
(require '[dag-unify.core :as unify])

;; for debugging:
(require '[babel.espanol.morphology.verbs :as esverbs])

(defn expression [& [spec]]
  (let [spec (if spec spec :top)]
    (writer/expression small spec)))

(defn fo [expression]
  (morph/fo expression))

(defn todos [ & [count]]
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
    (write-lexicon "es" @lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating examples with this many verbs:"
                   (.size root-verb-array)))

    ;; for debugging, change (pmap) to (map) so logging is easier to read: in-order rather than interleaved by multiple workers.
    (.size (pmap (fn [verb]
                   (log/trace (str "verb: " (strip-refs verb)))
                   (let [root-form (get-in verb [:espanol :espanol])]
                     (log/info (str "generating with verb: '" root-form "'"))
                     (.size (pmap (fn [tense]
                                   (let [spec (unify {:root {:espanol {:espanol root-form}}}
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
                                                                              "es")

                                                                     ;; TODO: move this to *before*
                                                                     ;; attempting generation that fails.
                                                                     (catch Exception e
                                                                       (cond
                                                                         ;; Ignore the generation-failure exception, if
                                                                         ;; there is a legitimate reason for the exception, such as:

                                                                         ;; The verb is "funzionare" (which takes a non-human
                                                                         ;; subject), but we're trying to generate with
                                                                         ;; {:agr {:person :1st or :2nd}}, for which the only lexemes
                                                                         ;; are human.
                                                                         (and
                                                                          (= (get-in spec [:root :espanol :espanol])
                                                                             "funcionar")
                                                                          (or (= (get-in spec [:comp :synsem :agr :person])
                                                                                 :1st)
                                                                              (= (get-in spec [:comp :synsem :agr :person])
                                                                                 :2nd)))
                                                                         (log/info (str "ignoring exception(funcionar): " e))

                                                                         ;; "llamarse": there is currently only singular
                                                                         ;; proper names, so any attempt to use
                                                                         ;; plural number with this verb will fail.
                                                                         (and
                                                                          (= (get-in spec [:root :espanol :espanol])
                                                                             "llamarse")
                                                                          (or (= (get-in spec [:comp :synsem :agr :number])
                                                                                 :plur)
                                                                              (= (get-in spec [:comp :synsem :agr :gender])
                                                                                 :fem)))
                                                                         (log/info (str "ignoring exception(llamarse): " e))

                                                                         true
                                                                         (do
                                                                           (log/error (str "Stopping further generation: unexpectedly "
                                                                                           "couldn't generate any expression for spec:"
                                                                                           spec))
                                                                           (throw e)))))
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
