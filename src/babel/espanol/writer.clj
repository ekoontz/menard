(ns babel.espanol.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.cache :refer (build-lex-sch-cache create-index spec-to-phrases)])
(require '[babel.forest :as forest])
(require '[babel.engine :as engine :refer [generate]])
(require '[babel.enrich :refer [enrich]])
(require '[babel.espanol.grammar :refer [small]])
(require '[babel.espanol.lexicon :refer [lexicon]])
(require '[babel.espanol.morphology :as morph])
(require '[babel.espanol.pos :refer :all])
(require '[babel.lexiconfn :refer (compile-lex map-function-on-map-vals unify)])
(require '[babel.log :refer [log4j!]])
(require '[babel.parse :as parse])
(require '[babel.ug :refer :all])
(require '[babel.writer :as writer :refer [process reload write-lexicon]])
(require '[clojure.string :as string])
(require '[clojure.tools.logging :as log])
(require '[dag_unify.core :refer (fail? get-in strip-refs)])
(require '[dag_unify.core :as unify])

;; for debugging:
(require '[babel.espanol.morphology.verbs :as esverbs])

(defn expression [& [spec]]
  (let [spec (if spec spec :top)]
    (engine/expression small spec)))

(defn fo [expression]
  (morph/fo expression))

(defn todos [ & [count lexeme]]
  (let [count (if count (Integer. count) 10)
        lexemes (if lexeme (list (get lexicon lexeme))
                    (vals lexicon))
        root-verbs 
        (zipmap
         (keys lexicon)
         (map (fn [lexeme-set]
                (filter (fn [lexeme]
                          (and
                           ;; how to only generate for a single infinitive (for testing/development):
                           ;;                           (= (get-in lexeme [:espanol :espanol]) "abandonar")
                           (= (get-in lexeme [:synsem :cat]) :verb)
                           (= (get-in lexeme [:synsem :infl]) :top)
                           (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                        lexeme-set))
              lexemes))
        root-verb-array
        (reduce concat
                (map (fn [key]
                       (get root-verbs key))
                     (sort (keys root-verbs))))]
    (write-lexicon "es" lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating examples with this many verbs:"
                   (.size root-verb-array)))

    ;; for debugging, change (pmap) to (map) so logging is easier to read: in-order rather than interleaved by multiple workers.
    (.size (pmap (fn [verb]
                   (log/debug (str "verb: " (strip-refs verb)))
                   (let [root-form (get-in verb [:espanol :espanol])]
                     (log/info (str "generating with verb: '" root-form "'"))
                     (.size (pmap (fn [tense]
                                    (let [spec (unify/unify {:root {:espanol {:espanol root-form}}}
                                                            tense)]
                                     (.size
                                      (map (fn [gender]
                                             (let [spec (unify/unify spec
                                                                     {:comp {:synsem {:agr gender}}})]
                                               (log/debug (str "generating from gender: " gender))
                                               (.size
                                                (map (fn [person]
                                                       (let [debug (log/debug (str "generating from spec(pre-person):" spec))
                                                             unify-with {:comp {:synsem {:agr {:person person}}}}
                                                             unified (unify/unify spec unify-with)
                                                             
                                                             spec
                                                             (if (fail? unified)
                                                               (let [message
                                                                     (log/warn (str "could not unify " spec " with person specification:" unify-with "."))]
                                                                 (if true
                                                                   (do
                                                                     (log/warn message)
                                                                     (log/warn (str "Ignoring and continuing."))
                                                                     unified)
                                                                   (do
                                                                     (log/error message)
                                                                     (log/error "Stopping further processing now.")
                                                                     (throw (Exception. message)))))
                                                               unified)

                                                             ]
                                                         (log/debug (str "generating from person: " person))
                                                         (log/debug (str "generating from spec+person: " spec))
                                                         (.size
                                                          (map (fn [number]
                                                                 (let [debug (log/debug (str "generating from spec(1): " spec))
                                                                       spec (unify/unify spec
                                                                                         {:comp {:synsem {:agr {:number number}}}})]
                                                                   (log/debug (str "generating from spec(2): " spec))
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
                                        ;
                                                                         ;; The spec was :fail.
                                                                         (= spec :fail)
                                                                         (if true
                                                                           (log/warn (str "ignoring spec: :fail : can't generate anything from that."))
                                                                           (let [message "spec is fail!"]
                                                                             (log/error message)
                                                                             (throw (Exception. message))))
                                                                         
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
                                                                         (log/info (str "ignoring exception(funcionar-is-only-nonhuman): " e))

                                                                         ;; "llamarse": there is currently only singular
                                                                         ;; proper male names, so any attempt to use
                                                                         ;; plural number with this verb will fail.
                                                                         (and
                                                                          (= (get-in spec [:root :espanol :espanol])
                                                                             "llamarse")
                                                                          (or (= (get-in spec [:comp :synsem :agr :number])
                                                                                 :plur)
                                                                              (= (get-in spec [:comp :synsem :agr :gender])
                                                                                 :fem)
                                                                              (and (not (= (get-in spec [:synsem :sem :tense])
                                                                                           :present))
                                                                                   (not (= (get-in spec [:synsem :sem :aspect])
                                                                                           :progressive)))
                                                                              (and (= (get-in spec [:synsem :sem :tense])
                                                                                      :past)
                                                                                   (= (get-in spec [:synsem :sem :aspect])
                                                                                      :progressive))))
                                                                         (log/info (str "ignoring exception(llamarse-is-only-singular and present): " e))

                                                                         true
                                                                         (do
                                                                           (log/error (str "Stopping further generation: unexpectedly "
                                                                                           "couldn't generate any expression for spec:"
                                                                                           spec))
                                                                           (throw e)))))
                                                                   ))
                                                               [:sing :plur]))))
                                                     [:1st :2nd :3rd]))))
                                           ;; TODO: why is this check disabled with (or true ..)?
                                           (cond (or true (= tense
                                                             {:synsem {:sem {:aspect :perfect
                                                                             :tense :past}}}))
                                                 [{:gender :masc}
                                                  {:gender :fem}]
                                                 true
                                                 [:top])))))
                                  (mapcat (fn [tense]
                                            (filter #(not (= :fail %))
                                                    (list
                                                     (unify/unify tense
                                                                  {:comp {:synsem {:pronoun true
                                                                                   :null-pronoun true}}})
                                                     (unify/unify tense
                                                                  {:comp {:synsem {:pronoun true
                                                                                   :null-pronoun false}}})
                                                     (unify/unify tense
                                                                  {:comp {:synsem {:agr {:person :3rd
                                                                                         :number :sing}
                                                                                   :pronoun false}}}))))
                                          (list {:synsem {:sem {:tense :conditional}}}
                                                {:synsem {:sem {:tense :future}}}
                                                {:synsem {:sem {:tense :present}}}
                                                {:synsem {:sem {:aspect :perfect
                                                                :tense :past}}}
                                                {:synsem {:sem {:aspect :perfect
                                                                :tense :past}}}

                                                ))


                                 ))))
                 root-verb-array))))
