(ns babel.espanol.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.directory :refer [models]])
(require '[babel.espanol.grammar :refer [parse]])
(require '[babel.espanol.morphology :as morph])
(require '[babel.espanol.pos :refer :all])
(require '[babel.generate :refer [generate]])
(require '[babel.lexiconfn :refer (compile-lex map-function-on-map-vals)])
(require '[babel.log :refer [log4j!]])
(require '[babel.parse :as parse])
(require '[babel.ug :refer :all])
(require '[babel.writer :as writer :refer [process reload write-lexicon]])
(require '[clojure.string :as string])
(require '[clojure.tools.logging :as log])
(require '[dag_unify.core :refer (fail? get-in strip-refs)])
(require '[dag_unify.core :as unify :refer [unify]])

;; for debugging:
(require '[babel.espanol.morphology.verbs :as esverbs])

;; for debugging: use map: in-order rather than interleaved by multiple workers.
;; TODO: using pmap fails: cannot generate sentences; figure out why.
(def use-map-fn pmap)

(def lexicon (-> (-> ((-> models :es)) deref) :lexicon))

(defn root-verbs [lexicon]
  (let [lexemes (vals lexicon)]
    (zipmap
     (keys lexicon)
     (use-map-fn (fn [lexeme-set]
                   (filter (fn [lexeme]
                             (and
                              ;; how to only generate for a single infinitive (for testing/development):
                              ;;                           (= (get-in lexeme [:espanol :espanol]) "abandonar")
                                  (= (get-in lexeme [:synsem :cat]) :verb)
                                  (= (get-in lexeme [:synsem :infl]) :top)
                                  (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                           lexeme-set))
                 lexemes))))

(defn todos [ & [count lexeme]]
  (let [count (if count (Integer. count) 10)
        lexemes (if lexeme (list (get lexicon lexeme))
                    (vals lexicon))

        model (-> ((-> models :es)) deref)
        type-of-model (type model)

        root-verbs (root-verbs lexicon)

        root-verb-array
        (reduce concat
                (use-map-fn (fn [key]
                              (get root-verbs key))
                            (if lexeme
                              [lexeme]
                              (sort (keys root-verbs)))))]
    (write-lexicon "es" lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating examples with this many verbs:"
                   (.size root-verb-array)))

    (.size
     (->> root-verb-array
          (use-map-fn
          (fn [verb]
            (log/debug (str "verb: " (strip-refs verb)))
            (let [root-form (get-in verb [:espanol :espanol])]
              (log/info (str "generating with verb: '" root-form "'"))
              (doall (use-map-fn
                      (fn [tense]
                        (let [debug (log/debug (str "{:root {:espanol {:espanol " root-form))
                              spec (unify {:root {:espanol {:espanol root-form}}}
                                          tense)]
                          (doall
                           (use-map-fn
                            (fn [gender]
                              (let [spec (dissoc (unify spec
                                                        {:comp {:synsem {:agr gender}}})
                                                 :dag_unify.core/serialized)]
                                (log/debug (str "generating from gender: " gender " with spec: " spec))
                                (doall
                                 (use-map-fn
                                  (fn [person]
                                    (let [debug (log/debug (str "generating from spec(pre-person):" spec))
                                          unify-with {:comp {:synsem {:agr {:person person}}}}
                                          unified (unify spec unify-with)
                                          debug (if (fail? unified)
                                                  (do
                                                    (log/warn (str "spec=fail:tried to generate with spec(pre-person):" spec))
                                                    (log/warn (str "spec=fail:tried to generate with unify-with      : " unify-with))))
                                          spec
                                          (if (fail? unified)
                                            (let [message (str "could not unify " spec " with person specification:" unify-with ".")]
                                              (cond
                                                (and (= (get-in spec [:comp :synsem :agr :person])
                                                        :3rd)
                                                     (= (get-in spec [:comp :synsem :pronoun])
                                                        false)
                                                     false)
                                                (do
                                                  (log/debug message)
                                                  (log/debug (str "Ignoring and continuing."))
                                                  :fail)
                                                true
                                                (do (log/error message)
                                                    (log/error "Stopping further processing now.")
                                                    (throw (Exception. message)))))
                                            unified)
                                          ]
                                      (log/debug (str "generating from person: " person))
                                      (log/debug (str "generating from spec+person: " spec))
                                      (if (fail? spec)
                                        (throw (Exception. (str "spec+person is fail!"))))
                                      (doall
                                       (use-map-fn
                                        (fn [number]
                                          (let [debug (log/debug (str "generating from spec(1): " spec))
                                                spec (unify spec
                                                            {:comp {:synsem {:agr {:number number}}}})]
                                            (log/debug (str "generating from spec(2): " spec))
                                            (try
                                              (if (fail? spec)
                                                (do
                                                  (if true
                                                    (throw (Exception. (str "spec is fail!")))
                                                    (log/debug (str "ignoring failed spec."))))
                                                (process [{:fill-one-language
                                                           {:count 1
                                                            :spec spec
                                                            :model model
                                                            }}]
                                                         "es"))
                                              
                                              ;; TODO: move this to *before*
                                              ;; attempting generation that fails.
                                              (catch Exception e
                                                (cond
                                                  ;; Ignore the generation-failure exception, if
                                                  ;; there is a legitimate reason for the exception, such as:
                                        ;
                                                  ;; The spec was :fail.
                                                  (= spec :fail)
                                                  (if false
                                                    (log/warn (str "spec is fail!"))
                                                    (let [message "spec is fail!"]
                                                      (log/error message)
                                                      (throw (Exception. message))))
                                                  
                                                  ;; The verb is "funcionar" (which takes a non-human
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
                                                  
                                                  
                                                  ;; The verb is "funcionar" (which takes a non-human
                                                  ;; subject), but we're trying to generate a non-pronoun complement;
                                                  ;; there are no such single words in the lexicon that are both
                                                  ;; non-human and non-pronoun. There are *human* non-pronouns in the lexicon,
                                                  ;; however: proper names like "Juan". If we added a place (e.g. "Madrid"), then
                                                  ;; we would have a non-human non-pronoun in the lexicon.
                                                  (and
                                                   (= (get-in spec [:root :espanol :espanol])
                                                      "funcionar")
                                                   (= (get-in spec [:comp :synsem :pronoun])
                                                      false))
                                                  (log/info (str "ignoring exception(funcionar-is-only-pronoun): " e))
                                                  
                                                  ;; "llamarse": there is currently only singular
                                                  ;; proper singular names, so any attempt to use
                                                  ;; plural number with this verb will fail.
                                                  (and
                                                   (= (get-in spec [:root :espanol :espanol])
                                                      "llamarse")
                                                   (or (= (get-in spec [:comp :synsem :agr :number])
                                                          :plur)
                                                       (and (not (= (get-in spec [:synsem :sem :tense])
                                                                    :present))
                                                            (not (= (get-in spec [:synsem :sem :aspect])
                                                                    :progressive)))
                                                       (and (= (get-in spec [:synsem :sem :tense])
                                                               :past)
                                                            (= (get-in spec [:synsem :sem :aspect])
                                                               :progressive))
                                                       (and (= (get-in spec [:synsem :sem :tense])
                                                               :present)
                                                            (= (get-in spec [:synsem :sem :aspect])
                                                               :perfect))))

                                                  (log/info (str "ignoring exception(llamarse-is-only-singular and present): " e))
                                                  
                                                  ;; "llamarse": subject must be a pronoun
                                                  (and
                                                   (= (get-in spec [:root :espanol :espanol])
                                                      "llamarse")
                                                   (= (get-in spec [:comp :synsem :pronoun])
                                                      false))
                                                  (log/info (str "ignoring exception(llamarse-requires-pronoun-as-subject): " e))
                                                  
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
                                         ;; don't generate with null-pronouns - too confusing and inconsistent.
                                        ;                                           (unify tense
                                        ;                                                        {:comp {:synsem {:pronoun true
                                        ;                                                                         :null-pronoun true}}})
                                         (unify tense
                                                {:comp {:synsem {:pronoun true
                                                                 :null-pronoun false}}})
;                                         (unify tense
;                                                {:comp {:synsem {:agr {:person :3rd
;                                                                       :number :sing}
;                                                                 :pronoun false}}})

                                         )))
                              (vals babel.espanol.grammar/tenses)))))))))))
