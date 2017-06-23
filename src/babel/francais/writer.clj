(ns babel.francais.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.generate :as generate])
(require '[babel.francais.grammar :refer [small medium]])
(require '[babel.francais.lexicon :refer [lexicon]])
(require '[babel.francais.morphology :refer [analyze exception-generator
                                             fo]])
(require '[babel.francais.pos :refer [intransitivize transitivize]])
(require '[babel.korma :refer [init-db]])
(require '[babel.writer :as writer :refer [process reload write-lexicon]])
(require '[clojure.tools.logging :as log])
(require '[dag_unify.core :refer (fail? get-in strip-refs unify)])

(defn rewrite-lexicon []
  (write-lexicon "fr" lexicon))

(defn tout [ & [count]]
  (let [use-map-fn pmap
        count (if count (Integer. count) 10)
        root-verbs 
        (zipmap
         (keys lexicon)
         (use-map-fn
          (fn [lexeme-set]
            (filter (fn [lexeme]
                      (and
                       ;; uncomment to only generate for a desired verb, e.g. "se amuser"
                       ;;                           (= (get-in lexeme [:français :français]) "se amuser")
                       (= (get-in lexeme [:synsem :cat]) :verb)
                       (= (get-in lexeme [:synsem :infl]) :top)
                       (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                    lexeme-set))
          (vals lexicon)))
        root-verb-array
        (reduce concat
                (use-map-fn (fn [key]
                              (get root-verbs key))
                            (sort (keys root-verbs))))]
    (init-db)
    (write-lexicon "fr" lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating examples with this many verbs:"
                   (.size root-verb-array)))

    ;; for debugging, change (pmap) to (map) so logging is easier to read: in-order rather than interleaved by multiple workers.
    (doall (use-map-fn
            (fn [verb]
              (log/trace (str "verb: " (strip-refs verb)))
              (let [root-form (get-in verb [:français :français])
                    essere (get-in verb [:synsem :essere])
                    medium (medium)]
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
                                                                              {:comp {:synsem {:agr {:number number}}}
                                                                               :synsem {:subcat '()}})
                                                                  spec (dissoc spec
                                                                               :dag_unify.core/serialized)]
                                                              (log/debug (str "generating from spec: " spec))
                                                              (try
                                                                (process [{:fill-one-language
                                                                           {:count 1
                                                                            :spec spec
                                                                            :model medium
                                                                            }}]
                                                                         "fr")
                                                                ;; TODO: move this to *before*
                                                                ;; attempting generation that fails.
                                                                (catch Exception e
                                                                  (cond
                                                                        
                                                                    ;; "se appeler": there is currently only singular
                                                                    ;; proper male names, so any attempt to use
                                                                    ;; plural number with this verb will fail.
                                                                    ;; Also verb is constrained in lexicon
                                                                    ;; to only be present progressive.
                                                                    (and
                                                                     (= (get-in spec [:root :français :français])
                                                                        "s'appeler")
                                                                     (or (= (get-in spec [:comp :synsem :agr :number])
                                                                            :plur)
                                                                         (= (get-in spec [:comp :synsem :agr :gender])
                                                                            :fem)
                                                                         (and (not (= (get-in spec [:synsem :sem :tense])
                                                                                      :present))
                                                                              (not (= (get-in spec [:synsem :sem :aspect])
                                                                                      :progressive)))))
                                                                    (do (log/info (str "ignoring exception (se-appeler): " e)))
                                                                    
                                                                    true
                                                                    (throw e))))
                                                              ))
                                                          [:sing :plur]))))
                                                [:1st :2nd :3rd]))))
                                      (cond (and (= true essere)
                                                 (= tense
                                                    {:synsem {:sem {:aspect :perfect
                                                                    :tense :past}}}))
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

