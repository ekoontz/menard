(ns babel.italiano.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.cache :refer (create-index)])
(require '[babel.english.writer :as en])
(require '[babel.enrich :refer [enrich]])
(require '[babel.forest :as forest])
(require '[babel.italiano.grammar :as gram])
(require '[babel.italiano.lexicon :as lex])
(require '[babel.italiano.morphology :as morph :refer [fo]])
(require '[babel.italiano.pos :refer [intransitivize transitivize]])
(require '[babel.lexiconfn :refer (compile-lex infinitives map-function-on-map-vals unify)])
(require '[babel.parse :as parse])
(require '[babel.ug :refer [head-principle]])
(require '[babel.writer :refer [fill-by-spec fill-verb process write-lexicon]])

(require '[clojure.string :as string])
(require '[clojure.tools.logging :as log])

(require '[dag-unify.core :refer (fail? get-in strip-refs)])
(require '[dag-unify.core :as unify])

(def grammar gram/grammar)
(def lexicon-source lex/lexicon-source)

;; see TODOs in lexiconfn/compile-lex (should be more of a pipeline as opposed to a argument-position-sensitive function.
(def lexicon (future (-> (compile-lex lex/lexicon-source
                                      morph/exception-generator 
                                      morph/phonize morph/italian-specific-rules)

                         ;; make an intransitive version of every verb which has an
                         ;; [:sem :obj] path.
                         intransitivize
                         
                         ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
                         transitivize

                         ;; if a verb is not specifically marked as reflexive, it
                         ;; is reflexive:false, to prevent generation of reflexive
                         ;; sentences using nonreflexive verbs
                         (map-function-on-map-vals
                          (fn [k vals]
                            (map (fn [val]
                                   (cond (and (= (get-in val [:synsem :cat])
                                                 :verb)
                                              (= (get-in val [:synsem :aux] false)
                                                 false)
                                              (= :none (get-in val [:synsem :sem :reflexive] :none)))
                                         (unify val {:synsem {:sem {:reflexive false}}})
                                         true
                                         val))
                                 vals)))

                         ;; Cleanup functions can go here. Number them for ease of reading.
                         ;; 1. this filters out any verbs without an inflection: infinitive verbs should have inflection ':top', 
                         ;; rather than not having any inflection.
                         (map-function-on-map-vals 
                          (fn [k vals]
                            (filter #(or (not (= :verb (get-in % [:synsem :cat])))
                                         (not (= :none (get-in % [:synsem :infl] :none))))
                                    vals))))))
(def small
  (future
    (let [grammar
          (filter #(or (= (:rule %) "s-conditional-phrasal")
                       (= (:rule %) "s-conditional-nonphrasal")
                       (= (:rule %) "s-present-phrasal")
                       (= (:rule %) "s-present-nonphrasal")
                       (= (:rule %) "s-future-phrasal")
                       (= (:rule %) "s-future-nonphrasal")
                       (= (:rule %) "s-imperfect-phrasal")
                       (= (:rule %) "s-imperfect-nonphrasal")
                       (= (:rule %) "s-aux")
                       (= (:rule %) "vp-aux")
                       (= (:rule %) "vp-aux-22")
                       (= (:rule %) "vp-pronoun-nonphrasal")
                       (= (:rule %) "vp-pronoun-phrasal"))
                  grammar)
          lexicon
          (into {}
                (for [[k v] @lexicon]
                  (let [filtered-v
                        (filter #(or (= (get-in % [:synsem :cat]) :verb)
                                     (= (get-in % [:synsem :propernoun]) true)
                                     (= (get-in % [:synsem :pronoun]) true))
                                v)]
                    (if (not (empty? filtered-v))
                      [k filtered-v]))))]
      {:name "small"
       :language "it"
       :language-keyword :italiano
       :morph fo
       :enrich enrich
       :grammar grammar
       :lexicon lexicon
       :index (create-index grammar (flatten (vals lexicon)) head-principle)})))

(def medium
  (future
    (let [lexicon
          (into {}
                (for [[k v] @lexicon]
                  (let [filtered-v v]
                    (if (not (empty? filtered-v))
                      [k filtered-v]))))]
      {:name "medium"
       :language "it"
       :language-keyword :italiano
       :morph fo
       :enrich enrich
       :grammar grammar
       :lexicon lexicon
       :index (create-index grammar (flatten (vals lexicon)) head-principle)
       })))

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
                                                :source-model en/small
                                                :target-model small}
                                               :count count}])))
                                (list {:synsem {:sem {:tense :conditional}}}
                                      {:synsem {:sem {:tense :future}}}
                                      {:synsem {:sem {:tense :present}}}
                                      {:synsem {:sem {:aspect :perfect
                                                      :tense :past}}})))))
                (reduce concat
                        (map (fn [key]
                               (get root-verbs key))
                             (sort (keys root-verbs))))))))

