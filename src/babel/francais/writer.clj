(ns babel.francais.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.cache :refer [create-index]])
(require '[babel.english.writer :as en])
(require '[babel.forest :as forest])
(require '[babel.francais.grammar :refer [grammar]])
(require '[babel.francais.lexicon :refer [lexicon-source]])
(require '[babel.francais.morphology :refer [analyze exception-generator
                                             fo get-string phonize]])
(require '[babel.francais.pos :refer [intransitivize transitivize]])
(require '[babel.lexiconfn :refer (compile-lex map-function-on-map-vals unify)])
(require '[babel.parse :as parse])
(require '[babel.ug :refer [head-principle]])
(require '[babel.writer :refer [process write-lexicon]])
(require '[clojure.string :as string])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer (fail? get-in strip-refs)])

(declare enrich)

;; see TODOs in lexiconfn/compile-lex (should be more of a pipeline
;; as opposed to a argument-position-sensitive function.
(def lexicon
  (future (-> (compile-lex lexicon-source exception-generator phonize)

              ;; make an intransitive version of every verb which has an
              ;; [:sem :obj] path.
              intransitivize
              
              ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
              transitivize

              ;; Cleanup functions can go here. Number them for ease of reading.
              ;; 1. this filters out any verbs without an inflection:
              ;; infinitive verbs should have inflection ':infinitive', 
              ;; rather than not having any inflection.
              (map-function-on-map-vals 
               (fn [k vals]
                 (filter #(or (not (= :verb (get-in % [:synsem :cat])))
                              (not (= :none (get-in % [:synsem :infl] :none))))
                         vals))))))
(def small
  (future
    (let [grammar
          (filter #(or (= (:rule %) "s-conditional-nonphrasal")
                       (= (:rule %) "s-present-nonphrasal")
                       (= (:rule %) "s-future-nonphrasal")
                       (= (:rule %) "s-imperfect-nonphrasal")
                       (= (:rule %) "s-aux")
                       (= (:rule %) "vp-aux"))
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
       :language "fr"
       :language-keyword :français
       :enrich enrich
       :grammar grammar
       :lexicon lexicon
       :morph fo
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
       :enrich enrich
       :grammar grammar
       :lexicon lexicon
       :index (create-index grammar (flatten (vals lexicon)) head-principle)
       })))

(defn tout [ & [count]]
  (let [count (if count (Integer. count) 10)]
    (write-lexicon "fr" @lexicon)
    (let [
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
      (.size (map (fn [verb]
                    (let [root-form (get-in verb [:français :français])]
                      (log/debug (str "generating from root-form:" root-form))
                      (.size (map (fn [tense]
                                    (let [spec (unify {:root {:français {:français root-form}}}
                                                      tense)]
                                      (log/debug (str "generating from: " spec))
                                      (process [{:fill
                                                 {:spec spec
                                                  :source-model en/small
                                                  :target-model small}
                                                 :count count}])))
                                  [{:synsem {:sem {:tense :conditional}}}
                                   {:synsem {:sem {:tense :future}}}
                                   {:synsem {:sem {:tense :present}}}
                                   ;; TODO: enable this when ready:
                                   ;; {:synsem {:sem {:aspect :perfect
                                   ;;                 :tense :past}}}
                                   ]
                                  ))))
                  (reduce concat
                          (map (fn [key]
                                 (get root-verbs key))
                               (sort (keys root-verbs)))))))))

(declare against-pred)
(declare matching-head-lexemes)
(defn enrich [spec lexicon]
  (against-pred spec lexicon))

(defn against-pred [spec lexicon]
  (let [pred (get-in spec [:synsem :sem :pred] :top)]
    (if (= :top pred)
      spec
      (mapcat (fn [lexeme]
                (let [result (unify spec
                                    {:synsem {:sem (strip-refs (get-in lexeme [:synsem :sem] :top))}})]
                  (if (not (fail? result))
                    (do
                      (log/debug (str "matched head lexeme: " (strip-refs lexeme)))
                      (list result)))))
              (matching-head-lexemes spec lexicon)))))

(defn matching-head-lexemes [spec lexicon]
  (let [pred-of-head (get-in spec [:synsem :sem :pred] :top)]
    (if (= pred-of-head :top)
      spec
      (mapcat (fn [lexemes]
                (mapcat (fn [lexeme]
                          (if (= pred-of-head
                                 (get-in lexeme [:synsem :sem :pred] :top))
                            (list lexeme)))
                        lexemes))
              (vals lexicon)))))
