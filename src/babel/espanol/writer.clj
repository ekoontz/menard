(ns babel.espanol.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.directory :refer [models]])
(require '[babel.espanol.grammar :refer [parse]])
(require '[babel.espanol.morphology :as morph])
(require '[babel.espanol.pos :refer :all])
(require '[babel.generate :refer [generate]])
(require '[babel.korma :refer [init-db]])
(require '[babel.lexiconfn :refer (compile-lex map-function-on-map-vals)])
(require '[babel.log :refer [log4j!]])
(require '[babel.parse :as parse])
(require '[babel.ug :refer :all])
(require '[babel.writer :as writer :refer [write-lexicon]])
(require '[clojure.string :as string])
(require '[clojure.tools.logging :as log])
(require '[dag_unify.core :refer (fail? get-in strip-refs)])
(require '[dag_unify.core :as unify :refer [unify]])

;; for debugging:
(require '[babel.espanol.morphology.verbs :as esverbs])

;; for debugging: use map: in-order rather than interleaved by multiple workers.
;; TODO: using pmap fails: cannot generate sentences; figure out why.
(def use-map-fn pmap)

(defn root-verbs [lexicon]
  (let [lexemes (vals lexicon)
        verbs (filter #(not (empty? (last %)))
                      (zipmap
                       (keys lexicon)
                       (use-map-fn (fn [lexeme-set]
                                     (filter (fn [lexeme]
                                               (and
                                                (= (get-in lexeme [:synsem :cat]) :verb)
                                                (= (get-in lexeme [:synsem :infl]) :top)
                                                (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                                             lexeme-set))
                                   lexemes)))]
    (zipmap (map first verbs)
            (map second verbs))))

(defn todos [ & [count lexeme]]
  (let [count (if count (Integer. count) 10)
        model (-> ((-> models :es)) deref)
        type-of-model (type model)
        lexicon (:lexicon model)
        lexemes (if lexeme (list (get lexicon lexeme))
                    (vals lexicon))
        root-verbs (root-verbs lexicon)

        root-verb-array
        (reduce concat
                (use-map-fn (fn [key]
                              (get root-verbs key))
                            (if lexeme
                              [lexeme]
                              (sort (keys root-verbs)))))]
    (init-db)
    (write-lexicon "es" lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating examples with this many verbs:"
                   (.size root-verb-array)))
    (.size
     (->> root-verb-array
          (use-map-fn
           (fn [verb]
             (log/debug (str "verb: " (strip-refs verb)))
             (let [root-form (get-in verb [:espanol :espanol])
                   spec {:root {:espanol {:espanol root-form}}}]
               (log/info (str "generating with verb: '" root-form "'"))
               (writer/generate-from-spec
                model (strip-refs spec)
                (vals babel.espanol.grammar/tenses)
                [{:gender :masc} {:gender :fem}]
                [:1st :2nd :3rd]
                [:sing :plur]))))))))

