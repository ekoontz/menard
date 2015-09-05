(ns babel.italiano
  (:refer-clojure :exclude [get-in]))

(require '[babel.cache :refer (create-index)])
(require '[babel.forest :as forest])
(require '[babel.italiano.grammar :as gram])
(require '[babel.italiano.lexicon :as lex])
(require '[babel.italiano.morphology :as morph :refer [fo]])
(require '[babel.italiano.pos :refer [intransitivize transitivize]])
(require '[babel.lexiconfn :refer (compile-lex infinitives map-function-on-map-vals unify)])
(require '[babel.parse :as parse])
(require '[babel.ug :refer [head-principle]])

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
                         
                         ;; Cleanup functions can go here. Number them for ease of reading.
                         ;; 1. this filters out any verbs without an inflection: infinitive verbs should have inflection ':top', 
                         ;; rather than not having any inflection.
                         (map-function-on-map-vals 
                          (fn [k vals]
                            (filter #(or (not (= :verb (get-in % [:synsem :cat])))
                                         (not (= :none (get-in % [:synsem :infl] :none))))
                                    vals))))))
(declare enrich)
(declare against-pred)
(declare against-comp)
(declare matching-head-lexemes)
(declare matching-comp-lexemes)

(def small
  (future
    (let [grammar
          (filter #(or (= (:rule %) "s-conditional-phrasal")
                       (= (:rule %) "s-conditional-nonphrasal")
                       (= (:rule %) "s-present-phrasal")
                       (= (:rule %) "s-present-nonphrasal")
                       (= (:rule %) "s-future-phrasal")
                       (= (:rule %) "s-future-nonphrasal")
                       (= (:rule %) "s-imperfetto-phrasal")
                       (= (:rule %) "s-imperfetto-nonphrasal")
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

(defn enrich [spec]
  (let [against-pred (against-pred spec)]
    (if true against-pred
        (let [against-comp (map (fn [spec]
                            (against-comp spec))
                          (if (seq? against-pred)
                            (seq (set against-pred))
                            against-pred))]
          (if (seq? against-comp)
            (seq (set against-comp))
            against-comp)))))

(defn against-pred [spec]
  (let [pred (get-in spec [:synsem :sem :pred] :top)]
    (if (= :top pred)
      spec
      (mapcat (fn [lexeme]
                (let [result (unify spec
                                    {:synsem {:sem (strip-refs (get-in lexeme [:synsem :sem] :top))}}
                                    {:synsem {:essere (strip-refs (get-in lexeme [:synsem :essere] :top))}}
                                    )]
                  (if (not (fail? result))
                    (list result))))
              (matching-head-lexemes spec)))))

;; TODO: not currently used: needs to be called from within (enrich).
(defn against-comp [spec]
  (let [pred-of-comp (get-in spec [:synsem :sem :subj :pred] :top)]
    (if (= :top pred-of-comp)
      spec
      (mapcat (fn [lexeme]
                (let [result (unify spec
                                    {:comp {:synsem {:agr (strip-refs (get-in lexeme [:synsem :agr] :top))
                                                     :sem (strip-refs (get-in lexeme [:synsem :sem] :top))}}})]
                  (if (not (fail? result))
                    (list result))))
              (matching-comp-lexemes spec)))))

(defn matching-head-lexemes [spec]
  (let [pred-of-head (get-in spec [:synsem :sem :pred] :top)]
    (if (= pred-of-head :top)
      spec
      (mapcat (fn [lexemes]
                (mapcat (fn [lexeme]
                          (if (= pred-of-head
                                 (get-in lexeme [:synsem :sem :pred] :top))
                            (list lexeme)))
                        lexemes))
              (vals @lexicon)))))

(defn matching-comp-lexemes [spec]
  (let [pred-of-comp (get-in spec [:synsem :sem :subj :pred] :top)]
    (if (= pred-of-comp :top)
      spec
      (mapcat (fn [lexemes]
                (mapcat (fn [lexeme]
                          (if (= pred-of-comp
                                 (get-in lexeme [:synsem :sem :pred] :top))
                            (list lexeme)))
                        lexemes))
              (vals @lexicon)))))
(defn lookup [token]
  "return the subset of lexemes that match this token from the lexicon."
  (morph/analyze token #(get @lexicon %)))

(def it lookup) ;; abbreviation for the above

(defn parse [string]
  (parse/parse string lexicon lookup grammar))

(def index nil)
;; TODO: trying to print index takes forever and blows up emacs buffer:
;; figure out how to change printable version to (keys index).
(def index (future (create-index grammar (flatten (vals @lexicon)) head-principle)))

(defn sentence [ & [spec]]
  (let [spec (unify (if spec spec :top)
                    {:synsem {:subcat '()
                              :cat :verb}})]
    (forest/generate spec grammar (flatten (vals @lexicon)) index)))

(declare small)

(defn generate [ & [spec model]]
  (let [spec (if spec spec :top)
        model (if model model small)
        model (if (future? model) @model model)]
    (forest/generate spec
                     (:grammar model)
                     (:lexicon model)
                     (:index model)
                     fo)))

;; TODO: factor out to forest/.
(defn generate-all [ & [spec {use-grammar :grammar
                              use-index :index
                              use-lexicon :lexicon}]]
  (let [spec (if spec spec :top)
        use-grammar (if use-grammar use-grammar grammar)
        use-index (if use-index use-index index)
        use-lexicon (if use-lexicon use-lexicon lexicon)]
    (log/info (str "using grammar of size: " (.size use-grammar)))
    (log/info (str "using index of size: " (.size @use-index)))
    (if (seq? spec)
      (mapcat generate-all spec)
      (forest/generate-all spec use-grammar
                           (flatten (vals @use-lexicon))
                           use-index))))

;; TODO: move the following 2 to lexicon.clj:
(def lookup-in
  "find all members of the collection that matches with query successfully."
  (fn [query collection]
    (loop [coll collection matches nil]
      (if (not (empty? coll))
        (let [first-val (first coll)
              result (unify/match (unify/copy query) (unify/copy first-val))]
          (if (not (unify/fail? result))
            (recur (rest coll)
                   (cons first-val matches))
            (recur (rest coll)
                   matches)))
        matches))))

(defn choose-lexeme [spec]
  (first (unify/lazy-shuffle (lookup-in spec (vals lexicon)))))
