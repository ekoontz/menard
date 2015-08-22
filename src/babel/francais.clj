(ns babel.francais
  (:refer-clojure :exclude [get-in]))

(require '[clojure.string :as string])
(require '[clojure.tools.logging :as log])

(require '[dag-unify.core :refer (fail? get-in strip-refs)])
(require '[babel.cache :refer [create-index]])
(require '[babel.forest :as forest])
(require '[babel.francais.grammar :refer [grammar]])
(require '[babel.francais.lexicon :refer [lexicon-source]])
(require '[babel.francais.morphology :refer [analyze exception-generator fo get-string phonize]])
(require '[babel.lexiconfn :refer (compile-lex map-function-on-map-vals unify)])

(require '[babel.parse :as parse])
(require '[babel.francais.pos :refer [intransitivize transitivize]])
(require '[babel.ug :refer [head-principle]])

;; see TODOs in lexiconfn/compile-lex (should be more of a pipeline as opposed to a argument-position-sensitive function.
(def lexicon
  (future (-> (compile-lex lexicon-source exception-generator phonize)

              ;; make an intransitive version of every verb which has an
              ;; [:sem :obj] path.
              intransitivize
              
              ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
              transitivize
              
              ;; Cleanup functions can go here. Number them for ease of reading.
              ;; 1. this filters out any verbs without an inflection: infinitive verbs should have inflection ':infinitive', 
              ;; rather than not having any inflection.
              (map-function-on-map-vals 
               (fn [k vals]
                 (filter #(or (not (= :verb (get-in % [:synsem :cat])))
                              (not (= :none (get-in % [:synsem :infl] :none))))
                         vals))))))

(def verbs (filter (fn [lexeme]
                     (contains? (set (get-in (get @lexicon lexeme) [:synsem :cat])) :verb))
                   (keys @lexicon)))

(defn lookup [token]
  "return the subset of lexemes that match this token from the lexicon."
  (analyze token #(get @lexicon %)))

(def fr lookup) ;; abbreviation for the above

(defn parse [string]
  (parse/parse string lexicon lookup grammar))

(def index nil)
;; TODO: trying to print index takes forever and blows up emacs buffer:
;; figure out how to change printable version to (keys index).
(def index (future (create-index grammar (flatten (vals @lexicon)) head-principle)))

(defn generate [ & [spec {use-grammar :grammar
                          use-index :index
                          use-lexicon :lexicon}]]
  (let [spec (if spec spec :top)
        use-grammar (if use-grammar use-grammar grammar)
        use-index (if use-index use-index index)
        use-lexicon (if use-lexicon use-lexicon lexicon)]
    (log/info (str "using grammar of size: " (.size use-grammar)))
    (if (seq? spec)
      (map generate spec)
      (forest/generate spec use-grammar
                       (flatten (vals @use-lexicon))
                       use-index))))

(defn sentence [ & [spec]]
  (let [spec (unify (if spec spec :top)
                    {:synsem {:subcat '()
                              :cat :verb}})]
    (generate spec {:use-grammar grammar
                    :use-index index
                    :use-lexicon (flatten (vals @lexicon))})))

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
(declare enrich)

(def small
  (future
    (let [grammar
          (filter #(or (= (:rule %) "s-conditional-nonphrasal")
                       (= (:rule %) "s-present-nonphrasal")
                       (= (:rule %) "s-future-nonphrasal")
                       (= (:rule %) "s-imperfetto-nonphrasal")
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

(declare against-comp)
(declare against-pred)
(declare matching-comp-lexemes)
(declare matching-head-lexemes)

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

(defn + [& args]
  (parse (string/join " " args)))
