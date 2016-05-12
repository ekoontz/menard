(ns babel.reader
  [:refer-clojure :exclude [get-in resolve]]
  [:require
   [babel.korma :as korma]
   [clojure.string :as string]
   [clojure.data.json :as json :refer [read-str write-str]]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [dag_unify.core :as unify :refer [deserialize get-in ref? strip-refs unify]]
   [korma.core :as db]])

;; Configure database's 'expression' table to find the expressions.
;; requires Postgres 9.4 or higher for JSONb operator '@>' support.

;; TODO: calling functions in here 'generate-X' is misleading
;; since they are querying a table to find sentences, not generating sentences from scratch.
(declare generate)
(declare generate-question-and-correct-set)
(declare id2expression)
(declare json-read-str)

;; web service that translates babel.reader's API and an HTTP client.
;; http://localhost:3001/reader?target_spec={%22root%22:{%22italiano%22:{%22italiano%22:%22scrivere%22}}%20%22synsem%22:%20{%22sem%22:%20{%22tense%22:%22past%22,%20%22aspect%22:%22perfect%22}}}
(def routes
  (compojure/routes
      (GET "/" request
           {:status 200
            :headers {"Content-Type" "application/json;charset=utf-8"}
            :body
            (let [source (get (:query-params request) "source" "en")
                  source-locale (get (:query-params request) "source_locale" "US")
                  target "it"
                  target-locale "IT"
                  target_spec (json-read-str
                               (if-let [target_spec (get (:query-params request) "target_spec")]
                                 target_spec "{}"))
                  gcacs (try
                          (generate-question-and-correct-set
                           target_spec
                           source source-locale
                           target target-locale)
                          (catch Exception e
                            {:exception (str e)}))]
              (write-str gcacs))})
;                          :target_spec target_spec
;                          ;; TODO: eventually add :source-v2
;                          :targets (get-in gcacs [:targets])
;                          :targets-v2 (get-in gcacs [:targets-v2])}))})

      (GET "/:expr" request
           (let [expr (:expr (:route-params request))]
             (log/debug (str "expr(1):" expr))
             (log/debug (str "expr(2):" (Integer. expr)))
             {:status 200
              :headers {"Content-Type" "application/json;charset=utf-8"}
              :body (write-str (id2expression (Integer. expr)))}))))

(defn generate-using-db [spec source-language target-language]
  (let [spec (unify spec
                    {:synsem {:subcat '()}})]
    (log/debug (str "spec pre-borges/generate:" spec))
    (generate spec source-language target-language)))

(defn id2expression [id]
  (let [results (db/exec-raw [(str "SELECT serialized::text AS structure 
                                      FROM expression 
                                     WHERE id=?")
                              [id]]
                             :results)]
    (if (not (empty? results))
      (deserialize (read-string (:structure (first results)))))))

(defn generate-question-and-correct-set [target-spec source-language source-locale
                                         target-language target-locale]
  "Return a set of semantically-equivalent expressions, for a given spec in the target language,
   and and a single expression in the source language that contains the semantics shared 
   by this set.
   To rephrase, the set of expressions in the target language share an identical semantics, 
   and the single expression in the source language contains that semantics."
  (log/debug (str "generate target language set with spec: " target-spec))
  (let [target-spec (unify target-spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup: convert a spec which is simply :top to be {}.
        json-input-spec (if (= :top target-spec)
                          {}
                          target-spec)
        
        target-json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " target-language
                    " with spec: " target-spec))
    (log/debug (str "looking for expressions in language: " target-language
                    " with json-spec: " target-json-spec))

    ;; get the structure of a random expression in the target language that matches
    ;; the specification _spec_.
    ;; TODO: this is wasteful - we are getting *all* possible expressions, when we only need
    ;; one (random) expression.
    ;; instead, we should use the target.surface as an input to a hash function; that is,
    ;; ORDER BY the value of the hash function on each row.
    (let [results (db/exec-raw [(str "SELECT target.serialized::text AS target,target.surface
                                        FROM expression AS target
                                       WHERE target.language=?
                                         AND target.structure IS NOT NULL
                                         AND target.surface != ''
                                         AND target.structure @> '" target-json-spec "'")
                                [target-language]]
                               :results)]
      (if (empty? results)
        (do
          (log/error (str "nothing found in target language: " target-language
                          " that matches spec: " target-spec))
          (throw (Exception. (str "nothing found in target language: " target-language
                                  " that matches spec: " target-spec))))

        ;; choose a random expression from the results of the above.
        (let [size-of-results (.size results)
              index-of-result (rand-int (.size results))
              debug (log/debug (str "number of target results:" size-of-results))
              debug (log/debug (str "index of target result:" index-of-result))
              target-expression (nth results index-of-result)
              debug (log/debug (str "target-expression is nil?" (nil? target-expression)))
              debug (log/trace (str "target-expression is: " target-expression))
              ]

          ;; Now get all the target expressions that are semantically
          ;; equivalent to this expression's semantics,
          ;; and a single source expression whose semantics contain
          ;; that same semantics.
          ;; TODO: consider selecting source where semantics contains
          ;; that semantics, OR is contained by that semantics.
          ;; Both possiblities might be necessary if the lexicon of
          ;; the source language and target language differ in how
          ;; much semantic information are encoded in entries for
          ;; specific lexemes. For example, 'to go' in English
          ;; specifies
          ;; {activity: true}, whereas 'andare' in English does
          ;; not. This might be a bug in the Italian lexicon, but
          ;; this database lookup should be able to handle bugs like this.
          (let [result (deserialize (read-string (:target target-expression)))
                ;; TODO: allow queries that have refs - might be
                ;; useful for modeling anaphora and binding.
                json-semantics (json/write-str (strip-refs (get-in result [:synsem :sem])))]
            (log/debug (str "semantics:" (strip-refs (get-in result [:synsem :sem]))))
            (log/trace (str "json-semantics:" json-semantics))
            (let [results
                  (db/exec-raw [(str "SELECT source.surface AS source, source.id AS source_id,
                                             target.surface AS target,target.root AS target_root,
                                             source.structure AS structure,
                                             target.structure::text AS target_structure
                                        FROM (SELECT surface, source.structure->'synsem'->'sem' 
                                                     AS sem,
                                                     source.structure AS structure, source.id
                                                FROM expression AS source
                                               WHERE source.language=?
                                                 AND source.structure->'synsem'->'sem' @> '"
                                     json-semantics "' LIMIT 1) AS source
                                  INNER JOIN (SELECT DISTINCT surface, target.structure->'synsem'->'sem' AS sem,
                                                              root,structure
                                                         FROM expression_with_root AS target
                                                        WHERE target.language=?
                                                          AND target.structure->'synsem'->'sem' = '" json-semantics "') AS target 
                                                           ON (source.surface IS NOT NULL) 
                                                          AND (target.surface IS NOT NULL) 
                                                          AND (source.sem @> target.sem)")
                            [source-language target-language]]
                           :results)]
          (if (nil? (first (map :source results)))
            (do
              (log/error (str "no source expression found for semantics: "
                              (get-in (strip-refs (deserialize
                                                   (read-string (:target target-expression))))
                                      [:synsem :sem])))
              (throw (Exception.
                      (str "no source expression found for target expression: '"
                           (:surface target-expression) "' and semantics: " 
                           (get-in (strip-refs (deserialize
                                                (read-string (:target target-expression))))
                                              [:synsem :sem])))))
            (let [debug (log/trace (str "source-structure:"
                                        (first (map :structure results))))
                  debug (log/trace (str "read from target_structure:"
                                        (first (map :target_structure results))))
                  debug (log/trace (str "target structures: (via read-str)" 
                                        (read-str (first (map :target_structure results)))))
                  debug (log/trace (str "target-structures (via json-read-str):" 
                                        (json-read-str (first (map :target_structure results)))))]
              {:source-id (first (map :source_id results))
               :source (first (map :source results))
               :source-v2 {:language source-language
                           :locale source-locale}
               :target-spec target-spec
               :targets-v2 (map (fn [result]
                                  {:surface (get result :target)
                                   :language target-language
                                   :locale target-locale
                                   :root (:target_root result)
                                   :lexemes
                                   (let [structure (json-read-str (:target_structure result))]
                                     (remove nil?
                                             [(get-in structure [:comp :italiano :italiano])
                                              (get-in structure [:root :italiano :infinitive])
                                              (get-in structure [:root :italiano :infinitive])
                                              (get-in structure [:root :italiano :italiano])]))})
                               results)
               :targets (map :target results)})))))))))

(defn get-lexeme [canonical language & [ spec ]]
  "get a lexeme from the database given the canonical form, given a
  language and optionally additional filter specification"
  ;; TODO: does not support filter yet.
  (let [results (db/exec-raw [(str "SELECT serialized 
                                      FROM lexeme
                                     WHERE canonical=?
                                       AND language=?")
                              [canonical language]]
                             :results)]
    (map (fn [result]
           (deserialize (read-string (:serialized result))))
         results)))

(defn generate-all [spec language]
  "find all sentences in the expression table matching 'spec' in a given language."
  (let [spec (unify spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " language " with spec: " spec))
    (log/debug (str "SQL: "
                   (str "SELECT surface FROM expression WHERE language='" language "' AND structure @> "
                        "'" json-spec "'")))

    (let [results (db/exec-raw [(str "SELECT serialized::text 
                                        FROM expression 
                                       WHERE language=? AND structure @> "
                                     "'" json-spec "'")
                                [language]]
                               :results)]
      (map (fn [result]
             (deserialize (read-string (:serialized result))))
           results))))

(defn read-all [spec language]
  (let [spec (unify spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " language " with spec: " spec))
    (let [results (db/exec-raw [(str "SELECT surface,serialized::text 
                                        FROM expression 
                                       WHERE language=? AND structure @> "
                                     "'" json-spec "' ORDER BY SURFACE ASC")
                                [language]]
                               :results)]
      (log/debug (str "Number of expressions that that need to be translated: " (.size results)))
      (map (fn [result]
             {:surface (:surface result)
              :structure (deserialize (read-string (:serialized result)))})
           results))))

(defn read-one [spec language]
  (let [spec (unify spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " language " with spec: " spec))
    (let [results (db/exec-raw [(str "SELECT surface,serialized::text 
                                        FROM expression 
                                       WHERE language=? AND structure @> "
                                     "'" json-spec "' LIMIT 1")
                                [language]]
                               :results)]
      (first (map (fn [result]
                    {:surface (:surface result)
                     :structure (deserialize (read-string (:serialized result)))})
                  results)))))

(defn contains [spec]
  "Find the sentences in English that match the spec, and the set of Italian sentences that each English sentence contains."
    (let [spec (if (= :top spec)
                 {}
                 spec)
          json-spec (json/write-str (strip-refs spec))
          results (db/exec-raw [(str "SELECT DISTINCT * 
                                        FROM (SELECT english.surface   AS en,
                                                      italiano.surface AS it,               
                                   italiano.structure->'synsem'->'sem' AS italian_semantics,
                                    english.structure->'synsem'->'sem' AS english_semantics         
                                                FROM expression AS italiano
                                          INNER JOIN expression AS english                                 
                                                  ON english.structure @> '" json-spec "'
                                                 AND italiano.language = 'it'
                                                 AND english.language  = 'en'
                                                 AND (italiano.structure->'synsem'->'sem') @> 
                                                     (english.structure->'synsem'->'sem')) AS pairs 
                                    ORDER BY pairs.en")
                                []]
                               :results)]
      results))

;; (map #(str (get-in % [:en]) " / " (get-in % [:it]) " | ") (contains {:synsem {:sem {:pred :mangiare :subj {:pred :noi}}}}))

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  (if (seq? input-map)
    (map get-meaning
         input-map)
    {:synsem {:cat (get-in input-map [:synsem :cat] :top)
              :sem (get-in input-map [:synsem :sem] :top)
              :subcat (get-in input-map [:synsem :subcat] :top)}}))

(defn zipmap-with-fn [the-keys the-vals acc]
  (if (not (empty? the-keys))
    (let [key (first the-keys)
          val (first the-vals)]
      (if (not (= :nothing (get acc key :nothing)))
        (zipmap-with-fn (rest the-keys) (rest the-vals)
                        (merge
                         acc
                         {key (cons val
                                    (get acc key))}))
					 (zipmap-with-fn (rest the-keys) (rest the-vals)
							 (merge
							  acc
							  {key (list val)}))))
    acc))

(defn group-by-canonical-form [lexicon]
  (zipmap-with-fn (map :surface lexicon)
                  (map :structure lexicon)
                  {}))

(defn read-lexicon [language]
  (let [results (db/exec-raw [(str "SELECT canonical,serialized FROM lexeme WHERE language=?")
                              [language]]
                             :results)]
    (group-by-canonical-form
     (map (fn [x]
            {:surface (:canonical x)
             :structure (deserialize (read-string (:serialized x)))})
          results))))

;; TODO: document why we need this
;; TODO: verbcoach should use this, not config/json-read-str
;; TODO: move this to babel/globals.cljc or something similar
(defn json-read-str [json]
  (json/read-str json
                 :key-fn keyword
                 :value-fn (fn [k v]
                             (cond
                              (and (or (= k :english)
                                       (= k :espanol)
                                       (= k :français)
                                       (= k :italiano))
                                   (not (map? v)))
                              (str v)

                              (and (string? v)
                                   (= (nth v 0) \:))
                              (keyword (string/replace-first v ":" ""))

                              (string? v)
                              (keyword v)
                              :else v))))
