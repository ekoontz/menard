(ns babel.reader
  [:refer-clojure :exclude [get-in resolve]]
  [:require
   [babel.directory :refer [models models2]]
   [babel.generate :refer [generate]]
   [babel.korma :as korma :refer [convert-keys-from-string-to-keyword init-db read-array]]
   [clojure.string :as string]
   [clojure.data.json :as json :refer [read-str write-str]]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [dag_unify.core :as unify :refer [dissoc-paths get-in ref? strip-refs unify]]
   [korma.core :as db]])

;; Configure database's 'expression' table to find the expressions.
;; requires Postgres 9.4 or higher for JSONb operator '@>' support.

;; TODO: calling functions in here 'generate-X' is misleading
;; since they are querying a table to find sentences, not generating sentences from scratch.
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
    (log/debug (str "(generate-using-db): spec:" spec))
    (generate spec source-language target-language)))

(defn id2expression [id]
  (let [results (db/exec-raw [(str "SELECT structure
                                      FROM expression 
                                     WHERE id=?")
                              [id]]
                             :results)]
    (if (not (empty? results))
      (json-read-str (.getValue (:structure (first results)))))))

(defn generate-question-and-correct-set-dynamic [target-spec source-language source-locale
                                                 target-language target-locale
                                                 source-model target-model]
  (let [target-expression (generate target-spec target-model)
        source-expression (generate {:synsem {:sem (get-in target-expression [:synsem :sem])}}
                                    source-model)]
        {:source ((:fo source-model) source-expression)
         :targets [(:fo target-model) target-expression]
         :target-spec target-spec
         :target-semantics {:synsem {:sem (get-in target-expression [:synsem :sem])}}}))

(defn generate-question-and-correct-set [target-spec source-language source-locale
                                         target-language target-locale]
  "Return a set of semantically-equivalent expressions, for a given spec in the target language,
   and and a single expression in the source language that contains the semantics shared 
   by this set.
   To rephrase, the set of expressions in the target language share an identical semantics, 
   and the single expression in the source language contains that semantics."
  (log/debug (str "generate target language set with target-language:" target-language " ;spec: " target-spec))
  (let [target-spec (-> target-spec
                        (unify {:synsem {:subcat '()}})
                        (dissoc-paths [[:dag_unify.core/serialized]]))]
    (cond (= target-language "la") ;; TODO: should not have a special case for one language.
          (babel.latin/read-one :top  ;; TODO: use target-spec
                                @((-> models :la))
                                @((-> models :en)))
          true
          (let [;; normalize for JSON lookup:
                json-input-spec
                (-> target-spec

                    ;; convert a spec which is simply :top to be {}:
                    ((fn [target-spec]
                       (if (= :top target-spec)
                         {}
                         target-spec))))
                target-json-spec (json/write-str (strip-refs json-input-spec))]
            (log/debug (str "looking for expressions in target language: " target-language
                            " with spec: " target-spec))
            (log/debug (str "looking for expressions in target language: " target-language
                            " with json-spec: " target-json-spec))

            ;; get the structure of a random expression in the target language that matches
            ;; the specification _spec_.
            ;; TODO: this is wasteful - we are getting *all* possible expressions, when we only need
            ;; one (random) expression.
            ;; instead, we should use the target.surface as an input to a hash function; that is,
            ;; ORDER BY the value of the hash function on each row.
            (let [results (db/exec-raw [(str "SELECT structure,surface "
                                             "  FROM expression "
                                             " WHERE active=true "
                                             "   AND language=? "
                                             "   AND structure IS NOT NULL "
                                             "   AND surface != '' "
                                             "   AND structure @> '" target-json-spec "' "
                                             "   AND active=true")
                                        [target-language]]
                                       :results)]
              (if (empty? results)
                (do
                  (log/error (str "nothing found in target language: " target-language
                                  " that matches spec: " target-spec))
                  (throw (Exception. (str "nothing found in target language: " target-language
                                          " that matches spec: " target-spec))))
                
                ;; choose a random expression from the results of the above.
                ;; TODO: should choose a random one using SQL rather than taking them
                ;; all and choosing a random one
                (let [size-of-results (.size results)
                      index-of-result (rand-int (.size results))
                      debug (log/debug (str "number of target results:" size-of-results))
                      debug (log/debug (str "index of target result:" index-of-result))
                      target-expression (nth results index-of-result)
                      debug (log/debug (str "target-expression is nil? " (nil? target-expression)))
                      debug (log/trace (str "target-expression is: " target-expression))]
            
                  ;; Now get all the target expressions that are semantically
                  ;; equivalent to this expression's semantics,
                  ;; and a single source expression whose semantics contain
                  ;; that same semantics.
                  ;; TODO: add language-specific semantic manipulations
                  ;; that can modify how source-target semantic correspondence
                  ;; is determined. For now we have some hard-wired modifications for
                  ;; correspondence between Italian as target and English as source.
                  ;; 
                  ;; For example, subjects in Italian have a :null key, which is
                  ;; set to true or false, while subjects in English, at present, lack this
                  ;; (though this key might be added to the English lexicon later at
                  ;;  some point).
                  (let [target-structure (json-read-str (.getValue (:structure target-expression)))
                        ;; TODO: allow queries that have refs - might be
                        ;; useful for modeling anaphora and binding.
                        source-language-keyword (keyword source-language)
                        target-language-keyword (keyword target-language)
                        target-semantics (get-in target-structure [:synsem :sem])
                        ;; Removes certain parts of the target-semantics that are not expressed
                        ;; in the source-semantics:
                        semantic-correspondence (-> @((models2 source-language-keyword))
                                                    :semantic-correspondence
                                                    target-language-keyword)
                        source-semantics (-> target-semantics
                                             (dissoc-paths semantic-correspondence))

                        ;; transform semantics to JSON so that we can search the expression database table.
                        json-target-semantics (json/write-str (strip-refs target-semantics))
                        json-source-semantics (json/write-str (strip-refs source-semantics))]
                    (log/debug (str "target expression:" (:surface target-structure)))
                    (log/debug (str "target semantics:" (strip-refs target-semantics)))
                    (log/debug (str "source semantics:" (strip-refs source-semantics)))
                    (log/trace (str "json target semantics:" json-target-semantics))
                    (log/trace (str "json source semantics:" json-source-semantics))
                    (let [result
                          (first
                           (db/exec-raw [(str "SELECT source.surface AS source, source.id AS source_id,  "
                                              " source.structure AS source_structure, "
                                              " ARRAY_AGG(target.surface) AS target, "
                                              " ARRAY_AGG(target.root) AS target_root,
                                              ARRAY_AGG(source.structure::jsonb) AS target_structure
                                        FROM (SELECT surface, source.structure->'synsem'->'sem' 
                                                     AS sem,
                                                     source.structure AS structure, source.id
                                                FROM expression AS source
                                               WHERE source.language=?
                                                 AND source.active=true
                                                 AND source.structure->'synsem'->'sem' @> ?::jsonb
                                             LIMIT 1) AS source
                                  INNER JOIN (SELECT 
                                            DISTINCT surface, target.structure->'synsem'->'sem' AS sem,
                                                     root, structure
                                                FROM expression_with_root AS target
                                               WHERE target.language=?
                                                 AND target.active=true
                                                 AND target.structure->'synsem'->'sem' = ?::jsonb) AS target 
                                          ON (source.surface IS NOT NULL) 
                                         AND (target.surface IS NOT NULL) "
                                              " GROUP BY source.surface,source.id,source.structure")
                                         [source-language json-source-semantics target-language json-target-semantics]]
                                        :results))]
                      (if (nil? result)
                        (let [message (str "no source expression found for target semantics: "
                                           (get-in target-structure [:synsem :sem])
                                           "; used source semantics:" source-semantics ".")]
                          (log/error message)
                          (throw (Exception. message))))
                      (let [show-target-structures false ;; true for debugging
                            retval
                            (-> result
                                
                                (dissoc :target) ;; aggregate 'target' values into 'targets'
                                (assoc :targets (read-array (:target result)))
                                
                                (dissoc :target_root) ;; targets' roots -> 'target_roots'
                                (assoc :target_roots (read-array (:target_root result)))

                                (dissoc :target_structure) ;; targets' structures -> 'target structures'

                                (dissoc :source_structure)
                                
                                (dissoc :source)
                                (assoc :source
                                       (let [morph (get @((get babel.directory/models2 source-language-keyword)) :morph)]
                                         (morph (json-read-str (str (:source_structure result)))
                                                :from-language target-language)))
                                
                                ((fn [retval]
                                   (or 
                                    (and show-target-structures
                                         (assoc :target_structures (read-array (:target_structure result))))
                                    retval)))

                                )]
                        (log/info (str "generate-question-and-correct-set [target-spec:" target-spec "] => retval: " retval))
                        retval))))))))))
    
(defn get-lexeme [canonical language & [ spec ]]
  "get a lexeme from the database given the canonical form, given a
  language and optionally additional filter specification"
  ;; TODO: does not support filter yet.
  (let [results (db/exec-raw [(str "SELECT structure
                                      FROM lexeme
                                     WHERE canonical=?
                                       AND language=?")
                              [canonical language]]
                             :results)]
    (map (fn [result]
           (json-read-str (.getValue (:structure result))))
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

    (let [results (db/exec-raw [(str "SELECT structure
                                        FROM expression 
                                       WHERE language=? 
                                         AND active=true
                                         AND structure @> "
                                     "'" json-spec "'")
                                [language]]
                               :results)]
      (map (fn [result]
             (json-read-str (.getValue (:structure result))))
           results))))

(defn read-all [spec language]
  (let [spec (dissoc (unify spec
                            {:synsem {:subcat '()}})
                     :dag_unify.core/serialized)

        ;; normalize for JSON lookup
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " language " with spec: " spec))
    (let [results (db/exec-raw [(str "SELECT DISTINCT surface,structure
                                        FROM expression 
                                       WHERE active=true 
                                         AND language=? 
                                         AND structure @> "
                                     "'" json-spec "' ORDER BY SURFACE ASC")
                                [language]]
                               :results)]
      (log/debug (str "Number of expressions that that need to be translated: " (.size results)))
      (map (fn [result]
             {:surface (:surface result)
              :structure (json-read-str (.getValue (:structure result)))})
           results))))

(defn read-one [spec language {no-older-than :no-older-than}]
  (log/debug (str "read-one: no-older-than: " no-older-than))
  (let [no-older-than (or no-older-than nil)
        spec (unify spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))]
    (log/debug (str "looking for all expressions in language: " language " with spec: " spec))

    (let [results (db/exec-raw [(str "SELECT count(*)
                                        FROM expression 
                                       WHERE language=? 
                                         AND ((?::timestamp IS NULL) OR 
                                              (expression.created > ?::timestamp))
                                         AND active=true
                                         AND structure @> ?::jsonb "
                                     " LIMIT 1 ")
                                [language no-older-than no-older-than (json/write-str spec)]]
                               :results)]
      (log/debug (str "results for spec:" spec " : " (string/join "," results))))

    (let [results (db/exec-raw [(str "SELECT surface,structure
                                        FROM expression 
                                       WHERE language=? 
                                         AND ((?::timestamp IS NULL) OR 
                                              (expression.created > ?::timestamp))
                                         AND active=true
                                         AND structure @> ?::jsonb "
                                     " LIMIT 1 ")
                                [language no-older-than no-older-than (json/write-str spec)]]
                               :results)]
      (if (not (empty? results))
        (log/debug (str "existing results:" (string/join "," results))))
      (first (map (fn [result]
                    {:surface (:surface result)
                     :structure (json-read-str (.getValue (:structure result)))})
                  results)))))

(defn contains [spec]
  "Find the sentences in English that match the spec, and the set of Italian sentences that each English sentence contains."
    (let [spec (if (= :top spec)
                 {}
                 spec)
          json-spec (json/write-str (strip-refs spec))
          ;; TODO: use '?' below, not string concatenation
          results (db/exec-raw [(str "SELECT DISTINCT * 
                                        FROM (SELECT english.surface   AS en,
                                                      italiano.surface AS it,               
                                   italiano.structure->'synsem'->'sem' AS italian_semantics,
                                    english.structure->'synsem'->'sem' AS english_semantics         
                                                FROM expression AS italiano
                                          INNER JOIN expression AS english                                 
                                                  ON english.structure @> '" json-spec "'
                                                 AND italiano.language = 'it' AND italiano.active=true
                                                 AND english.language  = 'en' AND english.active=true
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
  (let [results (db/exec-raw [(str "SELECT canonical,structure FROM lexeme WHERE language=?")
                              [language]]
                             :results)]
    (group-by-canonical-form
     (map (fn [x]
            {:surface (:canonical x)
             :structure (json-read-str (.getValue (:structure x)))})
          results))))

;; TODO: document why we need this
;; TODO: verbcoach should use this, not config/json-read-str
;; TODO: move this to babel/globals.cljc or something similar
(declare json-value-converter)

(defn json-read-str [json]
  (json/read-str json
                 :key-fn keyword
                 :value-fn json-value-converter))

(defn json-value-converter [k v]
  (cond
    (and (or (= k :english)
             (= k :espanol)
             (= k :français)
             (= k :italiano)
             (= k "english")
             (= k "espanol")
             (= k "français")
             (= k "italiano")
             (= k "latin")
             (= k :participle)
             (= k :past)
             (= k :past-participle)
             (= k :present)
             (= k :1sing)
             (= k :2sing)
             (= k :3sing))
         (not (map? v)))
    (str v)
    
    (and (string? v)
         (= (nth v 0) \:))
    (keyword (string/replace-first v ":" ""))
    
    (string? v)
    (keyword v)
    :else v))

  
