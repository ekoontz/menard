(ns babel.writer
  (:refer-clojure :exclude [get-in merge])
;  (:import [org.postgresql JDBCException])
  (:require
    [babel.lexiconfn :refer [sem-impl]]
    [clojure.data.json :as json]
    [clojure.string :as string]
    [clojure.tools.logging :as log]
    [dag-unify.core :refer [fail? get-in merge strip-refs serialize unify ref?]]
    [babel.engine :as engine]
    [babel.korma :as korma]
    [korma.core :refer [exec-raw]]))

;; TODO: more fine-grained approach to dealing with exceptions:
;; should be sensitive to environment -
;; e.g. do not throw exception in production; but *do* throw in dev.
(def mask-populate-errors false)

(declare populate)
(defn -main [& args]
  (if (not (nil? (first args)))
    (populate (Integer. (first args)))
    (populate 100)))

(defn truncate [ & [table]]
  (let [table (if table table "expression")]
;; doesn't work:
;;    (exec-raw ["TRUNCATE ?" [table]])))
   (exec-raw [(str "TRUNCATE " table)])))
;; catch exceptions when trying to populate

(defn expression [model spec]
  (let [no-language (if (nil? model)
                             (throw (Exception. "No target language model was supplied.")))
        model (if (future? model)
                @model
                model)

        sentence (engine/generate spec model :enrich true)

        sentence (merge sentence {:spec spec})

        check (if (nil? sentence)
                (throw (Exception. (str "Could not generate a sentence for spec: " spec " for language: " (:language model)
                                        " with model named: " (:name model)))))

        sentence (if (:morph-walk-tree model)
                   (merge ((:morph-walk-tree model) sentence)
                          sentence)
                   (do (log/warn (str "there is no morph-walk-tree function for the model:"
                                      (:name model) " of language: "
                                      (:language model)))
                       sentence))
        sentence (let [subj (get-in sentence
                                    [:synsem :sem :subj] :notfound)]
                   (cond (not (= :notfound subj))
                         (do
                           (log/debug (str "subject constraints: " subj))
                           (unify sentence
                                  {:synsem {:sem {:subj subj}}}))
                         true
                         sentence))]
    sentence))

(defn expression-pair [source-language-model target-language-model spec]
  "Generate a pair: {:source <source_expression :target <target_expression>}.

   First, the target expression is generated according to the given target-language-model and spec.

   Then, the source expression is generated according to the semantics of this target expression.

   Thus the source expression's spec is not specified directly - rather it is derived from the
   semantics of the target expression."

  (let [no-source-language (if (nil? source-language-model)
                             (throw (Exception. "No source language model was supplied.")))
        no-target-language (if (nil? target-language-model)
                             (throw (Exception. "No target language model was supplied.")))

        ;; 1. generate sentence in target language.
        ;; resolve future
        target-language-model (if (future? target-language-model)
                                @target-language-model
                                target-language-model)

        target-language-sentence (engine/generate spec target-language-model :enrich true)
        
        target-language-sentence (if (:morph-walk-tree target-language-model)
                                   (merge ((:morph-walk-tree target-language-model) target-language-sentence)
                                          target-language-sentence)
                                   (do (log/warn (str "there is no morph-walk-tree function for the model:"
                                                      (:name target-language-model) " of language: "
                                                      (:language target-language-model)))
                                       target-language-sentence))
        target-language-sentence (let [subj (get-in target-language-sentence
                                                    [:synsem :sem :subj] :notfound)]
                                   (cond (not (= :notfound subj))
                                         (do
                                           (log/debug (str "subject constraints: " subj))
                                           (unify target-language-sentence
                                                  {:synsem {:sem {:subj subj}}}))
                                         true
                                         target-language-sentence))
        target-fo (:morph target-language-model)

        semantics (strip-refs (get-in target-language-sentence [:synsem :sem] :top))

        ;; 2. generate sentence in source language using semantics of sentence in target language.
        ;; resolve future
        source-language-model (if (future? source-language-model)
                                @source-language-model
                                source-language-model)
        debug (log/debug (str "semantics of resulting expression: " semantics))
        debug (log/trace (str "entire expression: " target-language-sentence))

        target-language-surface (target-fo target-language-sentence)
        debug (log/debug (str "target surface: " target-language-surface))

        source-language (:language source-language-model)
        error (if (or (nil? target-language-surface)
                      (= target-language-surface ""))
                (let [message (str "Could not generate a sentence in source language '"
                                   (:language source-language-model) 
                                   "' for this spec: " spec)]
                  (if (= true mask-populate-errors)
                    (log/warn message)
                    ;; else
                    (let [target-language-model target-language-model]
                      (log/error message)
                      (log/error "grammar: " (map :rule (:grammar target-language-model)))
                      (log/error "lexicon: " (map (:morph target-language-model)
                                                  (sort (keys (:lexicon target-language-model)))))
                      ;; TODO: add partial parses for more diagnostics:
                      ;; need to try parsing same spec, but without the 
                      ;;  {:synsem {:subcat '()}} that makes us try to generate
                      ;; an entire sentence.
                      (throw (Exception. message))))))
        ;; TODO: add warning if semantics of target-expression is merely :top - it's
        ;; probably not what the caller expected. Rather it should be something more
        ;; specific, like {:pred :eat :subj {:pred :antonio}} ..etc.
        source-language-sentence (engine/generate {:synsem {:sem semantics}}
                                                  source-language-model :enrich true)

        source-language-sentence (if (:morph-walk-tree source-language-model)
                                   (merge ((:morph-walk-tree source-language-model) source-language-sentence)
                                          source-language-sentence)
                                   (do (log/warn (str "there is no morph-walk-tree function for the model:"
                                                      (:name source-language-model) " of language: "
                                                      (:language source-language-model)))
                                       source-language-sentence))

        source-fo (:morph source-language-model)
        source-language-surface (source-fo source-language-sentence)
        debug (log/debug (str "source surface: " source-language-surface))

        error (if (or (nil? source-language-surface)
                      (= source-language-surface ""))
                (let [message (str "Could not generate a sentence in source language '" source-language 
                                   "' for this semantics: " semantics "; target language was: " (:language target-language-model)
                                   "; target expression was: '" ((:morph  target-language-model) target-language-sentence) "'"
                                   "; target rule was: '" (:rule target-language-sentence) "'"
                                   "; target head rule was: '" (:rule (:head target-language-sentence)) "'"
                                   ;; TODO: add partial parses for more diagnostics:
                                   ;; need to try parsing same spec, but without the 
                                   ;;  {:synsem {:subcat '()}} that makes us try to generate
                                   ;; an entire sentence.
                                   )]
                  (if (= true mask-populate-errors)
                    (log/warn message)
                    ;; else
                    (throw (Exception. message)))))]
    {:source source-language-sentence
     :target target-language-sentence}))

(defn insert-expression [expression surface table language model-name]
  "Insert an expression into the given table. The information to be added will be:
   1. the surface form of the expression
   2. The JSON representation of the expression (the structure)
   3. the short name of the language (e.g. 'en','es', or 'it')
   4. the serialized form of the structure

   The structure column allows JSON queries using the containment operator (@>) to 
   find expressions matching a desired specification. See reader.clj for how these
   queries are done.

   The serialized column allows loading a desired expression as a Clojure map into the runtime system, including
   the expression' internal structure-sharing."

  (try
    (exec-raw [(str "INSERT INTO " table " (surface, structure, serialized, language, model) VALUES (?,"
                    "'" (json/write-str (strip-refs expression)) "'"
                    ","
                    "'" (str (serialize expression)) "'"
                    ","
                    "?,?)")
               [surface
                language
                model-name]])
    (catch Exception e
      (log/error (str "SQL error: " (.printStackTrace (.getNextException(.getSQLException e))))))))

(defn insert-lexeme [canonical lexeme language]
  (if (fail? lexeme)
    (let [message (str "Refusing to enter a :fail for canonical form: " canonical)] 
      (log/error message)
      (throw (Exception. message)))
    ;; else, lexeme is valid for insertion
    (exec-raw [(str "INSERT INTO lexeme 
                                 (canonical, structure, serialized, language) 
                          VALUES (?,"
                    "'" (json/write-str (strip-refs lexeme)) "'"
                    ",?,?)")
               [canonical (str (serialize lexeme))
                language]])))

(defn populate-with-language [num model spec]
  (let [spec (if spec spec :top)
        language (:language model)
        debug (log/debug (str "populate-with-language: spec: " spec "; language: " language))]
    (dotimes [n num]
      (let [sentence (expression model spec)
            fo (:morph model)
            surface (fo sentence)]
        (if (empty? surface)
          (let [err-mesg (str "Surface was empty for expression generated from spec: " spec)]
            (log/error err-mesg)
            (log/error (str " expression with empty surface has structure: " (strip-refs sentence)))
            (throw (Exception. err-mesg))))
        (log/info (str "populate-with-language:" language ": surface='"
                       surface "'"))
        (log/debug (str "populate-with-language:" language ": spec='"
                       spec "'"))
        (insert-expression sentence ;; underlying structure
                           surface ;; text of expression
                           "expression" ;; table in database
                           language
                           (:name model)) ;; name of language model; e.g.: 'small','medium'
        ))))

;; TODO: use named optional parameters.
(defn populate [num source-language-model target-language-model & [ spec table ]]
  (let [spec (if spec spec :top)
        debug (log/debug (str "populate spec(1): " spec))
        debug (log/trace (str "type of source language model: " (type source-language-model)))
        debug (log/trace (str "type of target language model: " (type target-language-model)))
        spec (cond
              (not (= :notfound (get-in spec [:synsem :sem :subj] :notfound)))
              (unify spec
                     {:synsem {:sem {:subj (sem-impl (get-in spec [:synsem :sem :subj]))}}})
              true
              spec)

        debug (log/debug (str "populate spec(2): " spec))

        ;; subcat is empty, so that this is a complete expression with no missing arguments.
        ;; e.g. "she sleeps" rather than "sleeps".
        spec (unify spec {:synsem {:subcat '()}}) 

        debug (log/debug (str "populate spec(3): " spec))

        table (if table table "expression")]
    (dotimes [n num]
      (let [sentence-pair (expression-pair source-language-model target-language-model spec)
            target-sentence (:target sentence-pair)
            source-sentence (:source sentence-pair)
            source-language-model (if (ref? source-language-model)
                                    @source-language-model source-language-model)
            target-language-model (if (ref? target-language-model)
                                    @target-language-model target-language-model)

            target-morphology (:morph @target-language-model)
            source-morphology (:morph @source-language-model)

            source-sentence-surface (source-morphology source-sentence)
            target-sentence-surface (target-morphology target-sentence)

            source-language (:language @source-language-model)
            target-language (:language @target-language-model)

            ]

        (log/info (str target-language ": '"
                        target-sentence-surface
                        "'"))
        (insert-expression target-sentence ;; underlying structure
                           target-sentence-surface ; surface string
                           "expression" ;; table
                           target-language
                           (:name target-language-model)) ;; name, like 'small','medium', etc.
        
        (log/info (str source-language ": '"
                       source-sentence-surface
                        "'"))
        (insert-expression source-sentence
                           source-sentence-surface
                           "expression"
                           source-language
                           (:name source-language-model))))))

(defn fill [num source-lm target-lm & [spec]]
  "wipe out current table and replace with (populate num spec)"
  (truncate)
  (let [spec (if spec spec :top)]
    (populate num source-lm target-lm spec)))

(defn populate-from [source-language-model target-language-model target-lex-set target-grammar-set]
  "choose one random member from target-lex-set and one from target-grammar-set, and populate from this pair"
  (let [lex-spec (nth target-lex-set (rand-int (.size target-lex-set)))
        grammar-spec (nth target-grammar-set (rand-int (.size target-grammar-set)))]
    (populate 1 source-language-model target-language-model (unify lex-spec grammar-spec))))

(defn fill-by-spec [spec count table source-model target-model]
  (if (nil? source-model)
    (throw (Exception. "No source language model was supplied.")))
  (if (nil? target-model)
    (throw (Exception. "No target language model was supplied.")))
  (let [target-language (:language @target-model)
        json-spec (json/write-str spec)
        current-target-count
        (:count
         (first
          (exec-raw ["SELECT count(*) FROM expression 
                         WHERE structure @> ?::jsonb
                           AND language=?"
                       [(json/write-str spec) target-language]]
                      :results)))]
    (log/debug (str "current-target-count for spec: " spec "=" current-target-count))
    (let [count (- count current-target-count)]
      (if (> current-target-count 0)
        (log/debug (str "There are already " current-target-count " expressions for: " spec)))
      (if (> count 0)
        (do
          (log/debug (str "Generating "
                         count
                         (if (> current-target-count 0) " more")
                         " expressions for spec: " spec))
          (populate 1
                    source-model
                    target-model
                    spec table)
          (fill-by-spec spec (- count 1) table source-model target-model))
        (log/debug (str "Since no more are required, not generating any for this spec."))))))

(defn fill-language-by-spec [spec count table model]
  (let [model (if (future? model)
                @model
                model)
        language (:language model)
        debug (log/debug (str "fill-language-by-spec: language: " language))
        debug (log/debug (str "fill-language-by-spec: spec: " spec))
        json-spec (json/write-str spec)
        current-count
        (:count
         (first
          (exec-raw ["SELECT count(*) FROM expression 
                         WHERE structure @> ?::jsonb
                           AND language=?"
                       [(json/write-str spec) language]]
                      :results)))]
    (log/debug (str "current-count for spec: " spec "=" current-count))
    (let [count (- count current-count)]
      (if (> current-count 0)
        (log/debug (str "There are already " current-count " expressions for: " spec)))
      (if (> count 0)
        (do
          (log/debug (str "Generating "
                         count
                         (if (> current-count 0) " more")
                         " expressions for spec: " spec))
          (populate-with-language 1 model spec)
          (fill-language-by-spec spec (- count 1) table model))
        (log/debug (str "Since no more are required, not generating any for this spec."))))))

(defn fill-verb [verb count source-model target-model & [spec table]] ;; spec is for additional constraints on generation.
  (let [spec (if spec spec :top)
        target-language-keyword (:language-keyword @target-model)
        tenses [{:synsem {:sem {:tense :conditional}}}
                {:synsem {:sem {:tense :futuro}}}
                {:synsem {:sem {:tense :past :aspect :progressive}}}
                {:synsem {:sem {:tense :past :aspect :perfect}}}
                {:synsem {:sem {:tense :present}}}]]
    (let [spec (unify {:root {target-language-keyword {target-language-keyword verb}}}
                       spec)]
      (log/debug (str "fill-verb spec: " spec))
      ;; use (map) for debugging
      (pmap (fn [tense] (fill-by-spec (unify spec
                                             tense)
                                      count
                                      table
                                      source-model
                                      target-model))
            tenses))))

(defn truncate-lexicon [language]
  (try
    (exec-raw [(str "DELETE FROM lexeme WHERE language=?")
               [language]])
    (catch java.sql.SQLException sqle
      (do
        (log/error (str "Exception when truncating lexicon: " sqle))
        (log/error (str "SQL error: " (.printStackTrace (.getNextException sqle))))))
      
    (catch Exception e
      (do
        (log/error (str "Exception when truncating lexicon: " e))))))


(defn delete-from-expressions [language spec]
  (try
    (do
      (log/debug (str "deleting "
                      (:count
                       (first (exec-raw [(str "SELECT count(*) 
                                                        FROM expression 
                                                       WHERE language=?
                                                         AND (structure @> ?::jsonb)")
                                         [language (json/write-str spec)]]
                                        :results)))
                      " expression(s) from the language: " language " because each matches spec: " spec))
      (exec-raw ["DELETE FROM expression 
                   WHERE language=?
                     AND (structure @> ?::jsonb)"
                 [language
                  (json/write-str spec)]]))


    (catch java.sql.SQLException sqle
      (do
        (log/error (str "SQL Exception when deleting expressions: " sqle))
        (log/error (str "next exception:" (.getNextException sqle)))
        (log/error (str "SQL error: " (.printStackTrace (.getNextException sqle))))))
    
    (catch Exception e
      (do
        (log/error (str "Exception when deleting expressions: " e))))))
  
(defn write-lexicon [language lexicon]
  (truncate-lexicon language)
  (loop [canonical (sort (keys lexicon)) result nil]
    (if (not (empty? canonical))
      (let [canonical-form (first canonical)]
        (log/info (str language ":" canonical-form))
        (loop [lexemes (get lexicon canonical-form) inner-result nil]
          (if (not (empty? lexemes))
            (do
              (insert-lexeme canonical-form (first lexemes) language)
              (recur (rest lexemes) inner-result))
            inner-result))
        (recur (rest canonical) result))
      result)))

(defn process [units target-language]
  (log/debug "Starting processing with: " (.size units) " instruction(s) for language " target-language)

  ;; cleanup: TODO: use temporary tables
  (let [import_table (str "expression_import_" target-language "_" (rand-int 1000000))
        expression_distinct_table (str "expression_distinct_" target-language "_" (rand-int 1000000))]

    ;; cleanup: TODO: use temporary tables
    (exec-raw (str "DROP TABLE IF EXISTS " import_table))

    (exec-raw (str "CREATE TABLE " import_table " (
    language text,
    model text,
    surface text,
    structure jsonb,
    serialized text)"))

    (log/debug (str "truncating import_table: " import_table))
    (exec-raw [(str "TRUNCATE " import_table)])

    (log/debug (str "Units: " (.size units)))
    (.size (map (fn [unit]
                  (log/trace (str "TYPE OF UNIT: " (type unit)))
                  (log/trace (str "KEYS OF UNIT: " (keys unit)))
                  (let [member-of-unit unit]
                    (log/debug (str "keys of member-of-unit:" (keys member-of-unit)))
                    (if (:sql member-of-unit)
                      (do
                        (log/debug (str "doing sql: " (:sql member-of-unit)))
                        (exec-raw (:sql member-of-unit))))
                    
                    (if (:fill member-of-unit)
                      (let [count (or (->> member-of-unit :fill :count) 10)]
                        (log/debug (str "doing fill-by-spec: " (->> member-of-unit :fill :spec)
                                        "; count=" count))
                        (fill-by-spec
                         (->> member-of-unit :fill :spec)
                         count
                         "expression_import"
                         (->> member-of-unit :fill :source-model)
                         (->> member-of-unit :fill :target-model))))
                    (if (:fill-one-language member-of-unit)
                      (let [count (or (->> member-of-unit :fill-one-language :count) 10)]
                        (log/debug (str "doing fill-one-language: " (->> member-of-unit :fill-one-language :spec)
                                        "; count=" count))
                        (fill-language-by-spec
                         (->> member-of-unit :fill-one-language :spec)
                         count
                         "expression_import"
                         (->> member-of-unit :fill-one-language :model))))
                    (if (:fill-verb member-of-unit)
                      (do
                        (log/info (str "Doing fill-verb: " (:fill-verb member-of-unit)))
                        (let [verb (:fill-verb member-of-unit)]
                          (.size (fill-verb
                                  (:fill-verb member-of-unit)
                                  (if (:count member-of-unit)
                                    (:count member-of-unit)
                                    1)
                                  (->> member-of-unit :fill-verb :source-model)
                                  (->> member-of-unit :fill-verb :target-model))))))))
                units))
    (exec-raw [(str "SELECT count(*) FROM " import_table)] :results)

    ;; cleanup: TODO: use temporary tables
    (exec-raw (str "DROP TABLE IF EXISTS " expression_distinct_table))

    (exec-raw (str "CREATE TABLE " expression_distinct_table " (
    language text,
    model text,
    surface text,
    structure jsonb,
    serialized text)"))

    (exec-raw (str "INSERT INTO " expression_distinct_table " (language,model,surface,structure,serialized) 
         SELECT DISTINCT language,model,surface,structure,serialized 
                    FROM " import_table ""))

    (exec-raw (str "INSERT INTO expression (language,model,surface,structure,serialized)
                    SELECT language,model,surface,structure,serialized
                      FROM " expression_distinct_table ""))

    ;; cleanup: TODO: use temporary tables
    (exec-raw (str "DROP TABLE " expression_distinct_table))
    (exec-raw (str "DROP TABLE " import_table))
    
    ))

;; (process accompany care fornire indossare moltiplicare recuperare riconoscere riscaldare)
