(ns menard.handlers
  (:require
   [clojure.tools.logging :as log]
   [java-time :refer [instant minus]]
   [dag_unify.core :as u]
   [dag_unify.serialization :refer [serialize]]
   [dag_unify.diagnostics :refer [strip-refs]]
   [menard.english :as en]
   [menard.english.complete :as en-complete]
   [menard.english.woordenlijst :as en-woordenlijst]
   [menard.nederlands :as nl]
   [menard.nederlands.basic :as nl-basic]
   [menard.nederlands.complete :as nl-complete]
   [menard.nederlands.woordenlijst :as nl-woordenlijst]
   [menard.translate.spec :as tr]))

(defn dag-to-string [dag]
  (-> dag dag_unify.serialization/serialize str))

(def source-generation-tries 2)

(defn get-target-model [& [given-model-name]]
  (let [model-name (or given-model-name "complete-model")]
    (cond (= "woordenlijst-model" model-name)
          nl-woordenlijst/model
          (= "woordenlijst" model-name)
          nl-woordenlijst/model

          (= "basic-model" model-name)
          nl-basic/model
          (= "basic" model-name)
          nl-basic/model

          (= "complete-model" model-name)
          nl-complete/model
          (= "complete" model-name)
          nl-complete/model

          given-model-name
          (do
            (log/warn (str "request-supplied target-model: '" given-model-name "' doesn't exist: falling back to nl-complete/model."))
            nl-complete/model)

          :else nl-complete/model)))

(defn get-source-model [& [given-model-name]]
  (let [result
        (let [model-name (or given-model-name "complete-model")]
          (cond (= "woordenlijst-model" model-name)
                en-woordenlijst/model
                (= "woordenlijst" model-name)
                en-woordenlijst/model

                (= "complete-model" model-name)
                en-complete/model
                (= "complete" model-name)
                en-complete/model
                
                given-model-name
                (do
                  (log/warn (str "request-supplied source-model: '" given-model-name "' doesn't exist: falling back to en-complete/model."))
                  en-complete/model)
                
                :else en-complete/model))]
    result))

(defn generate-nl-and-en
  "generate a Dutch expression from _spec_ and _target_model_ and
  translate to English with _source_model, and return this pair
   along with the semantics of the English specification also."
  [spec target-model source-model]
  (let [target-model (cond (= (type target-model) clojure.lang.Ref)
                           @target-model
                           :else target-model)
        source-model (cond (= (type source-model) clojure.lang.Ref)
                           @source-model
                           :else source-model)
        debug (log/debug (str "generate-nl-and-en: generating a target expression with spec: " (serialize spec)))

        ;; 1. generate a target expression
        nl-spec (merge spec {:language "nl"})
        target-expression (->> (repeatedly #(nl/generate spec target-model))
                               (remove empty?)
                               (take 1)
                               first)
        target-semantics (-> target-expression (u/get-in [:sem]))

        debug (log/info (str "target-expression st: " (-> target-expression nl/syntax-tree)))

        
        ;; 2. try _source-generation-tries_ to generate a source expression: fails occasionally for unknown reasons:
        debug (log/info (str "generate-nl-and-en: target-semantics: " (serialize target-semantics)))
        source-expression
        (->> (repeatedly #(-> target-expression tr/nl-to-en-spec
                              (merge {:language "en"})
                              ((fn [spec]
                                 (log/info (str "generate-nl-and-en: en/generate with spec: " (serialize spec)))
                                 (en/generate spec source-model)))))
             (take source-generation-tries)
             (remove empty?) ;; remove failed attempts
             (take 1)
             first)
        ;; 3. get the semantics of the source expression
        source-parses (binding [menard.morphology/show-notes? false]
                        (log/info (str "parsing source-expression: "
                                       (-> source-expression
                                           en/morph)))
                        (-> source-expression
                            en/morph
                            (en/parse source-model)))
        debug (log/debug (str "source parses:"
                              (->> source-parses
                                   (map en/syntax-tree)
                                   (clojure.string/join ","))))
        warn (if (empty? source-parses)
               (if (empty source-expression)

                 ;; 1. was not able to generate an english expression from the spec:
                 (log/warn (str "generate-nl-and-en: no english expression could be generated for spec: " (-> target-expression tr/nl-to-en-spec strip-refs)))
               

                 ;; 2. was able to generate an english expression, but
                 ;; couldn't parse what was generated:
                 (log/warn (str "generate-nl-and-en: no source (english) parses found for: "
                              "'"
                              (-> source-expression en/morph)
                              "'"))))
        source-semantics (->> source-parses
                              (map #(u/get-in % [:sem])))
        debug (if (= :fail source-semantics)
                (log/debug (str "fail-paths: "
                                (->> source-semantics
                                     (map (fn [source-sem]
                                            (dag_unify.diagnostics/fail-path source-sem
                                                                             target-semantics)))
                                     (clojure.string/join ",")))))

        ;; filter source-semantics to find subset that is compatible with target semantics:
        source-semantics (->> source-semantics
                              (remove #(= :fail (u/unify % target-semantics))))]
    (if (empty? source-semantics)
      (log/error (str "no source semantics compatible with target semantics: "
                      (-> target-semantics strip-refs))))
    (log/debug (str "given input input spec: "
                   (-> spec (dissoc :cat) (dissoc :sem))
                   ", generated: '" (-> source-expression en/morph) "'"
                   " -> '"  (-> target-expression nl/morph) "'"))
    (let [result
          {:source (-> source-expression ((:morph-fn source-model)))
           :target (-> target-expression ((:morph-fn target-model)))
           :source-tree (-> source-expression ((:syntax-tree-fn source-model)))
           :target-tree (-> target-expression ((:syntax-tree-fn target-model)))
           :target-root (-> target-expression (u/get-in [:head :root] :top))
           :source-sem (map dag-to-string source-semantics)}]
      (when (empty? source-expression)
        (log/error (str "failed to generate a source expression using source model with name: '" (-> source-model :name) "' for spec: " spec "; target expression: "
                        (nl/syntax-tree target-expression)))
        (log/error (str " tried to generate from: "
                        (dag_unify.serialization/serialize (-> target-expression tr/nl-to-en-spec)))))
      (log/debug (str "generate-nl: returning result: " result))
      result)))

(def ^:const clean-up-trees true)

(defn generate-nl-and-en-by-spec
  "decode a spec from the input request and generate with it."
  [spec nl-model en-model]
  (log/debug (str "spec pre-decode: " spec))
  (let [spec (-> spec read-string dag_unify.serialization/deserialize)]
    (log/debug (str "generate-by-spec: (pre)  input spec: " spec "; generating now.."))
    (let [result
          (generate-nl-and-en
           (-> spec
               (dissoc :source-tree)
               (dissoc :target-tree))
           nl-model en-model)]
      (log/debug (str "generate-by-spec: (post) input spec: " spec "; result: " result))
      result)))

(defn generate-nl-with-alternations
  "generate with _spec_ unified with each of the alternates, so generate one expression per <spec,alternate> combination."
  [spec alternates target-model source-model]
  (if (or (nil? alternates) (empty? alternates))
    (throw (Exception. "alternates were unexpectedly not provided.")))
  (let [alternates (map dag_unify.serialization/deserialize (read-string alternates))
        spec (-> spec read-string dag_unify.serialization/deserialize)]
    (log/debug (str "generate-nl-with-alternations: spec: " spec))
    (let [derivative-specs
          (->>
           alternates
           (map (fn [alternate]
                  (u/unify alternate spec))))
          ;; the first one is special: we will get the [:head :root] from it
          ;; and use it with the rest of the specs.
          first-expression (generate-nl-and-en (first derivative-specs) target-model source-model)
          expressions
          (cons first-expression
                (->> (rest derivative-specs)
                     (map (fn [derivative-spec]
                            (generate-nl-and-en (u/unify derivative-spec
                                                         {:head {:root
                                                                 (u/get-in first-expression [:target-root] :top)}})
                                                target-model
                                                source-model)))))]
      (if clean-up-trees
        (->> expressions
             ;; cleanup the huge syntax trees:
             (map #(-> %
                       (dissoc % :source-tree (dag-to-string (:source-tree %)))
                       (dissoc % :target-tree (dag-to-string (:target-tree %))))))

      ;; don't cleanup the syntax trees, but serialize them so they can be printed to json:
      (map #(-> %
                (assoc :source-tree (dag-to-string (:source-tree %)))
                (assoc :target-tree (dag-to-string (:target-tree %)))))))))

(defn generate-en [spec model]
  (log/debug (str "generate-en spec: " spec))
  (let [spec (-> spec read-string dag_unify.serialization/deserialize)
        phrasal? (u/get-in spec [:phrasal?] true)
        debug (log/debug (str "generate-en: spec: " spec "; model name: "
                              (-> model :name)))
        result (->> (repeatedly #(-> spec
                                     (en/generate model)))
                    (take 2)
                    (filter #(not (nil? %)))
                    first)]
    (log/debug (str "generate-en: phrasal? " phrasal?))
    (when (nil? result)
      (log/error (str "failed to generate on two occasions with spec: "
                      (dag-to-string spec) " and model named: "
                      (-> model :name))))
    (log/debug (str "generate-en: result: " (-> result en/syntax-tree)))
    {:tree (-> result en/syntax-tree)
     :sem (-> result (u/get-in [:sem]) dag_unify.serialization/serialize)
     :surface (-> result en/morph)}))

(defn en-word-spec [nl-word]
  {:agr (u/get-in nl-word [:agr] :top)
   :case (u/get-in nl-word [:case] :top)
   :cat (u/get-in nl-word [:cat] :top)
   :sem {:pred (u/get-in nl-word [:sem :pred] :top)}})

(defn grammar [lang]
  (let [unserialized
        (cond (= lang "nl")
              (-> nl-complete/model deref :grammar)
              (= lang "en")
              (-> en-complete/model deref :grammar)
              true [])]
    (log/debug (str "grammar for lang: " lang " unserialized: " unserialized))
    (->> unserialized
         (map (fn [rule] (-> rule dag_unify.serialization/serialize str))))))

(defn morphology [lang]
  (let [unserialized
        (cond (= lang "nl")
              (-> nl-complete/model deref :morphology)
              (= lang "en")
              (-> en-complete/model deref :morphology)
              true [])]
    (log/debug (str "morphology for lang: " lang " unserialized: " unserialized))    
    (->> unserialized
         (map (fn [{[generate-from generate-to] :g
                    [parse-from parse-to] :p
                    u :u}]
                ;; regexes like: #"ot$" have to be
                ;; converted into strings: "ot$" via (str).
                ;; The inverse of this is
                ;; clojure.core/re-pattern (converts a
                ;; string e.g. "ot$" into a regex #"ot$".
                {:u u
                 :g [(str generate-from) generate-to]
                 :p [(str parse-from) parse-to]})))))

(defn nl-to-en-by-token [nl-tokens]
  (->> nl-tokens
       (map nl/analyze)
       (map first)
       (map en-word-spec)
       (map #(menard.generate/get-lexemes % menard.english/index-fn))
       (map #(not (= :top (u/get-in % [:sem :pred] :top))))
       (map first)))

(defn parse-nl-start [string-to-parse model]
  (log/debug (str "menard.handler/parse-nl-start input: '" string-to-parse "' with model name:" (:name model)))  
  (->> (-> string-to-parse
           clojure.string/lower-case
           (nl/parse-start model))
       (map (fn [intermediate-result]
              (into {}
                    (->> (keys intermediate-result)
                         (map (fn [k]
                                [(str k)
                                 (map (fn [x] (-> x dag_unify.serialization/serialize str))
                                      (get intermediate-result k))]))))))))

(defn parse-en-start [string-to-parse model]
  (log/debug (str "parse-en-start input: '" string-to-parse "'"))
  (let [en-tokens (en/tokenize string-to-parse)]
    (cond (> (count en-tokens) 1)
          (let [intermediate-result (->> string-to-parse
                                         clojure.string/lower-case
                                         (en/parse-start model))]
            (log/debug (str "parse-en-start: intermediate-result: "
                            intermediate-result))
            (into {}
                  (->> (keys intermediate-result)
                       (map (fn [k]
                              [(str k)
                               (map (fn [x] (-> x dag_unify.serialization/serialize str))
                                    (get intermediate-result k))]))))))))

(defn parse-nl-all [string-to-parse]
  (log/info (str "parsing user guess: " string-to-parse ".."))
  (let [nl-tokens (nl/tokenize string-to-parse)]
    (if (> (count nl-tokens) 1)
      (let [start-time (.toEpochMilli (instant))]
        (let [retval
              (->> string-to-parse
                   clojure.string/lower-case
                   nl/parse-all)]
          (log/info (str "parse: '" string-to-parse "': took " (- (.toEpochMilli (instant)) start-time) "ms."))
          retval)))))

(defn parse-en-all [string-to-parse]
  (log/info (str "parse-en-all input: '" string-to-parse "'.."))
  (let [en-tokens (en/tokenize string-to-parse)]
    (if (> (count en-tokens) 1)
      (let [start-time (.toEpochMilli (instant))]
        (let [retval
              (->> string-to-parse
                   clojure.string/lower-case
                   en/parse-all)]
          (log/info (str "parse: '" string-to-parse "': took " (- (.toEpochMilli (instant)) start-time) "ms."))
          retval)))))

(defn language-to-analyze-fn [language]
  (cond (= language "en") en/analyze
        (= language "nl") nl/analyze
        :else nl/analyze))

(defn language-to-model [language]
  (cond (= language "en") en-complete/model
        (= language "nl") nl-complete/model
        :else nl-complete/model))

(defn analyze [string-to-analyze language]
  ((-> language
       language-to-analyze-fn)
   string-to-analyze))

(defn rules [rule-name language]
  (let [model (language-to-model language)]
    (->> (-> model deref :grammar)
         (filter #(= (:rule %) rule-name)))))

(defn parse-nl [string-to-parse]
  (log/info (str "parsing user guess: " string-to-parse))
  (let [nl-tokens (nl/tokenize string-to-parse)
        nl-parse-attempts (cond (> (count nl-tokens) 1)
                                (->> string-to-parse
                                     clojure.string/lower-case
                                     nl/parse))
        nl-parses (if (not (empty? nl-parse-attempts))
                    (->> nl-parse-attempts
                         (filter #(or (= [] (u/get-in % [:subcat]))
                                      (= :top (u/get-in % [:subcat]))
                                      (= ::none (u/get-in % [:subcat] ::none))))
                         (filter #(= nil (u/get-in % [:mod] nil)))
                         (sort (fn [a b] (> (count (str a)) (count (str b)))))))
        en-specs (cond
                   ;; parsing the expression succeeded:
                   (seq nl-parse-attempts)
                   (->> nl-parses
                        (map (fn [nl-parse]
                               (let [en-spec (tr/nl-to-en-spec nl-parse)]
                                 (log/debug (str "parse-nl: nl-spec: " (u/pprint nl-parse)))
                                 (log/debug (str "parse-nl: (successful) en-spec: " (u/pprint en-spec)))
                                 en-spec))))
                   ;; parsing the expression did not succeed, falling back to trying to analyze each token separately.
                   true
                   (->> nl-tokens
                        (map nl/analyze)
                        (map first)
                        (map en-word-spec)))
        en-parses (cond
                    ;; one or more complete parses of the user's input in Nederlands:
                    (seq nl-parse-attempts)
                    (map #(generate-en %) en-specs)

                    ;; no complete parses of the user's input in Nederlands:
                    true
                    (->> en-specs
                         (map (fn [en-spec]
                                (let [matching-lexemes
                                      (menard.english/index-fn en-spec)]
                                  (->> matching-lexemes
                                       (filter (fn [matching-lexeme]
                                                 (not (= :top (u/get-in matching-lexeme
                                                                        [:sem :pred] :top)))))
                                       (map (fn [matching-lexeme]
                                              (u/unify matching-lexeme
                                                       en-spec)))
                                       (filter #(not (= :fail %)))))))
                         (map (fn [matching-lexemes]
                                (filter (fn [matching-lexeme]
                                          (not (= :top (u/get-in matching-lexeme
                                                                 [:sem :pred] :top))))
                                        matching-lexemes)))
                         (map first)))]
    (let [new
          {:nl {:surface string-to-parse
                :tokens nl-tokens
                :sem (->> nl-parses
                          (map #(u/get-in % [:sem]))
                          (map dag-to-string))
                :trees (->> nl-parses
                            (map nl/syntax-tree))}
           :en {:surface (clojure.string/join ", "
                                              (->> en-parses (map :surface)))
                :specs (->> en-specs
                            (map dag-to-string))
                :sem (->> en-parses
                          (map #(u/get-in % [:sem]))
                          (map dag-to-string))
                :trees (->> en-parses (map en/syntax-tree))}}]
      ;; backward compatibility: nlquiz expects:
      ;; 1. semantics at [:sem], not [:nl :sem]
      ;; 2. english surface at [:english], not [:en :surface].
      (let [retval
            (merge new
                   {:sem (get-in new [:nl :sem])
                    :english (get-in new [:en :surface])})]
        retval))))
