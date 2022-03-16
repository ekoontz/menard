(ns menard.handlers
  (:require
   [clojure.tools.logging :as log]
   [java-time :refer [instant minus]]
   [dag_unify.core :as u]
   [menard.english :as en]
   [menard.nederlands :as nl]
   [menard.translate.spec :as tr]))

(defn dag-to-string [dag]
  (-> dag dag_unify.serialization/serialize str))

(defn generate-nl
  "generate a Dutch expression from _spec_ and translate to English, and return this pair
   along with the semantics of the English specification also."
  [spec]
  (let [debug (log/debug (str "generate-nl: generating a question with spec: " spec))
        ;; 1. generate a target expression
        target-expression (->> (repeatedly #(-> spec nl/generate))
                               (remove empty?)
                               (take 1)
                               first)
        target-semantics (-> target-expression (u/get-in [:sem]))

        ;; 2. try twice to generate a source expression: fails occasionally for unknown reasons:
        source-expression (->> (repeatedly #(-> target-expression tr/nl-to-en-spec en/generate))
                               (remove empty?)
                               (take 1)
                               first)
        debug (log/debug (str "target-semantics: " (-> target-semantics
                                                       dag-to-string)))
        ;; 3. get the semantics of the source expression
        source-parses (binding [menard.morphology/show-notes? false
                                menard.parse/take-this-many 100]
                        (->> source-expression
                             en/morph
                             en/parse))
        debug (log/debug (str "source parses:"
                              (->> source-parses
                                   (map en/syntax-tree)
                                   (clojure.string/join ","))))
        source-semantics (->> source-parses
                              (map #(u/get-in % [:sem])))
        debug (log/debug (str "source semantics: "
                              (->> source-semantics
                                   (map dag-to-string)
                                   (clojure.string/join ","))))
        debug (log/debug (str "fail-paths: "
                             (->> source-semantics
                                  (map (fn [source-sem]
                                         (dag_unify.diagnostics/fail-path source-sem
                                                                          target-semantics)))
                                  (clojure.string/join ","))))
        ;; filter source-semantics to find subset that is compatible with target semantics:
        source-semantics (->> source-semantics
                              (remove #(= :fail (u/unify % target-semantics))))]
    (if (empty? source-semantics)
      (log/error (str "no source semantics compatible with target semantics.!")))
    (log/debug (str "given input input spec: "
                   (-> spec (dissoc :cat) (dissoc :sem))
                   ", generated: '" (-> source-expression en/morph) "'"
                   " -> '"  (-> target-expression nl/morph) "'"))
    (let [result
          {:source (-> source-expression en/morph)
           :target (-> target-expression nl/morph)
           :source-tree (-> source-expression en/syntax-tree)
           :target-tree (-> target-expression nl/syntax-tree)
           :target-root (-> target-expression (u/get-in [:head :root] :top))
           :source-sem (map dag-to-string source-semantics)}]
      (when (empty? source-expression)
        (log/error (str "failed to generate a source expression for spec: " spec "; target expression: "
                       (nl/syntax-tree target-expression)))
        (log/error (str " tried to generate from: "
                        (dag_unify.serialization/serialize (-> target-expression tr/nl-to-en-spec)))))
      (log/debug (str "generate-nl: returning result: " result))
      result)))

(def ^:const clean-up-trees true)

(defn generate-nl-by-spec
  "decode a spec from the input request and generate with it."
  [spec]
  (log/debug (str "spec pre-decode: " spec))
  (let [spec (-> spec read-string dag_unify.serialization/deserialize)]
    (log/debug (str "generate-by-spec with spec: " spec))
    (-> spec
        generate-nl
        (dissoc :source-tree)
        (dissoc :target-tree))))

(defn generate-nl-with-alternations
  "generate with _spec_ unified with each of the alternates, so generate one expression per <spec,alternate> combination."
  [spec alternates]
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
          first-expression (generate-nl (first derivative-specs))
          expressions
          (cons first-expression
                (->> (rest derivative-specs)
                     (map (fn [derivative-spec]
                            (generate-nl (u/unify derivative-spec
                                                  {:head {:root
                                                          (u/get-in first-expression [:target-root] :top)}}))))))]
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

(defn generate-en [spec]
  (log/debug (str "generate-en spec: " spec))
  (let [spec (-> spec read-string dag_unify.serialization/deserialize)
        phrasal? (u/get-in spec [:phrasal?] true)
        result (->> (repeatedly #(-> spec
                                     en/generate))
                    (take 2)
                    (filter #(not (nil? %)))
                    first)]
    (log/debug (str "generate-en: phrasal? " phrasal?))
    (when (nil? result)
      (log/warn (str "failed to generate on two occasions with spec: "
                     (dag-to-string spec))))
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
              (-> nl/model deref :grammar)
              (= lang "en")
              (-> en/model deref :grammar)
              true [])]
    (log/debug (str "grammar for lang: " lang " unserialized: " unserialized))
    (->> unserialized
         (map (fn [rule] (-> rule dag_unify.serialization/serialize str))))))

(defn morphology [lang]
  (let [unserialized
        (cond (= lang "nl")
              (-> nl/model deref :morphology)
              (= lang "en")
              (-> en/model deref :morphology)
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

(defn parse-nl-start [string-to-parse]
  (log/info (str "parsing user guess: " string-to-parse))
  (let [nl-tokens (nl/tokenize string-to-parse)]
    (cond (> (count nl-tokens) 1)
          (let [intermediate-result (->> string-to-parse
                                         clojure.string/lower-case
                                         nl/parse-start)]
            (log/debug (str "parse-nl-start: intermediate-result: "
                            intermediate-result))
            (into {}
                  (->> (keys intermediate-result)
                       (map (fn [k]
                              [(str k)
                               (map (fn [x] (-> x dag_unify.serialization/serialize str))
                                    (get intermediate-result k))]))))))))

(defn parse-en-start [string-to-parse]
  (log/info (str "parsing user guess: " string-to-parse))
  (let [en-tokens (en/tokenize string-to-parse)]
    (cond (> (count en-tokens) 1)
          (let [intermediate-result (->> string-to-parse
                                         clojure.string/lower-case
                                         en/parse-start)]
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
  (log/info (str "parsing user guess: " string-to-parse ".."))
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
  (cond (= language "en") en/model
        (= language "nl") nl/model
        :else nl/model))

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
