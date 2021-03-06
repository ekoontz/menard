(ns menard.lambda.handlers
  (:require
   [clojure.tools.logging :as log]
   [dag_unify.core :as u]
   [menard.english :as en]
   [menard.nederlands :as nl]
   [menard.translate :as tr]))

(defn dag-to-string [dag]
  (-> dag dag_unify.serialization/serialize str))

(defn generate-nl
  "generate a Dutch expression from _spec_ and translate to English, and return this pair
   along with the semantics of the English specification also."
  [spec]
  (let [debug (log/debug (str "generate-nl: generating a question with spec: " spec))
        ;; 1. generate a target expression
        target-expression (->> (repeatedly #(-> spec nl/generate))
                               (take 4)
                               (remove empty?)
                               first)
        target-semantics (-> target-expression (u/get-in [:sem]))

        ;; 2. try twice to generate a source expression: fails occasionally for unknown reasons:
        source-expression (->> (repeatedly #(-> target-expression tr/nl-to-en-spec en/generate))
                               (take 2)
                               (remove empty?)
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
        ;; filter soource-semantics to find subset that is compatible with target semantics:
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
                                                          (u/get-in first-expression [:target-tree :head :root] :top)}}))))))]
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

(defn- generate-english [spec nl]
  (let [phrasal? (u/get-in spec [:phrasal] true)
        result (->> (repeatedly #(-> spec
                                     en/generate))
                    (take 2)
                    (filter #(not (nil? %)))
                    first)]
    (log/debug (str "generate-english: phrasal? " phrasal?))
    (when (nil? result)
      (log/warn (str "failed to generate on two occasions with nl: '" nl "'; spec: "
                     (dag-to-string spec))))
    result))

(defn en-word-spec [nl-word]
  {:agr (u/get-in nl-word [:agr] :top)
   :case (u/get-in nl-word [:case] :top)
   :cat (u/get-in nl-word [:cat] :top)
   :sem {:pred (u/get-in nl-word [:sem :pred] :top)}})

(defn nl-to-en-by-token [nl-tokens]
  (->> nl-tokens
       (map nl/analyze)
       (map first)
       (map en-word-spec)
       (map #(menard.generate/get-lexemes % menard.english/index-fn))
       (map #(not (= :top (u/get-in % [:sem :pred] :top))))
       (map first)))

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
                   ;; parsing the expression succeeded.
                   (not (empty? nl-parse-attempts))
                   (->> nl-parses
                        (map (fn [nl-parse]
                               (let [en-spec (tr/nl-to-en-spec nl-parse)]
                                 (log/debug (str "parse-nl: nl-spec: " (u/pprint nl-parse)))
                                 (log/debug (str "parse-nl: en-spec: " (u/pprint en-spec)))
                                     en-spec))))
                   ;; parsing the expression did not succeed, falling back to trying to analyze each token separately.
                   true
                   (->> nl-tokens
                        (map nl/analyze)
                        (map first)
                        (map en-word-spec)))
        en-parses (cond
                    (not (empty? nl-parse-attempts))
                    (->> en-specs
                         (map #(generate-english %
                                                 (clojure.string/join ","
                                                                      (map nl/syntax-tree nl-parses)))))
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
                                              (->> en-parses (map en/morph)))
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
