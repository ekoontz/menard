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
  (let [debug (log/info (str "generating a question with spec: " spec))
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
        ;; 3. get the semantics of the source expression
        source-semantics (binding [menard.morphology/show-notes? false]
                           (->> source-expression en/morph en/parse
                                (map #(u/get-in % [:sem]))
                                (remove #(= :fail (u/unify % target-semantics)))))]
    (log/info (str "given input input spec: "
                   (-> spec (dissoc :cat) (dissoc :sem))
                   ", generated: '" (-> source-expression en/morph) "'"
                   " -> '"  (-> target-expression nl/morph) "'"))
    (let [result
          {:source (-> source-expression en/morph)
           :target (-> target-expression nl/morph)
           :source-tree source-expression
           :target-tree target-expression
           :target-root (-> target-expression (u/get-in [:head :root] :top))
           :source-sem (map dag-to-string source-semantics)}]
      (when (empty? source-expression)
        (log/error (str "failed to generate a source expression for spec: " spec "; target expression: "
                       (nl/syntax-tree target-expression)))
        (log/error (str " tried to generate from: "
                        (dag_unify.serialization/serialize (-> target-expression tr/nl-to-en-spec)))))
      result)))

(def ^:const clean-up-trees true)

(defn generate-nl-by-spec
  "decode a spec from the input request and generate with it."
  [spec]
  (log/info (str "spec pre-decode: " spec))
  (let [spec (-> spec read-string dag_unify.serialization/deserialize)]
    (log/info (str "generate-by-spec with spec: " spec))
    (-> spec
        generate-nl
        (dissoc :source-tree)
        (dissoc :target-tree))))

(defn- generate-english [spec nl]
  (let [result (->> (repeatedly #(-> spec
                                     en/generate))
                    (take 2)
                    (filter #(not (nil? %)))
                    first)]
    (when (nil? result)
      (log/warn (str "failed to generate on two occasions with nl: '" nl "'")))
    result))

(defn generate-nl-with-alternations
  "generate with _spec_ unified with each of the alternates, so generate one expression per <spec,alternate> combination."
  [spec alternates]
  (let [alternates (map dag_unify.serialization/deserialize (read-string alternates))
        spec (-> spec read-string dag_unify.serialization/deserialize)]
    (log/info (str "generate-nl-with-alternations: spec: " spec))
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

(defn parse-nl [string-to-parse]
  (log/info (str "parsing input: " string-to-parse))
  (let [parses (->> string-to-parse
                    clojure.string/lower-case
                    nl/parse
                    (filter #(or (= [] (u/get-in % [:subcat]))
                                 (= :top (u/get-in % [:subcat]))
                                 (= ::none (u/get-in % [:subcat] ::none))))
                    (filter #(= nil (u/get-in % [:mod] nil)))
                    (sort (fn [a b] (> (count (str a)) (count (str b))))))
        syntax-trees (->> parses (map nl/syntax-tree))
        english (-> (->> parses
                         (map tr/nl-to-en-spec)
                         (map #(generate-english %
                                                 (clojure.string/join "," (map nl/syntax-tree parses))))
                         (map #(en/morph %))))]
    (log/info (str "nl: '" string-to-parse "' -> ["
                   (clojure.string/join "," english) "]"))
    {:nederlands string-to-parse
     :english (clojure.string/join ", " english)
     :trees syntax-trees
     :sem (->> parses
               (map #(u/get-in % [:sem]))
               (map dag-to-string))}))
