(ns menard.nederlands

  ;; TODO: don't we need to have a :require-macros
  ;; for menard.morphology, too?
  #?(:cljs (:require-macros [menard.grammar]))

  (:require [clojure.core.async :refer [go-loop]]
            [clojure.string :as string]
            [config.core :refer [env]]
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            [menard.generate :as g]
            [menard.grammar :as grammar]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [menard.model :as model :refer [current-ms
                                            get-info-of-files]]
            [menard.morphology :as m]
            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.nederlands.tenses :as tenses]
            [menard.nederlands.complete :as complete]
            [menard.nesting]
            [menard.parse :as p]
            [menard.serialization :as s]
            [menard.subcat]
            [menard.ug]
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [deserialize]]))
;;
;; For generation and parsing of Dutch.
;;

;; for parsing diagnostics:
;;(def log-these-rules #{"vp-conj"})
;;(def log-these-rules #{"np:1" "np:2" "nbar"})
;;(def log-these-rules #{"s"})
;;(def log-these-rules #{"conj-outer"})
(def log-these-rules #{})
(def truncate? true)

#?(:clj
   (defn get-inflection-of [lexeme morphology]
     (when lexeme
       (->> morphology
            (map (fn [rule]
                   {:u (reduce unify
                               [lexeme (:u rule)
                                {:cat :noun
                                 :exception false
                                 :agr {:number :plur}}])
                    :m (re-find (-> rule :g first)
                                (:canonical lexeme))}))
            (filter (fn [x] (and (not (= :fail (:u x)))
                                 (not (nil? (:m x))))))
            (map (fn [result]
                   (-> result :u :inflection)))
            first))))


#?(:cljs
   (def model
     (atom nil))) ;; TODO: add call to macro function like with morphology/compile-morphology.

#?(:clj
(defn create-model-from-filesystem [spec & [use-env]]
  (if (nil? spec)
    (exception (str "create-model-from-filesystem: spec was nil.")))
  (let [env (or use-env env)]
    (log/info (str "create-model-from-filesystem: menard-dir env: " (:menard-dir env)))
    (log/info (str "create-model-from-filesystem: spec: " spec))
    (if (empty? (:menard-dir env))
      (exception (str "you must set MENARD_DIR in your environment.")))
    (let [menard-dir (str (:menard-dir env) "/")]
      (log/debug (str "loading ug.."))
      (menard.ug/load-from-file)
      (log/debug (str "loading nesting.."))
      (menard.nesting/load-from-file)
      (log/debug (str "loading subcat.."))
      (menard.subcat/load-from-file)
      (log/debug (str "loading grammar.."))
      (let [grammar (model/load-grammar-from-file (str "file://" menard-dir "resources/"
                                                       (-> spec :grammar)))]
        (log/debug (str "loaded " (count grammar) " grammar rules."))
        (log/debug (str "loading morphology.."))
        (let [morphology (model/load-morphology (str "file://" menard-dir "resources/"
                                                     (-> spec :morphology :path) "/")
                                                (-> spec :morphology :sources))]
          (log/debug (str "loaded " (count morphology) " morphological rules."))
          (log/debug (str "loading lexical rules.."))
          (let [lexical-rules (-> (str "file://" menard-dir "resources/"
                                       (-> spec :lexicon :path) "/"
                                       (-> spec :lexicon :rules))
                                  l/read-and-eval)]
            (log/debug (str "loaded " (count lexical-rules) " lexical rules."))
            (log/info (str "create-model-from-filesystem: loading lexicon with spec: " spec))
            (let [lexicon (compile-lexicon
                           (model/load-lexicon lexical-rules
                                               spec
                                               (str "file://" menard-dir "resources/"
                                                    (-> spec :lexicon :path)))
                           morphology
                           (fn [x] x))]
              (log/debug (str "loaded " (count (keys lexicon)) " lexical keys."))
              (log/debug (str "done loading model."))
              (->
               (model/load "nl"
                           (fn [] lexical-rules)
                           (fn [_] lexicon)
                           (fn [] morphology)
                           (fn [] grammar)
                           spec)
               ((fn [model]
                  (merge model
                         {:spec spec
                          :lexicon-index-fn (model/lexicon-index-fn model)}))))))))))))

#?(:clj
   (defn load-model [model & [reload?]]
      (when (or (nil? @model) (true? reload?))
        (try
          (log/info (str (when @model "re") "loading model: " (:name @model)))
          (let [loaded (create-model-from-filesystem (:spec @model))]
            (dosync
             (ref-set model loaded))
            (log/info (str "model update done.")))
          (catch Exception e (do
                               (log/info (str "Failed to load model; the error was: '" (str e) "'. Will keep current model as-is and wait 10 seconds and see if it's fixed then."))))))
     (if (nil? @model)
       (log/error (str "load-model: model couldn't be loaded. Tried both built-in jar and filesystem.")))
     @model))

#?(:clj
   (defn start-reload-loop []
     (def last-file-check (atom 0))
     (go-loop []
       (let [nl-file-infos (get-info-of-files "../resources/nederlands" "**{.edn}")
             general-file-infos (get-info-of-files "../resources" "*{.edn}")
             most-recently-modified-info
             (->>
              (concat nl-file-infos
                      general-file-infos)
              (sort (fn [a b] (> (:last-modified-time-ms a) (:last-modified-time-ms b))))
              first)

             last-file-modification
             (:last-modified-time-ms most-recently-modified-info)

             model complete/model]
         (if (> last-file-modification @last-file-check)
           (log/info (str
                      "start-reload-loop: "
                      (:parent most-recently-modified-info) "/"
                      (:filename most-recently-modified-info)
                      " at: "
                      (:last-modified-time most-recently-modified-info))))
         (do (load-model model (> last-file-modification @last-file-check))
             (swap! last-file-check
                    (fn [_] (current-ms)))))
       (Thread/sleep 10000)
       (recur))))

#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon (:lexicon @complete/model)
                               "resources/nederlands/lexicon/compiled.edn")))

#?(:cljs
   ;; note that we exclude [:exception]s from the lexemes that we use for
   ;; generation since they are only to be used for parsing.
   (def lexeme-map
     {:verb (->> lexicon
                 (filter #(= :verb (u/get-in % [:cat])))
                 (filter #(not (u/get-in % [:exception]))))
      :det (->> lexicon
                (filter #(= :det (u/get-in % [:cat]))))
      :intensifier (->> lexicon
                        (filter #(= :intensifier (u/get-in % [:cat]))))
      :noun (->> lexicon
                 (filter #(= :noun (u/get-in % [:cat])))
                 (filter #(not (u/get-in % [:exception]))))
      :top lexicon
      :adjective (->> lexicon
                      (filter #(= :adjective (u/get-in % [:cat]))))}))

#?(:cljs
   (defn index-fn [spec]
     ;; for now a somewhat bad index function: simply returns
     ;; lexemes which match the spec's :cat, or, if the :cat isn't
     ;; defined, just return all the lexemes.
     (let [result (get lexeme-map (u/get-in spec [:cat] :top) nil)]
       (if (not (nil? result))
           (shuffle result)
           (do
             (log/warn (str "no entry from cat: " (u/get-in spec [:cat] ::none) " in lexeme-map: returning all lexemes."))
             lexicon)))))

(declare sentence-punctuation)

(defn morph
  ([tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (s/morph (u/get-in tree [:syntax-tree]) (:morphology @complete/model))

     :else
     (s/morph tree (:morphology @complete/model))))

  ([tree & {:keys [sentence-punctuation?]}]
   (when sentence-punctuation?
     (-> tree
         morph
         (sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

#?(:cljs
   (def grammar
     (->> (menard.grammar/read-compiled-grammar
           "resources/nederlands/grammar/compiled.edn")
          (map deserialize))))

#?(:clj
   (defn write-compiled-grammar []
     (grammar/write-compiled-grammar (-> @complete/model :grammar)
                                     "resources/nederlands/grammar/compiled.edn")))
(declare generate)
(declare syntax-tree)

(def expressions
  (->> (-> "nederlands/expressions.edn"
           grammar/read-expressions)))

;; <functions>

#?(:clj
   (defn syntax-tree [tree & [model]]
     (s/syntax-tree tree (:morphology (or model complete/model (load-model complete/model))))))

#?(:cljs
   (defn syntax-tree [tree]
     (s/syntax-tree tree [])))

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec & [model]]
  (let [model (or model complete/model (load-model complete/model))
        model (cond (= (type model) clojure.lang.Ref)
                    @model
                    (map? model)
                    model

                    :else
                    (exception (str "invalid model: " model)))]
    (if (:name model)
      (log/info (str "generating with model with :name: " (:name model)))
      (log/warn (str "generating with model with no name.")))
    (binding [g/max-depth (:max-depth spec g/max-depth)
              g/max-fails (:max-fails spec g/max-fails)
              g/allow-backtracking? true]
      (-> spec
          ((fn [x] (unify x (:training-wheels x :top))))
          (dissoc :training-wheels)
          (g/generate (-> model :grammar)
                      (-> model :lexicon-index-fn)
                      syntax-tree)))))

(defn generate-all
  "generate all expressions that satisfy _spec_."
  [spec]
  (let [model (load-model complete/model)]
    (binding [] ;;  g/stop-generation-at [:head :comp :head :comp]
      (g/generate-all [spec]
                      (-> model :grammar)
                      (-> model :lexicon-index-fn)
                      syntax-tree))))

(defn analyze [surface]
  (let [model (load-model complete/model)]
    (binding [l/lexicon (-> model :lexicon)
              p/syntax-tree syntax-tree
              l/morphology (:morphology model)]
      (let [variants (vec (set [(clojure.string/lower-case surface)
                                (clojure.string/upper-case surface)
                                (clojure.string/capitalize surface)]))
            found (mapcat l/matching-lexemes variants)]
        (log/debug (str "found: " (count found) " for: [" surface "]"))
        (if (seq found)
          found
          (let [found (l/matching-lexemes "_")]
            (log/info (str "no lexemes found for: [" surface "]"
                           (when (seq found)
                             (str "; will use null lexemes instead."))))
            found))))))

(defn parse [expression]
  (let [model (load-model complete/model)

        ;; remove trailing '.' if any:
        expression (string/replace expression #"[.]*$" "")]
        ;; ^ TODO: should handle '.' and other punctuation like '?' '!' and
        ;; use it as part of the meaning
        ;; i.e.
        ;; '.' -> declarative
        ;; '?' -> interrogative
        ;; '!' -> imperative
    (binding [l/lexicon (-> model :lexicon)
              l/morphology (-> model :morphology)
              p/grammar (-> model :grammar)
              p/syntax-tree syntax-tree
              p/morph morph
              p/truncate? truncate?
              p/split-on #"[ ]"
              p/log-these-rules log-these-rules
              p/lookup-fn analyze]
      (p/parse expression))))

(defn parse-start [expression]
  (let [model (load-model complete/model)

        ;; remove trailing '.' if any:
        expression (string/replace expression #"[.]*$" "")]
        ;; ^ TODO: should handle '.' and other punctuation like '?' '!' and
        ;; use it as part of the meaning
        ;; i.e.
        ;; '.' -> declarative
        ;; '?' -> interrogative
        ;; '!' -> imperative

    (binding [l/morphology (-> model :morphology)
              p/split-on #"[ ]"
              p/lookup-fn analyze]
      (p/parse-start expression))))

(defn generate-demo [index & [this-many]]
  (->>
   (repeatedly #(println (-> (nth expressions index)
                             generate
                             ((fn [x] (morph x :sentence-punctuation? true))))))
   (take (or this-many 10))
   count))

(defn demo []
  (->> (range 0 (count expressions))
       (map #(do
               (println (str % ": " (-> expressions (nth %) :note)))
               (generate-demo %)
               (println)))
       count))

(defn parse-demo [index & [this-many]]
  (->>
   (repeatedly #(println (-> (nth expressions index)
                             generate
                             morph
                             parse
                             time
                             first
                             ((fn [x] (syntax-tree x))))))
   (take (or this-many 10))
   count))

(defn demo-with-pruning [index & [this-many]]
  (binding [g/fold? true
            g/truncate? true]
    (->>
     (repeatedly #(println (-> (nth expressions index)
                               generate
                               time
                               ((fn [x] (morph x :sentence-punctuation? true))))))
     (take (or this-many 10))
     count)))

(defn tokenize [input-string]
  (binding [p/split-on #"[ ]"]
    (p/tokenize input-string)))

(defn sentence-punctuation
  "Capitalizes the first letter and puts a period (.) or question mark (?) at the end."
  [input mood]
  (str (string/capitalize (first input))
       (subs input 1 (count input))
       (if (= mood :interog)
         "?"
         ".")))

(defn roundtrip [input]
  (-> input
      parse
      first
      ((fn [input-expression]
         (let [spec {:sem (u/get-in input-expression [:sem] :top)
                     :mod (u/get-in input-expression [:mod] :top)
                     :phrasal? (u/get-in input-expression [:phrasal?] :top)
                     :subcat (-> input-expression (u/get-in [:subcat] []))
                     :cat (u/get-in input-expression [:cat] :top)}
               generated (-> spec generate)]
           {:input-expression (-> input-expression syntax-tree)
            :input-spec spec
            :readability-divider "--------------------------------"
            :generated-expression (-> generated syntax-tree)
            :generated (-> generated morph)
            :output-structure {:sem (-> generated (u/get-in [:sem] ::unspec))
                               :cat (-> generated (u/get-in [:cat] ::unspec))
                               :mod (-> generated (u/get-in [:mod] ::unspec))}})))
      u/pprint))

(defn sample [spec n]
  (->> #(-> spec generate morph)
       repeatedly (take n) set vec sort (map println)
       count))

;; example usage:
;;
;; (->> "slapen honden"
;;      parse
;;      (map (ugins [:infl] [:sem] [:head :sem][:subcat]))
;;      (map u/pprint))
;;
;; => parses with selected parts shown
;;
(defn ugin
  ([& paths]
   (fn [arg1]
     (reduce dag_unify.core/unify
             (map (fn [path]
                    (dag_unify.serialization/create-path-in path (u/get-in arg1 path)))
                  paths)))))

(def morphology (-> complete/model deref :morphology))

(defn parse-all [expression]
  (p/parse-all expression (fn [] (load-model complete/model)) syntax-tree analyze))
