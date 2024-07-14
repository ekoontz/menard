(ns menard.english

  ;; TODO: don't we need to have a :require-macros
  ;; for menard.morphology, too?
  #?(:cljs (:require-macros [menard.grammar]))

  (:require [clojure.string :as string]

            ;; models
            [menard.english.complete :as complete]
            
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            [menard.generate :as g]
            [menard.grammar :as grammar]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [menard.model :as model]
            [menard.nesting]
            [menard.parse :as p]
            [menard.serialization :as s]
            [menard.subcat]
            [menard.ug]
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :as diag]
            [dag_unify.serialization :refer [deserialize]]))
;;
;; For generation and parsing of English.
;;

(declare an)
(declare sentence-punctuation)

(defn morph
  ([tree]
   (cond
     (map? (u/get-in tree [:syntax-tree]))
     (-> (u/get-in tree [:syntax-tree])
         (s/morph (:morphology @complete/model))
         an)

     :else
     (-> tree
         (s/morph (:morphology @complete/model))
         an)))

  ([tree & {:keys [sentence-punctuation?]}]
   (when sentence-punctuation?
     (-> tree
         morph
         an
         (sentence-punctuation (u/get-in tree [:sem :mood] :decl))))))

#?(:clj
   (defn write-compiled-lexicon []
     (l/write-compiled-lexicon (:lexicon @complete/model)
                               "resources/english/lexicon/compiled.edn")))

#?(:cljs
   (def lexicon
     (-> (l/read-compiled-lexicon "english/lexicon/compiled.edn")
         l/deserialize-lexicon              
         vals
         flatten)))

#?(:clj
   (defn index-fn [spec]
     ((-> complete/model deref :lexicon-index-fn) spec)))

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

(defn syntax-tree [tree]
  (s/syntax-tree tree (:morphology @complete/model)))

(defn an
  "change 'a' to 'an' if the next word starts with a vowel; 
   and change 'an' to 'a' if the next word does *not* start with a vowel."
  [input]
  (-> input
      (string/replace #"\b([aA]) ([aeiou])"  "$1n $2")
      (string/replace #"\b([aA])n ([^aeiou])" "$1 $2")))

(declare sentence-punctuation)
(declare analyze)
(def split-on #"[ ]+")

(defn tokenize [input-string]
  (p/tokenize input-string split-on analyze))

(defn sentence-punctuation
  "Capitalizes the first letter and puts a period (.) or question mark (?) at the end."
  [input mood]
  (str (string/capitalize (first input))
       (subs input 1 (count input))
       (if (= mood :interog)
         "?"
         ".")))

(declare wrapped-lexicon-index-fn)

(defn generate
  "generate one random expression that satisfies _spec_."
  [spec & [model]]
  ;; should block on this until a model exists: maybe @model should be a future
  ;; or a promise (not sure what the difference is).
  (log/debug (str "menard.english/generate with spec: " (u/pprint spec)))
  (let [model (or model complete/model)
        model (cond (= (type model) clojure.lang.Ref)
                    @model
                    (map? model)
                    model
                    
                    :else
                    (let [error-message (str "menard.english/generate: invalid model: " model)]
                      (log/error error-message)
                      (exception error-message)))]
    (binding [g/max-depth (if (get-in spec [:max-depth])
                            (+ 5 (get-in spec [:max-depth]))
                            (get-in spec [:max-depth] g/max-depth))]
      (log/debug (str "english generate: " (diag/strip-refs spec)
                      " with max-depth: " g/max-depth))
      (g/generate spec
                  (-> model :grammar)
                  (wrapped-lexicon-index-fn (-> model :lexicon-index-fn)) syntax-tree))))

(defn generate-n
  "generate _n_ consecutive in-order expressions that satisfy _spec_."
  [spec n]
  (take n (repeatedly #(generate spec))))

;; <TODO> move into declarative grammar (i.e. the model).
(def post-lexical-retrieval-rules
  [{:rule :unspecified-infl-to-present-and-base
    :if {:cat :verb
         :aux? false
         :modal :none
         :infl ::unspec}
    :then [{:infl :base}
           {:infl :present}]}

    {:rule :present-infl-to-present-tense
    :if {:cat :verb
         :aux? false
         :modal :none
         :infl :present}
    :then [{:infl :present
            :sem {:tense :present
                  :aspect :simple}}]}

   {:rule :tense-non-aux-future
    :if {:cat :verb
         :aux? false
         :modal :none
         :infl :present}
    :then [{:infl :present
            :sem {:tense :present
                  :aspect :simple}}]}

   {:rule :modal-infinitive
    :if {:cat :verb
         :aux? false
         :modal :infinitive
         :infl ::unspec}
    :then [{:infl :base}
           {:infl :present
            :sem {:tense :present
                  :aspect :simple}}]}

   {:rule :present-participle
    :if {:cat :verb
         :aux? true
         :modal :present-participle}
    :then [{:sem {:tense :present
                  :aspect :progressive}}]}
   
   {:rule :past-participle
    :if {:cat :verb
         :aux? true
         :modal :past-participle}
    :then [{:sem {:tense :past
                  :aspect :progressive}}]}

   {:rule :past-simple-non-aux
    :if {:cat :verb
         :aux? false
         :modal :none
         :infl :past-simple
         :sem {:tense ::unspec}}
    :then [{:sem {:tense :past
                  :aspect :simple}}]}])
;; </TODO>

(defn post-lexical-retrieval-rule
  "takes: a list of lexemes and a rule
   return: a list of lexemes.
  
   For each lexeme, if the rule is applicable (unifying the antecedent :if),
   then concatenate the result of applying the rule to the input
   lexeme. If the :if of the rule does not unify with the lexeme, then simply return
   a list containing only that lexeme."
  [lexemes rule]
  (let [{if :if
         then :then} rule]
    (->> lexemes
         (mapcat (fn [lexeme]
                   (if (not (= :fail (unify if lexeme)))
                     (->> then
                          (map (fn [each-then]
                                 (let [unify (unify each-then lexeme)]
                                   (log/debug (str "post-lexical-retrieval-rule matched: " each-then))
                                   (log/debug (str " lexeme: " (l/pprint lexeme)))
                                   (if (= :fail unify)
                                     (exception (str "lexeme matched post-lexical-retrieval-rule's :if but not its :then. Lexeme: " (l/pprint lexeme)
                                                     " rule: " rule ": fail-path: " (diag/fail-path lexeme each-then)))
                                     unify)))))
                     [lexeme]))))))

(defn post-lexical-retrieval [lexemes rules]
  (if (seq rules)
    (post-lexical-retrieval
     (post-lexical-retrieval-rule lexemes (first rules))
     (rest rules))
    lexemes))

(defn analyze [surface & [model]]
  (log/debug (str "analyze: " surface))
  (let [model (or model @complete/model)
        lexicon (-> model :lexicon)
        morphology (-> model :morphology)
        variants (vec (set [(clojure.string/lower-case surface)
                            (clojure.string/upper-case surface)
                            (clojure.string/capitalize surface)
                            surface]))]
    (->> variants
         (mapcat #(l/matching-lexemes % lexicon morphology))
         (mapcat (fn [lexeme]
                   (post-lexical-retrieval [lexeme] post-lexical-retrieval-rules))))))

(defn wrapped-lexicon-index-fn [lexical-index-fn]
  (fn [spec]
    (log/debug (str "wrapped-lexicon-index-fn: input spec: " spec))
    (let [result (lexical-index-fn spec)]
      (log/debug (str "wrapped-lexicon-index-fn found: " (count result) " lexemes matching spec: " (l/pprint spec)))
      (-> result
          (post-lexical-retrieval post-lexical-retrieval-rules)))))

(defn resolve-model [model]
  (cond (= (type model) clojure.lang.Ref) @model
        (map? model)                      model
        :else                             (exception (str "invalid model: " model))))

;; TODO: consider setting p/truncate? false here in (defn parse)
;; to improve performance:
(defn parse [expression & [model]]
  (let [model (resolve-model (or model complete/model))]
    (log/debug (str "menard.english/parse with model name: "
                    (-> model :name)))
    (let [truncate? true]
      (->
       expression
       (p/parse (-> model :grammar) #(analyze % model) syntax-tree morph split-on truncate?)))))

;;(defn parses [expression & [model]]
;;  (->
;;   (p/parse-in-stages 

(defn parse-start [expression & [model]]
  (let [model (or model complete/model)

        ;; remove trailing '.' if any:
        expression (string/replace expression #"[.]*$" "")
        ;; ^ TODO: should handle '.' and other punctuation like '?' '!' and
        ;; use it as part of the meaning
        ;; i.e.
        ;; '.' -> declarative
        ;; '?' -> interrogative
        ;; '!' -> imperative
        ]
    (p/parse-start expression split-on analyze)))

(def expressions
  (-> "english/expressions.edn"
      grammar/read-expressions))

(defn demo []
  (count
   (->>
    (range 0 (count expressions))
    (map (fn [index]
           (let [generated-expression (first (->> (take 3 (repeatedly #(generate (nth expressions index))))
                                                  (filter #(not (nil? %)))))]
             (println (morph generated-expression
                             :sentence-punctuation? true))
             (println (syntax-tree generated-expression))
             (println)))))))

(defn parse-all [expression]
  (p/parse-all expression (-> complete/model deref :grammar) syntax-tree #"[ ]" analyze))

(defn get-grammar [rule-name]
  (->> (-> complete/model deref :grammar)
       (filter #(= rule-name (:rule %)))))

(defn get-lexicon [lexeme]
  (get (-> complete/model deref :lexicon) lexeme))
