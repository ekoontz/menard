(ns babel.italiano
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.italiano.grammar :as grammar]
   [babel.italiano.lexicon :as lex]
   [babel.italiano.morphology :as morph :refer [get-string patterns]]
   [babel.generate :as generate]
   [babel.over :as over]
   [babel.parse :as parse]
   [babel.test.test :refer [init-db]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [clojure.core.cache :as cache]
   [clojure.pprint :refer [pprint]]
   [clojure.repl :refer [doc]]
   [clojure.string :as string]
   [dag_unify.core :refer [fail-path-between get-in strip-refs unify unifyc]]))

(def model
  (do
    (init-db)
    @@(get babel.directory/models :it)))

(defn apply-patterns [input-string]
  (let [result 
        (reduce (fn [str [from to]] (string/replace str from to))
                (cons input-string patterns))]
    (if (= result input-string)
      result
      (apply-patterns result))))

(defn morph [expr & {:keys [from-language show-notes]
                     :or {from-language nil
                          show-notes true}}]
  ;; modeled after babel.english/morph:
  ;; most arguments are simply discarded for italian.
  ;; TODO: rules should apply repeatedly until no change.
  (->
   (get-string (get-in expr [:italiano]))
   (apply-patterns)
   (string/trim)))
    
(defn morph-ps
  ([expr]
   (morph-ps expr model))

  ([expr model & {:keys [from-language show-notes]
                  :or {from-language nil
                       show-notes true}}]
   ;; modeled after babel.english/morph:
   ;; most arguments are simply discarded for italian.
   (parse/fo-ps expr (:morph model))))

(defn fo-ps [expr]
  (parse/fo-ps expr #(get-string (get-in % [:italiano]))))

(defn analyze
  "analyze a word: as opposed to parsing which is multi-word."
  ;; TODO: should take a language model, not a lexicon
  ([surface-form]
   (analyze surface-form model))
  ([surface-form model]
   (morph/analyze surface-form (:lexicon model))))

(defn generate
  ([spec]
   (generate spec model))
  ([spec model & {:keys [do-enrich truncate]
            :or {do-enrich true
                 truncate true}}]
   (log/debug (str "generating with spec: " (strip-refs spec)))
   (let [spec (unify spec {:modified false})
         spec (let [result (grammar/generation-implications spec model)]
                (cond (= :fail result)
                      (do (log/warn (str "spec failed when generation-implications applied:" spec ";"
                                         "using original-spec."))
                          spec)
                      true result))
         result (generate/generate spec model)]
     (if result
       (conj {:surface (get-string (get-in result [:italiano]))}
             result)))))

(defn an-example []
  (let [med (babel.italiano.grammar/model)
        med-reload (babel.italiano.grammar/model-reloaded)]
    (do
      (take 5
            (repeatedly 
             #(println 
               (morph (time (generate {:synsem {:cat :verb
                                                :subcat []
                                              :sem {:pred :know-s}}}
                                      med-reload))))))
      (take 5
            (repeatedly 
             #(println 
               (morph (time (generate {:synsem {:cat :verb
                                                :subcat []
                                                :sem {:pred :know-s}}}
                                      med)))))))))

(defonce tokenizer #"[ '\n,’».]")

(defn tokenize [input]
  (string/split input tokenizer))

(defn analyze-tokens
  "given a string, generate a list of tokenization hypotheses."
  [string]
  (map #(string/split % tokenizer)
       (morph/replace-over [string])))

(defn over
  "given a parent and 2 children, try to arrange them with the first child on the left and the second child on the right."
  [parent child1 child2]
  (over/over parent child1 child2))

(defn preprocess [input]
  "arbitrary regexp replacements to convert Italian orthography into a parsable whitespace-delimited expression"
  (let [processed
        (string/join
         " "
         (map string/lower-case
              (->
               input
               (string/replace #","   "")
               (string/replace #"\."   "")
               (string/replace #"\s+" " ")
               (string/split #" "))))]
    (log/debug (str "preprocess: " input " -> " processed))
    processed))

(defn parse
  "parse a string in Italian into zero or more (hopefully more) phrase structure trees"
  ([input]
   (let [model @@(get babel.directory/models :it)]
     (parse input model)))
  ([input model]
   (let [input (preprocess input)]
     (cond (string? input)
           (map (fn [tokenization]
                  {:tokens tokenization
                   :input input
                   :parses (parse tokenization model input)})
                (analyze-tokens (string/trim input)))

           (or (seq? input) (vector? input))
           (parse/parse input model)
        
           true
           (str "don't know how to parse input: " (type input)))))

  ([input model original-input]
   (let [input (if (string? input)
                 (preprocess input)
                 input)]
     (cond (string? input)
           (map (fn [tokenization]
                  {:tokens tokenization
                   :parses (parse tokenization model input)})
                (analyze-tokens (string/trim input)))

           (or (seq? input) (vector? input))
           (parse/parse input model :original-input original-input)
        
           true
           (str "don't know how to parse input: " (type input))))))

(defn create-model [ & words]
  (let [base-model (babel.italiano.grammar/model)
        base-lexicon
        (->
         ;; filter the whole lexicon down to a minimal set of 'must-haves':
         ;; (this filtered set might be as small as having no members at all).
         (:lexicon base-model)
         (babel.lexiconfn/only-nonempty-vals
          #(or (and false (= :det (dag_unify.core/get-in % [:synsem :cat]))
                    (= :def (dag_unify.core/get-in % [:synsem :def])))
               (and false (= :det (dag_unify.core/get-in % [:synsem :cat]))
                    (= :indef (dag_unify.core/get-in % [:synsem :def])))
               (and false (= true (dag_unify.core/get-in % [:synsem :pronoun]))
                    (= :nom (dag_unify.core/get-in % [:synsem :case]))))))]
    (->
     {:input-words (vec words)}
     (merge
      (let [model (babel.italiano.grammar/model-plus-lexicon
                   (merge
                    base-lexicon
                    (zipmap
                     (sort words)
                     (map (fn [word]
                            (get (:lexicon base-model) word))
                          (sort words)))))]
        model)))))

(defn test-cm []
  (let [m (create-model "io" "lei" "lo"
                        "loro" "lui" "mi" "tu" "ti" "vedere")]
    (repeatedly
     #(->
       {:synsem {:cat :verb
                 :infl :present
                 :sem {:aspect :simple}
                 :subcat []}
        :head {:phrasal true}}
       (generate m)
       morph
       println
       time))))


(defn from-repl []
  (load "babel/test/it")
  (load "babel/italiano")
  (in-ns 'babel.italiano)
  (test-cm))
