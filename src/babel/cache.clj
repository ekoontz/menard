(ns babel.cache
  (:refer-clojure :exclude [get-in merge resolve find parents])
  (:require
   ;; TODO: comment is misleading in that we never call core/get-in from this file.
   ;; TODO: alphabetize
   [clojure.core :as core] ;; This allows us to use core's get-in by doing "(core/get-in ..)"
;;   [clojure.set :refer :all]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   ;; TODO: be more specific in :refer than :all.
   [dag_unify.core :refer :all :exclude [unify]]

   [babel.over :exclude [overc overh]]
   [babel.over :as over]))

;; For now, this cache is just a stub; no actual caching is done; it simply calls 
;; the over/ equivalents of each of the defined functions.

(def head-cache {})
(def comp-cache {})

(defn specs-to-subsets [lexicon-of-heads lexicon]
  (if (not (empty? lexicon-of-heads))
    (let [lexeme (first lexicon-of-heads)]
      (if (keyword? (show-spec (get-in lexeme '(:synsem :subcat :1 :cat))))
        (conj {(show-spec (get-in lexeme '(:synsem :subcat :1 :cat)))
               (filter (fn [each-lex]
                         (not (fail? (unifyc (get-in each-lex '(:synsem :cat))
                                             (get-in lexeme '(:synsem :subcat :1 :cat))))))
                       lexicon)}
              (specs-to-subsets (rest lexicon-of-heads)
                                lexicon))
        (specs-to-subsets (rest lexicon-of-heads)
                          lexicon)))
    {}))

(declare spec-to-phrases)

(defn build-lex-sch-cache [phrases lexicon all-phrases]
  "Build a mapping of phrases onto subsets of the lexicon. The two values (subsets of the lexicon) to be
   generated for each key (phrase) are: 
   1. the subset of the lexicon that can be the head of this phrase.
   2. the subset of the lexicon that can be the complement of this phrase.

   End result is a set of phrase => {:comp subset-of-lexicon 
                                     :head subset-of-lexicon}."
  (log/debug (str "build-lex-sch-cache: lexicon size: " (.size lexicon)))
  (log/debug (str "build-lex-sch-cache: grammar size: " (.size all-phrases)))
  (if (not (empty? phrases))
    (conj
     {(get-in (first phrases) [:rule])
      {:comp
       (filter (fn [lex]
                 (not (fail? (unifyc (first phrases)
                                     {:comp lex}))))
               lexicon)

       :comp-phrases
       (filter (fn [comp-phrase]
                 (not (fail? (unifyc (first phrases)
                                     {:comp comp-phrase}))))
               all-phrases)

       :head-phrases
       (filter (fn [head-phrase]
                 (not (fail? (unifyc (first phrases)
                                     {:head head-phrase}))))
               all-phrases)

       :head
       (filter (fn [lex]
                 (log/debug (str "trying lexeme: " lex))
                 (not (fail? (unifyc (first phrases)
                                     {:head lex}))))
               lexicon)}}
     (build-lex-sch-cache (rest phrases) lexicon all-phrases))
    {:lexical-subsets (specs-to-subsets lexicon lexicon)}))

(defn spec-to-phrases [specs all-phrases]
  (if (not (empty? specs))
    (let [spec (first specs)]
      (conj
       {spec 
        (filter #(not (fail? %))
                (map (fn [each-phrase]
                       (unifyc each-phrase spec))
                     ;; TODO: possibly: remove-paths such as (subcat) from head: would make it easier to call with lexemes:
                     ;; e.g. "generate a sentence whose head is the word 'mangiare'" (i.e. user passes the lexical entry as
                     ;; head param of (lightning-bolt)".
                     all-phrases))}
       (spec-to-phrases (rest specs) all-phrases)))
    {}))

;; TODO: spec is not used yet; add support for it.
(defn get-lex [schema head-or-comp cache spec]
  "return the subset of the whole lexicon that can be added either as a head (head-or-comp=:head) or as a comp (head-or-comp=:comp)."
  (if (nil? schema)
    #{}
    (do
      (log/debug (str "get-lex: " (get-in schema [:rule]) " ; " head-or-comp))
      (if (not (map? schema))
        (throw (Exception. (str "first arguments should have been a map, but instead was of type: " (type schema) "; schema: " schema))))
      (log/trace (str "get-lex schema: " (get-in schema [:rule]) " for: " head-or-comp))
      (if (nil? (get-in schema [:rule]))
        (throw (Exception. (str "no schema for: " schema))))
      (let [result (cond (= :head head-or-comp)
                         (if (and (= :head head-or-comp)
                                  (not (nil? (:head (get cache (get-in schema [:rule]))))))
                           (do
                             (log/trace (str "get-lex hit: head for schema: " (get-in schema [:rule])))
                             (:head (get cache (get-in schema [:rule]))))

                           (do (log/warn (str "CACHE MISS 1 for rule: " (get-in schema [:rule]) " h/c: " head-or-comp " :: cache:" (type cache)))
                               #{}))

                         (= :comp head-or-comp)
                         (if (and (= :comp head-or-comp)
                                  (not (nil? (:comp (get cache (get-in schema [:rule]))))))
                           (do
                             (log/trace (str "get-lex hit: comp for schema: " (get-in schema [:rule])))
                             (:comp (get cache (get-in schema [:rule]))))
                           (do
                             (log/warn (str "CACHE MISS 2"))
                             nil))
                       
                         true
                         (do (log/warn (str "CACHE MISS 3: head-or-comp:" head-or-comp))
                             nil))]
        (let [filtering false ;; no quantitative evidence that this helps, so turning off.
              result
              (if filtering
                (filter (fn [each]
                          (let [spec-val (get-in spec [:synsem :sem :pred] :none)]
                            (or (= :none spec-val)
                                (= spec-val (get-in each [:synsem :sem :pred])))))
                        result)
                result)]
          (lazy-shuffle result))))))
  
(defn get-parent-phrases-for-spec [cache spec]
  (log/trace (str "Looking up spec: " (show-spec spec)))
  (let [result (get (get cache :phrases-for-spec) (show-spec spec))
        result (if (nil? result) (list) result)]
    (if (empty? result)
      (log/trace (str "parent-phrases for spec: " (show-spec spec) " is empty.")))
    (lazy-shuffle result)))

(defn get-head-phrases-of [parent cache]
  (let [result (:head-phrases (get cache (get-in parent [:rule])))
        result (if (nil? result) (list) result)
        label (label-of parent)]
    (if (empty? result)
      (log/trace (str "headed-phrases of parent: " label " is empty.")))
    (lazy-shuffle result)))

(defn get-comp-phrases-of [parent cache]
  (let [result (:comp-phrases (get cache (get-in parent [:rule])))
        result (if (nil? result) (list) result)]
    (if (empty? result)
      (log/trace (str "comp-phrases of parent: " (label-of parent) " is empty.")))
    (lazy-shuffle result)))

(defn overc-with-cache-1 [parent lex]
  (if (not (empty? lex))
    (do
      (lazy-cat (over/overc parent (first lex))
                (overc-with-cache-1 parent (rest lex))))))

(defn get-subset-from-cache [cache use-spec]
  (let [debug (log/debug (str "looking for use-spec 1: " use-spec))
        use-spec (get-in use-spec '(:synsem :cat))
        debug (log/debug (str "looking for use-spec 2: " use-spec))
        ls-part (get cache :lexical-subsets :notfound)]
    (if (= :notfound ls-part)
      :notfound
      (get ls-part (show-spec use-spec) :notfound))))

;; TODO: document how this works and especially what 'phrase-constraint' means.
(defn create-index [grammar lexicon phrase-constraint]
  (let [lexicon (if (map? lexicon)
                  (keys lexicon)
                  lexicon)]
    (log/debug (str "create index with lexicon: " (.size lexicon)))
    (conj (build-lex-sch-cache grammar
                               (map (fn [lexeme]
                                      (log/debug (str "trying(ci) lexeme: " lexeme))
                                      (unifyc lexeme
                                              {:phrasal false}))
                                    lexicon)
                               grammar)
          {:phrase-constraints phrase-constraint
           :phrases-for-spec
           (spec-to-phrases
            ;; TODO: make this list derivable from the grammar and /or lexicon.
            (list {:synsem {}, :head {:synsem {}}, :phrasal true}
                  {:synsem {:cat :verb, :aux false}, :head {:synsem {:subcat {:2 {}, :1 {}}, :infl :present, :cat :verb, :sem {:tense :present}}, :phrasal false}, :phrasal true}
                  {:synsem {:cat :verb}, :head {:synsem {:cat :verb, :infl {:not :past}, :subcat {:2 {:cat :noun, :subcat (), :pronoun true}, :1 {}}}, :phrasal false}, :phrasal true}
                  {:synsem {:cat :verb, :aux false}, :head {:synsem {:cat :verb, :infl :infinitive, :subcat {:2 {}, :1 {}}}, :phrasal false}, :phrasal true}
                  )
            grammar)})))


