(ns babel.index
  (:refer-clojure :exclude [get-in resolve find parents])
  (:require
   ;; TODO: comment is misleading in that we never call core/get-in from this file.
   ;; TODO: alphabetize
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer [fail? dissoc-paths get-in label-of
                           remove-top-values-log
                           unifyc]]

   [babel.over :as over]))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. error-string)))
  #?(:cljs
     (throw (js/Error. error-string))))

;; For now, this index is just a stub; no actual caching is done; it simply calls 
;; the over/ equivalents of each of the defined functions.

(def head-index {})
(def comp-index {})
(declare show-spec)

(declare spec-to-phrases)

;; TODO: diagnostic function that is too specific currently (e.g. refers to ':english').
(defn check-index [index]
  (if (not (= :top (get-in (first (:head (get index "nbar"))) [:english :agr :number])))
    (throw (exception (str "CHECK INDEX FAILED! " (get index "nbar"))))))
  
(defn build-lex-sch-index [phrases lexicon all-phrases]
  "Build a mapping of phrases onto subsets of the lexicon. The two values (subsets of the lexicon) to be
   generated for each key (phrase) are: 
   1. the subset of the lexicon that can be the head of this phrase.
   2. the subset of the lexicon that can be the complement of this phrase.

   End result is a set of phrase => {:comp subset-of-lexicon 
                                     :head subset-of-lexicon}."
  (log/debug (str "build-lex-sch-index: lexicon size: " (count lexicon)))
  (log/debug (str "build-lex-sch-index: grammar size: " (count all-phrases)))
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
     (build-lex-sch-index (rest phrases) lexicon all-phrases))))

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

(defn get-lex [parent head-or-comp index]
  "return the subset of the whole lexicon that can be added to _parent_ either as a head (head-or-comp=:head) or as a comp (head-or-comp=:comp)."
  (log/debug (str "get-lex:" parent))
  (if (nil? parent)
    #{}
    (do
      (log/debug (str "get-lex: " (get-in parent [:rule]) " ; head-or-comp:" head-or-comp))
      (if (not (map? parent))
        (throw (exception (str "first arguments should have been a map, but instead was of type: " (type parent) "; parent: " parent))))
      (log/trace (str "get-lex parent: " (get-in parent [:rule]) " for: " head-or-comp))
      (if (nil? (get-in parent [:rule]))
        (throw (exception (str "no parent for: " parent))))
      (let [result (cond (= :head head-or-comp)
                         (if (and (= :head head-or-comp)
                                  (not (nil? (:head (get index (get-in parent [:rule]))))))
                           (do
                             (log/trace (str "get-lex hit: head for parent: " (get-in parent [:rule])))
                             (:head (get index (get-in parent [:rule]))))

                           (do (log/warn (str "INDEX MISS 1 for rule: " (get-in parent [:rule]) " h/c: " head-or-comp " :: index:" (type index)))
                               #{}))

                         (= :comp head-or-comp)
                         (if (and (= :comp head-or-comp)
                                  (not (nil? (:comp (get index (get-in parent [:rule]))))))
                           (do
                             (log/trace (str "get-lex hit: comp for parent: " (get-in parent [:rule])))
                             (:comp (get index (get-in parent [:rule]))))
                           (do
                             (log/warn (str "INDEX MISS 2"))
                             nil))
                       
                         true
                         (do (log/warn (str "INDEX MISS 3: head-or-comp:" head-or-comp))
                             nil))]
        result))))
  
(defn get-parent-phrases-for-spec [index spec]
  (log/trace (str "Looking up spec: " (show-spec spec)))
  (let [result (get (get index :phrases-for-spec) (show-spec spec))
        result (if (nil? result) (list) result)]
    (if (empty? result)
      (log/trace (str "parent-phrases for spec: " (show-spec spec) " is empty.")))
    result))

(defn get-head-phrases-of [parent index]
  (if (= true (get-in parent [:head :phrasal] :true))
    (let [result (:head-phrases (get index (get-in parent [:rule])))
          result (if (nil? result) (list) result)
          label (label-of parent)]
      (if (empty? result)
        (log/warn (str "headed-phrases of parent: " label " is empty: " (get-in parent [:head]))))
      result)))

(defn get-comp-phrases-of [parent index]
  (let [result (:comp-phrases (get index (get-in parent [:rule])))
        result (if (nil? result) (list) result)]
    (if (empty? result)
      (log/trace (str "comp-phrases of parent: " (label-of parent) " is empty.")))
    result))

(defn overc-with-index-1 [parent lex]
  (if (not (empty? lex))
    (do
      (lazy-cat (over/overc parent (first lex))
                (overc-with-index-1 parent (rest lex))))))

;; TODO: document how this works and especially what 'phrase-constraint' means.
(defn create-index [grammar lexicon phrase-constraint]
  (let [lexicon (if (map? lexicon)
                  (keys lexicon)
                  lexicon)]
    (log/info (str "create index with lexicon with size: " (count lexicon)))
    (conj (build-lex-sch-index grammar
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


(defn show-spec [spec]
  (cond (seq? spec)
        (map show-spec spec)
        true
        (remove-top-values-log (dissoc-paths spec '((:english :initial)
                                                    (:italiano :initial)
                                                    (:synsem :essere)
                                                    (:synsem :agr)
                                                    (:synsem :pronoun)
                                                    (:synsem :sem :tense)
                                                    (:synsem :sem :obj :tense)
                                                    (:synsem :sem :mod)
                                                    (:synsem :infl))))))



(defn map-subset-by-pred [preds
                          lexemes]
  (if (and true (not (empty? preds)))
    (let [pred (first preds)]
      (merge {pred
              (filter (fn [lexeme]
                        (or (= :top (get-in lexeme [:synsem :sem :pred] :top))
                            (= pred
                               (get-in lexeme [:synsem :sem :pred]))))
                      lexemes)}
             (map-subset-by-pred (rest preds)
                                 lexemes)))))

(defn map-subset-by-cat [cats lexemes]
  (if (and true (not (empty? cats)))
    (let [cat (first cats)]
      (merge {cat
              (filter (fn [lexeme]
                        (or (= :top (get-in lexeme [:synsem :cat] :top))
                            (= cat
                               (get-in lexeme [:synsem :cat]))))
                      lexemes)}
             (map-subset-by-cat (rest cats)
                                lexemes)))))
