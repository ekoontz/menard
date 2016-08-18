;; TODO: nouns do not need {:essere false}
(ns babel.italiano.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.lexicon :refer [universals]]

   ;; TODO: use dag_unify/unifyc instead:
   ;; deprecate lexiconfn/unify.
   [babel.lexiconfn :refer [compile-lex if-has if-then constrain-vals-if
                            filter-vals listify
                            map-function-on-map-vals
                            rewrite-keys unify]]

   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [babel.italiano.morphology :refer [agreement aux-verb-rule essere-default exception-generator
                                      italian-specific-rules phonize2]]
   ;; These symbols from b.i.p. are all needed by the "eval" step in the lexical compilation pipeline
   [babel.italiano.pos :refer [adjective
                               agreement-noun
                               agreement-of-obj-of-main-verb
                               agreement-of-subj-of-main-verb
                               cat-of-pronoun
                               common-noun
                               comparative
                               countable-noun
                               determiner
                               essere-aux-subject-agreement
                               gender-and-number-agreement-1
                               intransitive
                               intransitive-unspecified-obj
                               intransitivize
                               masculine-noun
                               non-comparative-adjective
                               pred-is-obj-pred
                               preposition
                               pronoun-acc
                               pronoun-reflexive
                               reflexive
                               reflexive-indirect-obj-is-subcat3
                               sentential-adverb
                               subj-obj-humanness
                               transitive
                               transitivize
                               verb-aux
                               verb-subjective]]
   [clojure.edn :as edn]
   [clojure.java.io :refer [reader]]
   [clojure.repl :refer [doc]]
   [dag_unify.core :refer [fail? get-in strip-refs]]))

(def analyze-lexemes false)

;; TODO: promote to babel.lexiconfn
;; also consider renaming babel.lexiconfn to babel.lexicon.
(defn apply-unify-key [lexicon]
  (into {}
        (for [[k vals] lexicon]
          [k
           (map (fn [v]
                  (cond
                    (map? v)
                    (reduce unify
                            (cons (dissoc v :unify)
                                  (map (fn [each-unify-arg]
                                         (cond (fn? each-unify-arg)
                                               (each-unify-arg)
                                               true each-unify-arg))
                                       (:unify v))))
                    true v))
                vals)])))

(defn evaluate [lexicon]
  (into {}
        (for [[k v] lexicon]
          [k (eval v)])))

(defn exception-generator2 [lexicon]
  (let [exceptions 
        (reduce #(merge-with concat %1 %2)
                (exception-generator lexicon))]
    (merge-with concat lexicon exceptions)))

(defn agreement2 [lexicon]
  (into {}
        (for [[k vals] lexicon]
          [k
           (->> vals
                (map agreement)
                (map essere-default)
                (map aux-verb-rule))])))

(defn infer-cat [lexicon]
  (-> lexicon
      (if-has
       [:synsem :propernoun] true
       {:synsem {:cat :noun}})
      (if-has
       [:synsem :propernoun] true
       {:synsem {:cat :noun}})))

(defn infer-subcat [lexicon]
  (-> lexicon
      (if-has
       [:synsem :pronoun] true
       {:synsem {:subcat '()}})
      (if-has
       [:synsem :propernoun] true
       {:synsem {:subcat '()}})))

;; TODO: factor out Italian-specific parts and promote to babel.lexiconfn.
;; TODO: see if we can use Clojure transducers here. (http://clojure.org/reference/transducers)
(defn edn2lexicon [resource]
  (-> (read-string (slurp resource)) ;; read .edn file into a Clojure map.
      evaluate ;; evaluate all expressions within this map (e.g. grep for "(let") in the .edn file.
      listify ;; if any value of the map is not a sequence, make it a sequence with one element: the original value.
      ;; end language-independent operations.

      ;; begin language-dependent operations.
      apply-unify-key ;; turn any :unify [..] key-value pairs with (reduce unify (:unify values)).
      ;; the values of :unify may be symbols that refer to language-dependent values.

      exception-generator2 ;; add new keys to the map for all exceptions found.

      infer-cat ;; infer [:synsem :cat] for words for which it is not supplied in the source lexicon.
      infer-subcat ;; infer [:synsem :subcat :1]
      
      agreement2 ;; apply Italian agreement rules (e.g. subject and verb).

      phonize2 ;; for each value v of each key k, set the [:italiano :italiano] of v to k, if not already set
      ;; e.g. by exception-generator2.

      
      ;; TODO: throw error or warning in certain cases:
      ;; (= true (fail? value))
      ;;

     ;; common nouns need a gender (but propernouns do not need one).
     ;; TODO: throw error rather than just throwing out entry.
     (filter-vals
      #(or (not (and (= :noun (get-in % [:synsem :cat]))
                     (= :none (get-in % [:synsem :agr :gender] :none))
                     (= false (get-in % [:synsem :propernoun] false))))
           (and (log/warn (str "ignoring lexical entry with :cat=:noun but no gender specified: " %))
                false)))
     
     ;; filter out entries with no :cat.
     (filter-vals
      #(or (and (not (= :none (get-in % [:synsem :cat] :none)))
                (or (log/debug (str "lexical entry has a cat - good : " (strip-refs %)))
                    true))
           (and (log/warn (str "ignoring lexical entry with no :cat: " (strip-refs %)))
                false)))

     (constrain-vals-if
      (fn [val]
        (not (nil? (get universals (get-in val [:synsem :sem :pred])))))
      (fn [val]
        (get universals (get-in val [:synsem :sem :pred]))))

     ;; TODO: refactor this; it's very monolithic currently:
     intransitivize

     ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
     ;; TODO: refactor this; it's very monolithic currently:
     transitivize

     (constrain-vals-if
      (fn [val]
        (and (= :noun (get-in val [:synsem :cat]))
             (= true (get-in val [:synsem :reflexive]))
             (= true (get-in val [:synsem :pronoun]))))
      (fn [val]
        (unify (let [case (atom :acc)
                     cat (atom :noun)]
                 {:synsem {:cat cat
                           :pronoun true
                           :subcat '()
                           :reflexive true
                           :case case}
                  :italiano {:cat cat
                             :case case}}))))
     (constrain-vals-if
      (fn [val]
        (and (= (get-in val [:synsem :cat]) :noun)
             (or (= (get-in val [:synsem :agr :gender]) :masc)
                 (= (get-in val [:synsem :agr :gender]) :fem))
             (= false (get-in val [:synsem :propernoun] false))
             (= false (get-in val [:synsem :pronoun] false))
             (not (= '() (get-in val [:synsem :subcat] :top)))))
      ;; some nouns e.g. "casa" may have a sense that requires no determiner (subcat = '())
      ;; in such cases, don't apply agreement-noun.
      (fn [val]
        (unify val agreement-noun)))

     ))



     




