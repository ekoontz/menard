;; TODO: nouns do not need {:essere false}
(ns babel.italiano.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.lexicon :refer [universals]]

   ;; TODO: use dag_unify/unifyc instead:
   ;; deprecate lexiconfn/unify.
   [babel.lexiconfn :refer [compile-lex if-has if-then
                            constrain-vals-if
                            default
                            filter-vals listify
                            map-function-on-map-vals
                            new-entries rewrite-keys unify]]

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
   [dag_unify.core :refer [dissoc-paths fail? get-in strip-refs unifyc]]))

(declare edn2lexicon)

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

;; TODO: dangerous to (eval) code that we don't directly control:
;; replace with a DSL that accomplishes the same thing without
;; security problems.
(defn evaluate [lexicon]
  (into {}
        (for [[k v] lexicon]
          [k (eval v)])))

;; TODO rename without confusing "2"
(defn exception-generator2 [lexicon]
  (let [exception-maps (exception-generator lexicon)]
    (if (not (empty? exception-maps))
      (merge-with concat
                  lexicon
                  (reduce (fn [m1 m2]
                            (merge-with concat m1 m2))
                          (exception-generator lexicon)))
      lexicon)))

;; TODO: factor out Italian-specific parts and promote to babel.lexiconfn.
;; TODO: see if we can use Clojure transducers here. (http://clojure.org/reference/transducers)
(defn edn2lexicon [resource]
  (-> (read-string (slurp resource)) ;; read .edn file into a Clojure map.
      evaluate ;; evaluate all expressions within this map (e.g. grep for "(let") in the .edn file.
      listify ;; if any value of the map is not a sequence, make it a sequence with one element: the original value.
      ;; end language-independent operations.

      ;; begin language-dependent operations.
      apply-unify-key ;; turn any :unify [..] key-value pairs with (reduce unify (:unify values)).
      ;; the operation of apply-unify-key is language-independent, but
      ;; the values of :unify may be symbols that refer to language-dependent values.

      exception-generator2 ;; add new keys to the map for all exceptions found.

      (default ;; a common noun takes a determiner as its only argument.
       {:synsem {:cat :noun
                 :pronoun false
                 :propernoun false
                 :subcat {:1 {:cat :det}
                          :2 '()}}})

      (default ;; how a determiner modifies its head noun's semantics.
       (let [def (atom :top)]
         {:synsem {:cat :noun
                   :pronoun false
                   :propernoun false
                   :sem {:spec {:def def}}
                   :subcat {:1 {:def def}}}}))
      
      (default;; a pronoun takes no args.
       {:synsem {:cat :noun
                 :pronoun true
                 :propernoun false
                 :subcat '()}})

      (default ;; a propernoun takes no args.
       {:synsem {:cat :noun
                 :pronoun false
                 :propernoun true
                 :subcat '()}})

      ;; reflexive pronouns: set sharing within :italiano so that morphology can work as expected.
      (default
       (let [cat (atom :noun)
             case (atom :acc)]
         {:synsem {:cat cat
                   :pronoun true
                   :reflexive true
                   :subcat '()
                   :case case}
          :italiano {:cat cat
                     :case case}}))
            
      (default ;; a verb's first argument's case is nominative.
       {:synsem {:cat :verb
                 :subcat {:1 {:cat :noun
                              :case :nom}}}})

      (default ;; a verb's second argument's case is accusative.
       {:synsem {:cat :verb
                 :subcat {:2 {:cat :noun
                              :case :acc}}}})

      (default ;;  a verb's first argument defaults to the semantic subject of the verb.
       (let [subject-semantics (atom :top)]
         {:synsem {:cat :verb
                   :subcat {:1 {:sem subject-semantics}}
                   :sem {:subj subject-semantics}}}))

      (new-entries ;; remove the second argument and semantic object to make verbs intransitive.
       {:synsem {:cat :verb
                 :aux false
                 :sem {:obj {:top :top}
                       :reflexive false}
                 :subcat {:2 {:cat :noun}
                          :3 '()}}}
       (fn [lexeme]
         (dissoc-paths lexeme [[:synsem :sem :obj]
                               [:synsem :subcat :2]])))
      
      (default ;; a verb defaults to intransitive.
       {:synsem {:cat :verb
                 :subcat {:1 {:top :top}
                          :2 '()}}})

      (default ;;  a verb's second argument defaults to the semantic object of the verb.
       (let [object-semantics (atom :top)]
         {:synsem {:cat :verb
                   :subcat {:2 {:sem object-semantics}}
                   :sem {:obj object-semantics}}}))

      (default ;; a verb defaults to transitive if not intransitive.
       {:synsem {:cat :verb
                 :subcat {:1 {:top :top}
                          :2 {:top :top}
                          :3 '()}}})
      
      (default ;;  a preposition's first argument defaults to the semantic object of preposition.
       (let [object-semantics (atom :top)]
         {:synsem {:cat :prep
                   :subcat {:1 {:cat :noun
                                :sem object-semantics}}
                   :sem {:obj object-semantics}}}))
      
      (default ;; a verb agrees with its first argument
       (let [subject-agreement (atom :top)]
         {:synsem {:cat :verb
                   :subcat {:1 {:agr subject-agreement}}
                   :agr subject-agreement}}))

      (default ;; morphology looks in :italiano, so share relevant grammatical pieces of info there so it can see them.
       (let [agr (atom :top)
             cat (atom :verb)
             infl (atom :top)]
         {:italiano {:agr agr
                     :cat cat
                     :infl infl}
          :synsem {:agr agr
                   :cat cat
                   :infl infl}}))


      (default ;; aux defaults to false.
       {:synsem {:cat :verb
                 :aux false}})

      (default ;; essere must be shared within :italiano, because that is all morphology can see.
       (let [essere (atom :top)]
         {:synsem {:cat :verb
                   :essere essere}
          :italiano {:essere essere}}))
      
      (default ;; essere defaults to false.
       {:synsem {:cat :verb
                 :essere false}})

      (default ;; reflexive defaults to false.
       {:synsem {:cat :verb
                 :sem {:reflexive false}}})

      (default ;; noun agreement
       (unify {:synsem {:cat :noun
                        :pronoun false
                        :propernoun false
                        :subcat {:1 {:cat :det}
                                 :2 '()}}}
              (let [agr (atom :top)
                    cat (atom :top)]
                {:italiano {:agr agr
                            :cat cat}
                 :synsem {:cat cat
                          :agr agr
                          :subcat {:1 {:agr agr}}}})))

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
                     (= false (get-in % [:synsem :propernoun] false))
                     (= false (get-in % [:synsem :pronoun] false))))
           (and (log/warn (str "ignoring lexical entry with :cat=:noun but no gender specified: " %))
                false)))
     
     ;; filter out entries with no :cat.
     (filter-vals
      #(or (and (not (= :none (get-in % [:synsem :cat] :none)))
                (or (log/debug (str "lexical entry has a cat - good : " (strip-refs %)))
                    true))
           (and (log/warn (str "ignoring lexical entry with no :cat: " (strip-refs %)))
                false)))

     ;; end of language-specific grammar rules

     ;; begin world-knowledge constraints
     (constrain-vals-if
      (fn [val]
        (not (nil? (get universals (get-in val [:synsem :sem :pred])))))
      (fn [val]
        (get universals (get-in val [:synsem :sem :pred]))))

     ;; TODO: consider doing encyclopedia constraints
     ))

(def lexicon (edn2lexicon (clojure.java.io/resource "babel/italiano/lexicon.edn")))
