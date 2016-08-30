;; TODO: nouns do not need {:essere false}
(ns babel.italiano.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.lexicon :refer [universals]]
   [babel.encyclopedia :as encyc]
   [babel.lexiconfn :refer [compile-lex if-has if-then default
                            filter-vals listify map-function-on-map-vals
                            new-entries rewrite-keys verb-pred-defaults
                            ;; TODO: use dag_unify/unifyc instead:
                            ;; deprecate lexiconfn/unify.
                            unify]]

   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [babel.italiano.morphology :refer [agreement essere-default exception-generator
                                      phonize2]]

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

      (default ;; all lexemes are phrasal=false by default.
       {:phrasal false})

      ;; end language-independent operations.

      ;; begin language-dependent operations.
      apply-unify-key ;; turn any :unify [..] key-value pairs with (reduce unify (:unify values)).
      ;; the operation of apply-unify-key is language-independent, but
      ;; the values of :unify may be symbols that refer to language-dependent values.

      exception-generator2 ;; add new keys to the map for all exceptions found.

      ;; <noun default rules>
      (default ;; a noun by default is neither a pronoun nor a propernoun.
       {:synsem {:cat :noun
                 :pronoun false
                 :propernoun false}})

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
      
      (default ;; a pronoun takes no args.
       {:synsem {:cat :noun
                 :pronoun true
                 :propernoun false
                 :subcat '()}})

      (default ;; a propernoun takes no args.
       {:synsem {:cat :noun
                 :pronoun false
                 :propernoun true
                 :subcat '()}})

      (default ;; a propernoun is agr=3rd singular
       {:synsem {:cat :noun
                 :pronoun false
                 :propernoun true
                 :agr {:number :sing
                       :person :3rd}}})

      (default  ;; reflexive pronouns are case=acc
       {:synsem {:case :acc
                 :cat :noun
                 :pronoun true
                 :reflexive true}})
                 
      (default
       ;; pronoun case and subcat: set sharing within :italiano so
       ;; that morphology can work as expected.
       (let [cat (atom :noun)
             case (atom :top)]
         {:synsem {:case case
                   :cat cat
                   :pronoun true
                   :subcat '()}
          :italiano {:cat cat
                     :case case}}))
      
      (default ;; determiner-noun agreement
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
      ;; </noun default rules>            

      ;; <verb default rules>
      (default ;; aux defaults to false..
       {:synsem {:cat :verb
                 :aux false}})

      (default ;; ..but if aux is true:
       (let [;; whether a verb has essere or avere as its
             ;; auxiliary to form its past form:
             essere-binary-categorization (atom :top)
             pred (atom :top)
             sem (atom {:tense :past
                        :pred pred})
             subject (atom :top)]
         {:synsem {:aux true
                   :cat :verb
                   :essere essere-binary-categorization
                   :sem sem
                   :subcat {:1 subject
                            :2 {:cat :verb
                                :essere essere-binary-categorization
                                :aux false
                                :infl :past
                                :subcat {:1 subject}
                                :sem sem}}}}))

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

      (verb-pred-defaults encyc/verb-pred-defaults)

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

      (default ;; reflexive defaults to false..
       {:synsem {:cat :verb
                 :aux false
                 :sem {:reflexive false}}})

      (default ;; ..but if a verb *is* reflexive:
       (let [subject-semantics (atom {:animate true})
             subject-agr (atom :top)]
         {:synsem {:cat :verb
                   :essere true
                   :sem {:subj subject-semantics
                         :obj subject-semantics
                         :reflexive true}
                   :subcat {:1 {:agr subject-agr
                                :sem subject-semantics}
                            :2 {:agr subject-agr
                                :pronoun true
                                :reflexive true
                                :sem subject-semantics}}}}))
      
      (default ;; a verb defaults to intransitive.
       {:synsem {:cat :verb
                 :subcat {:1 {:top :top}
                          :2 '()}}})
      
      (default ;; intransitive verbs' :obj is :unspec.
       {:synsem {:cat :verb
                 :subcat {:1 {:top :top}
                          :2 '()}
                 :sem {:obj :unspec}}})

      (default ;;  a verb's second argument (if there is one)
       ;; defaults to the semantic object of the verb.
       (let [object-semantics (atom :top)]
         {:synsem {:cat :verb
                   :subcat {:2 {:sem object-semantics}}
                   :sem {:obj object-semantics}}}))

      (default ;; a verb defaults to transitive if not intransitive..
       {:synsem {:cat :verb
                 :subcat {:1 {:top :top}
                          :2 {:top :top}
                          :3 '()}}})

      (default ;;  but if there *is* a third argument, it defaults
       ;; to the semantic indirect object of the verb.
       (let [indirect-object-semantics (atom :top)]
         {:synsem {:cat :verb
                   :subcat {:3 {:sem indirect-object-semantics}}
                   :sem {:iobj indirect-object-semantics}}}))
            
      (default ;; a verb agrees with its first argument
       (let [subject-agreement (atom :top)]
         {:synsem {:cat :verb
                   :subcat {:1 {:agr subject-agreement}}
                   :agr subject-agreement}}))

      (default ;; essere defaults to false.
       {:synsem {:cat :verb
                 :essere false}})
      
      ;; </verb default rules>

      ;; <preposition default rules>
      (default ;;  a preposition's semantic object defaults to its first argument.
       (let [object-semantics (atom :top)]
         {:synsem {:cat :prep
                   :subcat {:1 {:cat :noun
                                :sem object-semantics}}
                   :sem {:obj object-semantics}}}))
      ;; </preposition default rules>
      
      ;; <adjective default rules>
      (default ;; an adjective is comparative=false by default..
       (let [subject (atom :top)]
         {:synsem {:cat :adjective
                   :sem {:arg1 subject
                             :comparative false}
                   :subcat {:1 {:sem subject}
                            :2 '()}}}))
      ;; ..but, if comparative:
      (default
       (let [complement-sem (atom :top)
             subject-sem (atom :top)]
         {:synsem {:sem {:comparative true
                         :arg1 subject-sem
                         :arg2 complement-sem}
                   :cat :adjective
                   :subcat {:1 {:cat :noun
                                :sem subject-sem}
                            :2 {:cat :prep
                                :sem {:pred :di ;; Italian name for pred, for now: TODO: change to English :than.
                                      :obj complement-sem}
                                :subcat {:1 {:sem complement-sem}
                                         :2 '()}}}}}))
      ;; </adjective default rules>

      ;; <adverb default rules>
      (default
       (let [sentential-sem (atom :top)]
         {:synsem {:cat :sent-modifier
                   :sem {:subj sentential-sem}
                   :subcat {:1 {:sem sentential-sem}}}}))
      ;; </adverb default rules>

      ;; <determiner default rules>
      (default
       (let [def (atom :top)]
         {:synsem {:cat :det
                   :def def
                   :sem {:def def}}}))
                   
      ;; <category-independent> 
      (default ;; morphology looks in :italiano, so share relevant grammatical pieces of
       ;; info (:agr, :cat, :infl, and :essere) there so it can see them.
       (let [agr (atom :top)
             cat (atom :verb)
             essere (atom :top)
             infl (atom :top)]
         {:italiano {:agr agr
                     :cat cat
                     :essere essere
                     :infl infl}
          :synsem {:agr agr
                   :cat cat
                   :essere essere
                   :infl infl}}))
      ;; </category-independent>

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
           (and (log/warn (str "ignoring common noun with no gender specified: " %))
                false)))
     
     ;; filter out entries with no :cat.
     (filter-vals
      #(or (and (not (= :none (get-in % [:synsem :cat] :none)))
                (or (log/debug (str "lexical entry has a cat - good : " (strip-refs %)))
                    true))
           (and (log/warn (str "ignoring lexical entry with no :cat: " (strip-refs %)))
                false)))

     ;; end of language-specific grammar rules
))

(defn lexicon []
  (->
   (clojure.java.io/resource "babel/italiano/lexicon.edn")
   edn2lexicon))
