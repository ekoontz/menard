(ns babel.english.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.encyclopedia :as encyc]
   [babel.english.morphology :as morph]
   [babel.lexiconfn :refer [apply-unify-key compile-lex default edn2lexicon
                            new-entries verb-pred-defaults]]
   [clojure.java.io :refer [resource]]
   [dag_unify.core :refer [dissoc-paths fail? get-in strip-refs unify]]))

;; TODO: allow a filter of lexemes
(defn deliver-lexicon []
  (->
   (edn2lexicon (resource "babel/english/lexicon.edn"))
   (compile-lex morph/exception-generator
                morph/phonize)

   apply-unify-key
   
   ;; <category-independent rules>
   
   (default
    (let [cat (atom :top)]
      {:english {:cat cat}
       :synsem {:cat cat}}))
   
   ;; </category-independent rules>

   ;; <adjective default rules>
   (default
    {:synsem {:cat :adjective
              :subcat {:1 {:cat :det}
                       :2 '()}
              :sem {:comparative false}}})
   
   ;; </adjective default rules>
   
   ;; <noun default rules>
   
   ;; make :propernoun and :pronoun available to morphological rules
   ;; to prevent e.g. (they -> *theys) or (ourselves -> *ourselvess)
   (default
    (let [pronoun (atom :top)
          propernoun (atom :top)]
      {:english {:pronoun pronoun
                 :propernoun propernoun}
       :synsem {:cat :noun
                :pronoun pronoun
                :propernoun propernoun}}))
   
   ;; pronouns have semantic number and gender.
   (default
    (let [gender (atom :top)
          number (atom :top)]
      {:synsem {:cat :noun
                :pronoun true
                :agr {:gender gender
                      :number number}
                :sem {:gender gender
                         :number number}}}))
   
   ;; propernouns have semantic number and gender.
   (default
    (let [gender (atom :top)
          number (atom :top)]
      {:synsem {:cat :noun
                   :propernoun true
                :agr {:gender gender
                      :number number}
                :sem {:gender gender
                      :number number}}}))
   
   ;; nouns have number-agreement morphology: 'the dog sleeps' vs 'the dogs sleep'
   ;; english.morphology needs to see the :cat=noun as well, so share that within :english.
   (default
    (let [agr (atom :top)]
      {:english {:agr agr}
       :synsem {:cat :noun
                :agr agr}}))
   ;; A pronoun is either reflexive or not reflexive, but
   ;; a non-pronoun is never reflexive.
   (default
    {:synsem {:cat :noun
              :pronoun false
              :reflexive false}})
   
   ;; </noun default rules>
   
   ;; <verb default rules>
   ;; add a second argument to every verb, unless it's explicitly disallowed with {:2 '()}.
   (default
    {:synsem {:cat :verb
              :subcat {:2 {:cat :top}}}})
   
   ;; this key: :modal-with determines inflection of argument of a modal verb
   ;; values can be :infinitive,:root, or false
   ;; TODO: should be possible to do with a modal verb's subcat instead of a whole new key.
   (default
    {:modal-with false
     :synsem {:cat :verb}})
   
   (default
    (let [modal-subject (atom {:cat :noun})]
      {:modal-with :infinitive
       :synsem {:cat :verb
                :subcat {:1 modal-subject
                         :2 {:cat :verb
                             :infl :infinitive
                             :subcat {:1 modal-subject
                                      :2 '()}}}}}))
   (default
    (let [modal-subject (atom {:cat :noun})]
      {:modal-with :root
       :synsem {:cat :verb
                :subcat {:1 modal-subject
                         :2 {:cat :verb
                             :infl :root
                             :subcat {:1 modal-subject
                                      :2 '()}}}}}))
   
   ;; prevent :shared-semantics :obj unless it's already set
   (default
    {:share-sem false
     :synsem {:cat :verb}})
   
   ;; semantic object of lexical verb is the same as the object of verb's prepositional phrase.
   (default
    (let [obj (atom :top)]
      {:share-sem :obj
       :synsem {:cat :verb
                :sem {:obj obj}
                :subcat {:2 {:cat :prep
                             :sem {:obj obj}}}}}))
   
   ;; add :sem :obj if necessary, so that intransitivize is triggered.
   (default {:modal-with false
             :synsem {:cat :verb
                      :subcat {:2 {:cat :noun}}
                      :sem {:obj {:pred :top}}}})
   
   (new-entries ;; remove the second argument and semantic object to make verbs intransitive.
    {:synsem {:cat :verb
              :aux false
              :sem {:obj {:top :top}
                    :shared-with-obj false
                    :reflexive false}
              ;; likely to be :noun or :prep but could be others
              :subcat {:2 {:cat :top}
                       :3 '()}}}
    (fn [lexeme]
      (unify
       (dissoc-paths lexeme [[:synsem :sem :obj]
                             [:synsem :subcat :2]])
       {:applied {:1 true}
        :synsem {:subcat {:2 '()}}})))
   
   (default ;; intransitive verbs' :obj is :unspec.
    {:modal-with false
     :applied {:2 true}
     :synsem {:cat :verb
              :subcat {:1 {:top :top}
                       :2 '()}
              :sem {:reflexive false
                    :shared-with-obj false
                    :obj :unspec}}})
   
   ;; subject-verb agreement
   (default (let [infl (atom :top)
                  agr (atom :top)]
              {:english {:agr agr
                         :infl infl}
               :synsem {:infl infl
                        :cat :verb
                        :subcat {:1 {:agr agr}}}}))
   
   (verb-pred-defaults encyc/verb-pred-defaults)
   
   ;; if a verb has a subject,
   ;; and the subject is {:cat :noun},
   ;; then the subject is {:synsem {:case :nom}}.
   (default {:synsem {:cat :verb
                      :subcat {:1 {:cat :noun
                                   :case :nom}}}})
   
   ;; if a verb has an object,
   ;; and the object is {:cat :noun},
   ;; then the object is {:synsem {:case :acc}}.
   (default {:synsem {:cat :verb
                      :subcat {:2 {:cat :noun
                                   :case :acc}}}})
   
   ;; aux default: false
   (default {:synsem {:cat :verb
                      :aux false}})
   
   ;; phrasal-verbs: false
   (default {:synsem {:cat :verb}
             :phrasal-verb false})
   
   (default
    (let [subject (atom :top)]
      {:synsem {:cat :verb
                :sem {:subj subject}
                :subcat {:1 {:sem subject}}}}))
   
   (default
    (let [object (atom :top)]
      {:phrasal-verb false
       :synsem {:cat :verb
                :sem {:obj object
                      :iobj nil}
                :subcat {:2 {:sem object}}}}))
   
   (default
    (let [object (atom :top)]
      {:phrasal-verb true
       :synsem {:cat :verb
                :sem {:obj object}
                :subcat {:2 {:cat :prep}
                         :3 {:cat :noun
                             :pronoun false
                             :subcat '()
                             :sem object}}}}))
   (default
    (let [object (atom :top)]
      {:phrasal-verb true
       :synsem {:cat :verb
                :sem {:obj object}
                :subcat {:2 {:cat :noun
                             :pronoun false
                             :subcat '()
                             :sem object}
                         :3 {:cat :prep}}}}))
   
   ;; reflexive=false
   (default {:synsem {:cat :verb
                      :sem {:reflexive false}}})

   ;; for transitive, non-reflexive verbs, the 2nd arg is non-reflexive by default.
   (default {:synsem {:cat :verb
                      :sem {:reflexive false}
                      :subcat {:2 {:reflexive false}}}})
   
   ;; for transitive, reflexive verbs, the 2nd arg is a reflexive pronoun by default.
   (default
    (let [subject-agr (atom :top)]
      {:synsem {:sem {:reflexive true}
                :cat :verb
                :subcat {:1 {:agr subject-agr}
                         :2 {:reflexive true
                             :pronoun true
                             :agr subject-agr}}}}))
   (default
    {:synsem {:cat :verb
              :subcat {:2 {:subcat '()}}}})
   
   ;; note that {:english {:exception true}} is
   ;; set by (babel.english.morphology/exception-generator)
   (default
    {:english {:exception false}
     :synsem {:cat :verb}})
   
   (default
    {:english {:exception true}
     :synsem {:cat :verb
              :participle true
              :infl :participle}})
   
   (default
    {:english {:exception true}
     :synsem {:cat :verb
              :aux false
              :infl :past
              :sem {:tense :past}}})
   ;; Not sure why or if this (default) rule is needed?
   ;; Why set the :aspect to :perfect by default?
   ;; TODO: remove if not needed.
   (default
    {:english {:exception true}
     :synsem {:cat :verb
              :aux false
              :infl :past
              :sem {:aspect :perfect
                    :tense :past}}})

   (default
    {:english {:exception true}
     :synsem {:cat :verb
              :aux false
              :infl :present
              :sem {:aspect :simple
                    :tense :present}}})
   
   ;; </verb default rules>
   
   ;; <prep default rules>
   (default
    (let [obj-sem (atom :top)]
      {:synsem {:cat :prep
                :subcat {:1 {:cat :noun
                             :case :acc
                             :subcat '()
                             :sem obj-sem}
                         :2 '()}
                :sem {:obj obj-sem}}}))
   
   ;; </prep default rules>
   
   ))

