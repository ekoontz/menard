(ns babel.italiano.pos
  (:require
   [babel.lexiconfn :as lexiconfn]
   [babel.pos :as pos]
   [dag_unify.core :refer (unifyc)]))

(def cat-of-pronoun pos/cat-of-pronoun)
(def common-noun pos/common-noun)
(def comparative pos/comparative)
(def countable-noun pos/countable-noun)
(def determiner pos/determiner)
(def drinkable-noun pos/drinkable-noun)
(def non-comparative-adjective pos/non-comparative-adjective)
(def noun pos/noun)
(def pronoun-acc pos/pronoun-acc)
(def sentential-adverb pos/sentential-adverb)
(def verb-aux
  (let [sem (atom {:tense :past})]
    (unifyc {:synsem {:sem sem
                      :subcat {:2 {:infl :past}}}}
            ;; whether a verb has essere or avere as its
            ;; auxiliary to form its past form:
            (let [aux (atom true)
                  pred (atom :top)
                  sem (atom {:pred pred})
                  subject (atom :top)
                  essere-binary-categorization (atom :top)]
              {:synsem {:aux aux
                        :sem sem
                        :essere essere-binary-categorization
                        :subcat {:1 subject
                                 :2 {:cat :verb
                                     :essere essere-binary-categorization
                                     :aux false
                           :subcat {:1 subject}
                                     :sem sem}}}}))))

(def noun-agreement
  (let [agr (atom :top)
        cat (atom :top)]
    {:italiano {:agr agr
                :cat cat}
     :synsem {:cat cat
              :agr agr}}))

(def agreement-noun (unifyc noun-agreement pos/agreement-noun))

(def feminine-noun (unifyc
                    pos/agreement-noun (:feminine pos/noun)))

(def masculine-noun (unifyc
                     pos/agreement-noun (:masculine pos/noun)))
(def adjective
  (unifyc pos/adjective
          (let [agr (atom :top)
                cat (atom :top)]
            {:italiano {:agr agr
                        :cat cat}
             :synsem {:agr agr
                      :cat cat}})))

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (unifyc pos/verb-subjective
          (let [infl (atom :top)
                agr (atom :top)
                essere-type (atom :top)]
            {:italiano {:agr agr
                        :essere essere-type
                        :infl infl}
             :synsem {:infl infl
                      :essere essere-type
                      :subcat {:1 {:agr agr}}}})))

(def transitive
  (unifyc verb-subjective
          pos/transitive))

(def intransitive-unspecified-obj
  (unifyc
   {:synsem {:sem {:reflexive false}}}
   (unifyc verb-subjective
           pos/intransitive-unspecified-obj)))

(def intransitive
  (unifyc
   {:synsem {:reflexive false}}
   (unifyc verb-subjective
           pos/intransitive)))

(defn intransitivize [lexicon]
  (lexiconfn/intransitivize lexicon intransitive transitive intransitive-unspecified-obj))

(defn transitivize [lexicon]
  (lexiconfn/transitivize lexicon transitive verb-subjective))

;; TODO: move place/city to if/then rules.
(def pronoun-reflexive
  {:synsem {:cat :noun
            :sem {:place false
                  :city false}
            :pronoun true
            :case :acc
            :reflexive true}})

(def agreement-of-obj-of-main-verb
  (let [agr (atom :top)]
    {:synsem {:subcat {:2 {:agr agr
                           :subcat {:2 {:agr agr}}}}}}))

(def agreement-of-subj-of-main-verb
  (let [agr (atom :top)]
    {:synsem {:subcat {:1 {:agr agr}
                       :2 {:agr agr
                           :subcat {:1 {:agr agr}}}}}}))

(def essere-aux-subject-agreement
  (let [subject-agreement (atom :top)]
    {:synsem {:subcat {:1 {:agr subject-agreement}
                       :2 {:subcat {:1 {:agr subject-agreement}}}}}}))

(def gender-and-number-agreement-1
  (let [gender (atom :top)
        number (atom :top)]
    {:synsem {:subcat {:1 {:agr {:gender gender
                                 :number number}}
                       :2 {:agr {:gender gender
                                 :number number}}}}}))

(def pred-is-obj-pred
  (let [obj (atom :top)
        pred (atom :top)]
    {:synsem {:sem {:obj obj
                    :pred pred}
              :subcat {:2 {:sem {:obj obj
                                 :pred pred}}}}}))

(def preposition
  (let [shared (atom :top)]
    {:synsem {:cat :prep
              :sem {:obj shared}
              :subcat {:1 {:sem shared}}}}))
  
(def reflexive
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

(def reflexive-indirect-obj-is-subcat3
  (let [subject (atom :top)
        indirect-object (atom :top)]
    (unifyc reflexive
            {:synsem {:sem {:subj subject
                            :obj subject
                            :iobj indirect-object}
                      :subcat {:1 {:sem subject}
                               :2 {:sem subject}
                               :3 {:sem indirect-object}}}})))
        
(def subj-obj-humanness
  (let [human (atom :top)]
    {:synsem {:sem {:subj {:human human}
                    :obj {:human human}}}}))

