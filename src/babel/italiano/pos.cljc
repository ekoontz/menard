(ns babel.italiano.pos
  (:require
   [babel.lexiconfn :as lexiconfn]
   [babel.pos :as pos]
   [dag_unify.core :refer (unifyc)]))

(def cat-of-pronoun pos/cat-of-pronoun)
(def countable-noun pos/countable-noun)
(def determiner pos/determiner)
(def drinkable-noun pos/drinkable-noun)
(def noun pos/noun)
(def pronoun-acc pos/pronoun-acc)
(def sentential-adverb pos/sentential-adverb)

(def noun-agreement
  (let [agr (atom :top)
        cat (atom :top)]
    {:italiano {:agr agr
                :cat cat}
     :synsem {:cat cat
              :agr agr}}))

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

;; TODO: move place/city to if/then rules.
(def pronoun-reflexive
  {:synsem {:cat :noun
            :sem {:place false
                  :city false}
            :pronoun true
            :case :acc
            :reflexive true}})

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
(def subj-obj-humanness
  (let [human (atom :top)]
    {:synsem {:sem {:subj {:human human}
                    :obj {:human human}}}}))
