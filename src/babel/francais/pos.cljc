(ns babel.francais.pos
  (:require [babel.lexiconfn :as lexiconfn :refer (map-function-on-map-vals)]
            [babel.pos :as pos]
            [dag_unify.core :refer (unifyc)]))

(def verb-aux
  (let [sem (atom {:aspect :perfect
                  :tense :past})]
    (unifyc {:synsem {:sem sem
                      :subcat {:2 {:infl :past-p}}}}
            (let [aux (atom true)
                  pred (atom :top)
                  sem (atom {:pred pred})
                  subject (atom :top)]
              {:synsem {:aux aux
                        :sem sem
                        :subcat {:1 subject
                                 :2 {:cat :verb
                                     :aux false
                                     :subcat {:1 subject}
                                     :sem sem}}}}))))

(def agreement-noun pos/agreement-noun)
(def cat-of-pronoun pos/cat-of-pronoun)
(def common-noun pos/common-noun)
(def determiner pos/determiner)

(def noun-agreement
  (let [agr (atom :top)]
    {:français {:agr agr}
     :synsem {:agr agr}}))

(def feminine-noun (unifyc
                    noun-agreement (:feminine pos/noun)))

(def masculine-noun (unifyc
                     noun-agreement (:masculine pos/noun)))

(def adjective
  (unifyc pos/adjective
          (let [agr (atom :top)
                cat (atom :top)]
            {:français {:agr agr
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
            {:français {:agr agr
                        :essere essere-type
                        :infl infl}
             :synsem {:infl infl
                      :essere essere-type
                      :subcat {:1 {:agr agr}}}})))
(def transitive
  (unifyc verb-subjective
          pos/transitive))

(def intransitive-unspecified-obj
  (unifyc verb-subjective
          pos/intransitive-unspecified-obj))

(def intransitive
  (unifyc verb-subjective
          pos/intransitive))

(defn intransitivize [lexicon]
  (lexiconfn/intransitivize lexicon intransitive transitive intransitive-unspecified-obj))

(defn transitivize [lexicon]
  (lexiconfn/transitivize lexicon transitive verb-subjective))

(def gender-pronoun-agreement
  (let [gender (atom :top)]
    {:synsem {:cat :noun
              :pronoun true
              :agr {:gender gender}
              :sem {:gender gender}
              :subcat '()}}))
