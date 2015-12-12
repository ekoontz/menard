(ns babel.espanol.pos)

(require '[babel.lexiconfn :as lexiconfn :refer (map-function-on-map-vals unify)])
(require '[babel.pos :as pos])

(def agreement-noun pos/agreement-noun)
(def cat-of-pronoun pos/cat-of-pronoun)
(def common-noun pos/common-noun)
(def determiner pos/determiner)

(def noun-agreement
  (let [agr (atom :top)]
    {:espanol {:agr agr}
     :synsem {:agr agr}}))

(def feminine-noun (unify
                    noun-agreement (:feminine pos/noun)))

(def masculine-noun (unify
                     noun-agreement (:masculine pos/noun)))
(def adjective
  (unify pos/adjective
         (let [agr (atom :top)
               cat (atom :top)]
           {:espanol {:agr agr
                      :cat cat}
            :synsem {:agr agr
                     :cat cat}})))

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (unify pos/verb-subjective
         (let [infl (atom :top)
               agr (atom :top)
               essere-type (atom :top)]
           {:espanol {:agr agr
                      :essere essere-type
                      :infl infl}
            :synsem {:infl infl
                     :essere essere-type
                     :subcat {:1 {:agr agr}}}})))

(def transitive
  (unify verb-subjective
         pos/transitive
         {:synsem {:essere false}}))

(def intransitive-unspecified-obj
  (unify verb-subjective
         {:synsem {:sem {:reflexive false}}}
         pos/intransitive-unspecified-obj))

(def intransitive
  (unify verb-subjective
         pos/intransitive))

(defn intransitivize [lexicon]
  (lexiconfn/intransitivize lexicon intransitive transitive intransitive-unspecified-obj))

(defn transitivize [lexicon]
  (lexiconfn/transitivize lexicon transitive verb-subjective))
