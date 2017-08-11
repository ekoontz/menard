(ns babel.francais.pos
  (:require [babel.lexiconfn :as lexiconfn]
            [babel.pos :as pos]
            [dag_unify.core :refer (unify)]))

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (unify pos/verb-subjective
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
  (unify verb-subjective
         pos/transitive))

(def intransitive-unspecified-obj
  (unify verb-subjective
         pos/intransitive-unspecified-obj))

(def intransitive
  (unify verb-subjective
         pos/intransitive))

(defn intransitivize [lexicon]
  (lexiconfn/intransitivize lexicon intransitive transitive intransitive-unspecified-obj))

(defn transitivize [lexicon]
  (lexiconfn/transitivize lexicon transitive verb-subjective))
