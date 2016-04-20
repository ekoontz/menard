(ns babel.english.pos
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.english.morphology :refer (fo)]
   ;; TODO: use unify/unifyc, not lexiconfn/unify: they are (should be equivalent), and
   ;; to have both is confusing.
   [babel.lexiconfn :as lexiconfn :refer (map-function-on-map-vals unify)]
   [babel.pos :as pos]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :as unify :refer (dissoc-paths get-in serialize unifyc)]))

(def adjective pos/adjective)
(def animal pos/common-noun)
(def common-noun pos/common-noun)
(def countable-noun pos/common-noun)

(def feminine-noun (unifyc
                    pos/agreement-noun (:feminine pos/noun)))

(def masculine-noun (unifyc
                    pos/agreement-noun (:masculine pos/noun)))

(def agreement-noun
  (let [agr (atom :top)]
    {:english {:agr agr}
     :synsem {:agr agr}}))

(def subject-verb-agreement
  (let [infl (atom :top)
        agr (atom :top)]
    {:english {:agr agr
               :infl infl}
     :synsem {:infl infl
              :subcat {:1 {:agr agr}}}}))

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (unify pos/verb-subjective
         subject-verb-agreement))

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
