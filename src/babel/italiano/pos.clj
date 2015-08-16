(ns babel.italiano.pos)

(require '[babel.lexiconfn :as lexiconfn])
(require '[babel.pos :as pos])
(require '[dag-unify.core :refer (unifyc)])

(def agreement-noun pos/agreement-noun)
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
(def verb-aux pos/verb-aux)

(def noun-agreement
  (let [agr (ref :top)]
    {:italiano {:agr agr}
     :synsem {:agr agr}}))

(def feminine-noun (unifyc
                    noun-agreement (:feminine pos/noun)))

(def masculine-noun (unifyc
                     noun-agreement (:masculine pos/noun)))

(def adjective
  (unifyc pos/adjective
          (let [agr (ref :top)
                cat (ref :top)]
            {:italiano {:agr agr
                        :cat cat}
             :synsem {:agr agr
                      :cat cat}})))

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (unifyc pos/verb-subjective
          (let [infl (ref :top)
                agr (ref :top)
                essere-type (ref :top)]
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
