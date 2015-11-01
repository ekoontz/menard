(ns babel.pos
  (:require
   [dag-unify.core :refer (unifyc)]))

;; TODO: this file has a lot of language-specific stuff that should be factored into pos/$language.
;; for example, english does not have gender agreement between articles, nouns and adjectives.
(def noun
  (let [gender (ref :top)
        ;; common nouns are underspecified for number: number selection (:sing or :plur) is deferred until later.
        ;; (except for mass nouns which are only singular)
        number (ref :top)
        ;; common nouns are neither nominative or accusative. setting their case to :top allows them to (match) with
        ;; verbs' case specifications like {:case {:not :acc}} or {:case {:not :nom}}.
        case (ref :top)
        person (ref :top)
        agreement
        (let [number (ref :top)
              gender (ref :top)
              person (ref :top)
              pronoun (ref :top)
              agr (ref {:number number
                        :pronoun pronoun
                        :gender gender
                        :person person})
              cat (ref :top)]
          {:synsem {:cat cat
                    :pronoun pronoun
                    :case :top
                    :subcat {:1 {:number number
                                 :person person
                                 :gender gender}}
                    :agr agr}})
        common
        {:synsem {:cat :noun
                  :propernoun false
                  :agr {:person :3rd}
                  :subcat {:1 {:cat :det}
                           :2 '()}}}

        masculine {:synsem {:agr {:gender :masc}}}
        feminine {:synsem {:agr {:gender :fem}}}

        mass
        (let [mass (ref true)]
          {:synsem {:subcat {:1 {:cat :det
                                 :mass mass
                                 :number :sing}}
                    :sem {:mass mass}}})

        countable
        (let [mass (ref false)]
          {:synsem {:subcat {:1 {:cat :det
                                 :mass mass}}
                    :sem {:mass mass}}})

        drinkable
        (unifyc mass
                common
                {:synsem {:sem {:number :sing
                                :drinkable true}}})]
    {:agreement agreement
     :common common
     :countable countable
     :drinkable drinkable
     :feminine feminine
     :masculine masculine}))

(def agreement-noun (:agreement noun))
(def common-noun (:common noun))
(def countable-noun (:countable noun))
(def drinkable-noun (:drinkable noun))

(def determiner
  (let [def (ref :top)]
    {:synsem {:def def
              :sem {:def def}}}))

(def proper-noun
  {:synsem {:cat :noun
            :pronoun false
            :propernoun false
            :agr {:person :3rd}
            :subcat '()}})

;; useful abbreviations (aliases for some commonly-used maps):
(def human {:human true})
(def animal {:animate true :human false})
(def adjective
  {:synsem {:cat :adjective}})

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (let [subj-sem (ref :top)
        subject-agreement (ref :nom)
        infl (ref :top)
        agr (ref :top)
        essere-type (ref :top)]
    {:synsem {:essere essere-type
              :infl infl
              :cat :verb
              :sem {:subj subj-sem}
              :subcat {:1 {:sem subj-sem
                           :cat :noun
                           :agr agr
                           :case subject-agreement}}}}))

;; intransitive: has subject but no object.
(def intransitive
  (unifyc verb-subjective
          {:synsem {:subcat {:2 '()}}}))

;; intransitive: has subject and no syntactic object, but only a semantic and underspecified (:unspec) object.
(def intransitive-unspecified-obj
  (unifyc intransitive
          {:synsem {:sem {:obj :unspec}}}))

;; transitive: has both subject and object.
(def transitive
  (unifyc verb-subjective
          (let [obj-sem (ref :top)
                infl (ref :top)]
            {:synsem {:sem {:obj obj-sem}
                      :infl infl
                      :subcat {:2 {:sem obj-sem
                                   :subcat '()
                                   :cat :noun
                                   :case :acc}}}})))

(def transitive-but-object-cat-not-set
  (unifyc verb-subjective
          (let [obj-sem (ref :top)
                infl (ref :top)]
            {:synsem {:sem {:obj obj-sem}
                      :infl infl
                      :subcat {:2 {:sem obj-sem
                                   :case :acc}}}})))

(def verb {:transitive transitive})

(def modal
  "modal verbs take a VP[inf] as their 2nd arg. the subject of the modal verb is the same as the subject of the VP[inf]"
  (let [subj-sem (ref :top)
        vp-inf-sem (ref {:subj subj-sem})
        subj-subcat (ref {:cat :noun
                          :sem subj-sem})]
     {:synsem {:sem {:subj subj-sem
                     :obj vp-inf-sem}
               :modal true
               :subcat {:1 subj-subcat
                        :2 {:sem vp-inf-sem
                            :cat :verb
                            :infl :infinitive
                            :subcat {:1 subj-subcat
                                     :2 '()}}}}}))

;; TODO: not using this: either use or lose.
(def transitive-but-with-prepositional-phrase-instead-of-noun
  (unifyc verb-subjective
          (let [obj-sem (ref :top)
                infl (ref :top)]
            {:synsem {:sem {:obj obj-sem}
                      :infl infl
                      :subcat {:2 {:sem obj-sem
                                   :subcat '()
                                   :cat :prep}
                               :3 '()}}})))

;; "Y is Xer than Z".
(def comparative
  {:synsem {:sem {:comparative true}
            :cat :adjective
            :subcat {:1 {:cat :noun}
                     :2 {:cat :prep
                         :subcat '()}}}})

(def pronoun-acc (ref :acc))

(def subcat1 {:synsem {:subcat {:1 {:cat :top}
                                :2 '()}}})

;; "Y is X."
;; TODO: put semantics here.
(def non-comparative-adjective
  subcat1)

(def comp-sem (ref {:activity false
                    :discrete false}))

(def disjunctive-case-of-pronoun (ref :disj))
(def cat-of-pronoun (ref :noun))

(def subcat0 {:synsem {:subcat '()}})

(def sentential-adverb
  (let [sentential-sem (ref :top)]
    {:synsem {:cat :sent-modifier
              :sem {:subj sentential-sem}
              :subcat {:1 {:sem sentential-sem
                           :subcat '()}}}}))

