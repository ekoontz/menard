(ns babel.francais.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.francais.morphology :as morph :refer [phonize]]
   [babel.francais.pos :refer [gender-pronoun-agreement intransitivize
                               transitivize verb-aux]]
   [babel.lexiconfn :as lexiconfn :refer [compile-lex default edn2lexicon if-has
                                          if-then listify map-function-on-map-vals]]
   [clojure.java.io :refer [reader resource]]
   [babel.pos :as pos :refer [pronoun-acc]]
   [dag_unify.core :refer [get-in unify]]))

(def lexicon (promise))
(def cat1 (atom :top))
(def gender (atom :top))
(def verb-aux-sem (atom {:aspect :perfect
                         :tense :present}))
(def verb-aux-subject (atom :top))
(declare compile-lexicon)

(defn deliver-lexicon []
  (if (not (realized? lexicon))
    (deliver
     lexicon
     (compile-lexicon))))

(defn compile-lexicon []
  (->
   (resource "babel/francais/lexicon.edn")
   (edn2lexicon)
   (compile-lex)
   
   (map-function-on-map-vals
    (fn [lexical-string lexical-val]
      (phonize lexical-val lexical-string)))
   
   ;; Mark lexemes with no :cat with their own :cat to avoid matching any rules after this.
   (default {:gender-pronoun-agreement false
             :synsem {:cat :lexeme-with-an-unspecified-category}
             :d-no-cat true})

   ;; Morphology only sees :français, but needs :cat to know how to conjugate words:
   ;; see also (defn babel.francais.morphology/pre-conjugate)
   (default {:français {:cat cat1}
             :synsem {:cat cat1}})
   
   ;; Agreement between subject pronouns and verbs.
   (default {:gender-pronoun-agreement true
             :synsem {:cat :noun
                      :pronoun true
                      :agr {:gender gender}
                      :sem {:gender gender}
                      :subcat '()}
             :d-gpa true})
   
   ((fn [lexicon]
      (let [exceptions (apply merge-with concat
                              (map (fn [x]
                                     {(first (keys x))
                                      [(first (vals x))]})
                                   (morph/exception-generator lexicon)))]
        (apply merge-with concat
               [exceptions lexicon]))))

   (if-has [:français :future-stem] :top
           {:français {:future {:stem true}
                       :conditional {:stem true}}})
   (default {:français {:conditional :regular}})
   (default {:français {:future :regular}})
   (default {:synsem {:cat :verb}
             :français {:future {:1sing :top
                                 :regular false}}
             :d-verb-irreg-future true})

   (if-has [:français :imperfect-stem] :top
           {:français {:imperfect {:stem true}}})
   (if-has [:français :boot-stem1] :top
           {:français {:present {:boot-stem1 true}}})
   (if-has [:français :boot-stem2] :top
           {:français {:present {:boot-stem2 true}}})
   
   (default {:français {:imperfect :regular}})
   (default {:synsem {:cat :verb}
             :français {:imperfect {:1sing :top
                                    :regular false}}
             :d-verb-irreg-imperfect true})
   (default {:français {:imperfect {:regular false
                                    :stem false}}})
   
   (default {:français {:present :regular}})
   (default {:synsem {:cat :verb}
             :français {:present {:1sing :top
                                  :regular false
                                  :boot-stem1 false
                                  :boot-stem2 false}}
             :d-verb-irreg-present true})

   ;; Prevent irregular infinitives ({:present :regular false})
   ;; from matching phrase structure rules intended for non-infinitives.
   (default {:synsem {:cat :verb}
             :français {:conjugated false
                        :exception false
                        :present {:regular false}}
             :d-verb-irreg-present true})
   
   (default {:synsem {:cat :verb}
             :français {:conjugated false
                        :exception false
                        :imperfect {:regular false}}
             :d-verb-irreg-imperfect true})

   (if-has [:français :past-participle] :top
           {:français {:past-p {:regular false}}})

   (default {:français {:past-p {:regular true}}})
   
   ;; Verbs are *not* aux unless explicitly stated as such..
   (default {:synsem {:cat :verb
                      :aux false}
             :d-verb-aux false})
   
   ;; ..but for verbs that *are* aux, then:
   (default {:synsem {:sem verb-aux-sem
                      :aux true
                      :subcat {:1 verb-aux-subject
                               :2 {:infl :past-p
                                   :sem verb-aux-sem
                                   :cat :verb
                                   :aux false
                                   :subcat {:1 verb-aux-subject}}}}
             :d-verb-aux true})
   
   ;; All pronouns are nouns.
   (default {:synsem {:cat :noun
                      :pronoun true}
             :d-noun true})
     
   ;; Make an intransitive version of every verb which has a path [:sem :obj].
   intransitivize
   
   ;; If verb does specify a [:sem :obj], then fill it in with subcat info.
   transitivize
   
   (if-then {:synsem {:cat :verb
                      :subcat {:3 '()}}}
            {:synsem {:subcat {:3 '()}}})
   
   ;; if object is not specified, then set to :unspec.
   ;; this prevents translations that may have actual objects - e.g. would allow translations like:
   ;; "Je mange" => "I eat the bread" whereas a better translation is "I eat".
   (if-then {:synsem {:cat :verb
                      :aux false
                      :sem {:obj :unspec
                            :reflexive false
                            }}}
            {:synsem {:sem {:obj :unspec}}})
   
   ;; default: reflexive=false.
   (if-then {:synsem {:cat :verb
                      :aux false
                      :sem {:reflexive false}}}
            {:synsem {:sem {:reflexive false}}})
   
   ;; TODO: use lexiconfn/if-then here, like espanol/lexicon does.
   ;; default: essere=false
   (map-function-on-map-vals
    (fn [k vals]
      (map (fn [val]
             ;; if: 1. the val's :cat is :verb
             ;;     2. it is not true that essere=true (either essere=false or essere is not defined)
             ;; then: essere=false
             (cond (and (= (get-in val [:synsem :cat])
                           :verb)
                        (not (= true (get-in val [:synsem :essere] false))))
                   (unify val {:synsem {:essere false}})
                   true ;; otherwise, leave the verb alone
                   val))
           vals)))

;; Cleanup functions can go here. Number them for ease of reading.
;; 1. this filters out any verbs without an inflection:
;; infinitive verbs should have inflection ':infinitive',
;; rather than not having any inflection.
   (map-function-on-map-vals
    (fn [k vals]
      (filter #(or (not (= :verb (get-in % [:synsem :cat])))
                   (not (= :none (get-in % [:synsem :infl] :none))))
              vals)))
   ))



