(ns babel.francais.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.francais.morphology :as morph :refer [phonize]]
   [babel.francais.pos :refer [gender-pronoun-agreement intransitivize
                               transitivize verb-aux]]
   [babel.lexiconfn :as lexiconfn :refer [compile-lex default edn2lexicon if-then
                                          listify map-function-on-map-vals]]
   [clojure.java.io :refer [reader resource]]
   [babel.pos :as pos :refer [pronoun-acc]]
   [dag_unify.core :refer [get-in unify]]))

(def lexicon (promise))
(def gender (atom :top))
(def verb-aux-sem (atom {:aspect :perfect
                         :tense :past}))
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

   (map-function-on-map-vals
    (fn [lexical-string lexical-vals]
      (map (fn [lexical-val]
             (if (or (= (string? (get-in lexical-val [:français :present :1sing])))
                     (= (string? (get-in lexical-val [:français :boot-stem1]))))
               (unify {:français {:present {:regular false}}}
                      lexical-val)
               lexical-val))
           lexical-vals)))

   (default {:synsem {:cat :verb}
             :d1 true
             :français {:present {:regular true}}})
   
   (map-function-on-map-vals
    (fn [lexical-string lexical-vals]
      (map (fn [lexical-val]
             (if (and (= (string? (get-in lexical-val [:français :imperfect :1sing])))
                      (= :verb (get-in lexical-val [:synsem :cat])))
               (unify {:français {:imperfect {:regular false}}}
                      lexical-val)
               lexical-val))
           lexical-vals)))

   (default {:synsem {:cat :verb}
             :français {:imperfect {:regular true}}
             :d2 true})

   (map-function-on-map-vals
    (fn [lexical-string lexical-vals]
      (map (fn [lexical-val]
             (if (and (= (string? (get-in lexical-val [:français :future :1sing])))
                      (= :verb (get-in lexical-val [:synsem :cat])))
               (unify {:français {:future {:regular false}}}
                      lexical-val)
               lexical-val))
           lexical-vals)))
   
   ;; Mark lexemes with no :cat with their own :cat to avoid matching any rules after this.
   (default {:gender-pronoun-agreement false
             :synsem {:cat :lexeme-with-an-unspecified-category}
             :d-no-cat true})
      
   ;; Agreement between subject pronouns and verbs.
   (default {:gender-pronoun-agreement true
             :synsem {:cat :noun
                      :pronoun true
                      :agr {:gender gender}
                      :sem {:gender gender}
                      :subcat '()}
             :d-gpa true})

   (default {:synsem {:cat :verb}
             :français {:future {:regular true}}
             :d-future true})
   
   ((fn [lexicon]
      (let [exceptions (apply merge-with concat
                              (map (fn [x]
                                     {(first (keys x))
                                      [(first (vals x))]})
                                   (morph/exception-generator lexicon)))]
        (apply merge-with concat
               [exceptions lexicon]))))

   ;; Prevent irregular infinitives ({:present :regular false})
   ;; from matching phrase structure rules intended for non-infinitives.
   (default {:synsem {:cat :verb}
             :français {:exception false
                        :present {:regular false
                                  :conjugated false}}
             :d-verb-irreg-present true})
   
   (default {:synsem {:cat :verb}
             :français {:exception false
                        :imperfect {:regular false
                                    :conjugated false}}
             :d-verb-irreg-imperfect true})
      
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



