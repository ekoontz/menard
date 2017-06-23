(ns babel.espanol.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.encyclopedia :as encyc]
   [babel.lexiconfn :as lexiconfn
    :refer [compile-lex if-then listify
            map-function-on-map-vals
            verb-pred-defaults]]
   [babel.espanol.morphology :as morph]
   [babel.espanol.pos :refer [agreement-noun cat-of-pronoun common-noun determiner
                              feminine-noun intransitivize masculine-noun transitivize]]
   [babel.pos :as pos :refer [pronoun-acc]]
   [clojure.java.io :refer [resource]]
   [clojure.tools.logging :as log]
   [dag_unify.core :refer [fail? get-in unify]]))

(defn edn2lexicon [resource]
  (-> (lexiconfn/edn2lexicon resource)

      (compile-lex)

      (map-function-on-map-vals
       (fn [lexical-string lexical-val]
         (morph/phonize lexical-val lexical-string)))

      ((fn [lexicon]
         (merge-with concat lexicon
                     (listify 
                      (let [tmp (map #(listify %)
                                     (morph/exception-generator lexicon))]
                        (if (empty? tmp)
                          nil
                          (reduce #(merge-with concat %1 %2)
                                  tmp)))))))

      
      ;; if a non-auxiliary, non-reflexive verb has no :obj,
      ;; then its {:obj is :unspec}.
      (if-then {:synsem {:cat :verb
                         :aux false
                         :sem {:reflexive false
                               :obj :unspec}}}
               {:synsem {:sem {:obj :unspec}}})
      
      ;; make an intransitive version of every verb which has an
      ;; [:sem :obj] path.
      intransitivize
      
      ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
      transitivize
      
      (verb-pred-defaults encyc/verb-pred-defaults)
      
      ;; TODO: use (default) rather than (if-then)
      ;; if verb has no :aux, it's {:aux false}
      (if-then {:synsem {:cat :verb
                         :aux false}}
               {:synsem {:aux false}})
      
      (if-then {:synsem {:cat :verb
                         :subcat {:2 {:reflexive false}}}}
               {:synsem {:subcat {:2 {:reflexive false}}}})
      
      (if-then {:synsem {:cat :noun
                         :pronoun true
                         :null-pronoun false}}
               {:synsem {:null-pronoun false}})
      
      (if-then {:synsem {:cat :verb
                         :subcat {:3 '()}}}
               {:synsem {:subcat {:3 '()}}})
      
      
      (if-then
       (let [determiner (let [def (atom :top)]
                          {:synsem {:def def
                                    :sem {:def def}}})
             cat-of-pronoun :noun
             pronoun-acc :acc
             masculine-noun-agreement (let [agr (atom {:gender :masc})]
                                        {:espanol {:agr agr}
                                         :synsem {:agr agr}})
             
             feminine-noun-agreement (let [agr (atom {:gender :fem})]
                                       {:espanol {:agr agr}
                                        :synsem {:agr agr}})
             
             common-noun {:synsem {:cat :noun
                                   :propernoun false
                                   :agr {:person :3rd}
                                   :subcat {:1 {:cat :det}
                                            :2 '()}}}]
         common-noun) common-noun)
      
      ;; Cleanup functions can go here. Number them for ease of reading.
      ;; 1. this filters out any verbs without an inflection: infinitive verbs
      ;; should have inflection ':infinitive',
      ;; rather than not having any inflection.
      (map-function-on-map-vals
       (fn [k vals]
         (filter #(or (not (= :verb (get-in % [:synsem :cat])))
                      (not (= :none (get-in % [:synsem :infl] :none))))
                 vals)))))

(def lexicon (promise))
(defn deliver-lexicon []
  (if (not (realized? lexicon))
    (deliver lexicon (edn2lexicon (resource "babel/espanol/lexicon.edn")))))

