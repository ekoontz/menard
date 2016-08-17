(ns babel.test.en
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.english :refer [analyze generate parse]]
            [babel.english.grammar :refer [small small-plus-plus-np small-plus-vp-pronoun medium]]
            [babel.english.lexicon :refer [lexicon]]
            [babel.english.morphology :refer [fo get-string]]

            ;; TODO: add parsing tests
            [babel.parse :as parse]

            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [get-in strip-refs]]))

(deftest generate-irregular-present-1
  (let [form {:english {:a {:cat :verb,
                            :infl :present,
                            :present {:1sing "am",
                                      :3plur "are",
                                      :1plur "are",
                                      :2plur "are",
                                      :3sing "is",
                                      :2sing "are"},
                            :past {:1sing "was",
                                   :3plur "were",
                                   :1plur "were",
                                   :2plur "were",
                                   :3sing "was",
                                   :2sing "were"},
                            :agr {:person :3rd,
                                  :gender :masc,
                                  :number :sing},
                            :english "be"},
                        :b {:agr {:person :3rd,
                                  :gender :masc,
                                  :number :sing},
                            :english "Antonio",
                            :cat :noun,
                            :pronoun false}}}]
    (is (= (fo form)
           "is Antonio"))))

(deftest generate-irregular-present-2
  (let [form {:english {:past {:1sing "was",
                               :3plur "were",
                               :1plur "were",
                               :2plur "were",
                               :3sing "was",
                               :2sing "were"},
                        :agr {:person :3rd
                              :gender :fem
                              :pronoun false
                              :number :sing}
                        :english "be",
                        :cat :verb,
                        :infl :present,
                        :present {:1sing "am",
                                  :3plur "are",
                                  :1plur "are",
                                  :2plur "are",
                                  :3sing "is",
                                  :2sing "are"}}}]
    (is (= (fo form)
           "is"))))

(deftest generate-irregular-present-3
  (let [form {:english {:a {:infl :present,
                            :past {:2sing "were",
                                   :1sing "was",
                                   :3sing "was",
                                   :3plur "were",
                                   :2plur "were",
                                   :1plur "were"},
                            :agr {:gender :masc,
                                  :person :3rd,
                                  :number :sing},
                            :cat :verb,
                            :present {:2sing "are",
                                      :1sing "am",
                                      :3sing "is",
                                      :3plur "are",
                                      :2plur "are",
                                      :1plur "are"},
                            :english "be"},
                        :b {:pronoun false,
                            :agr {:gender :masc,
                                  :person :3rd,
                                  :number :sing},
                            :cat :noun,
                            :english "Juan"}}}]
    (is (= (fo form)
           "is Juan"))))

(deftest generate-irregular-imperfect
  (let [form {:english
              {:a {:english "you all",
                   :agr {:person :2nd, :gender :fem, :number :plur},
                   :cat :noun, :pronoun true},
               :b {:past "went downstairs",
                   :participle "going downstairs"
                   :present {:3sing "goes downstairs"},
                   :english "go downstairs",
                   :cat :verb,
                   :infl :imperfect,
                   :agr {:person :2nd, :gender
                         :fem, :number
                         :plur}}}}]
    (let [surface-form (fo form)]
      (is (or (= surface-form
                 "you all were going downstairs")
              (= surface-form
                 "you all used to go downstairs"))))))

(deftest simple-parse
  (is (not (empty? (parse "she sleeps")))))

(deftest be-called
  (is (= 10
         (count
          (filter #(not (= % ""))
                  (take
                   10
                   (repeatedly #(let [foo (generate {:synsem {:cat :verb
                                                              :modified false
                                                              :sem {:pred :be-called}}})]
                                  (is (not (= "" (fo foo))))
                                  (log/info (str "fo: " (fo foo)))
                                  (fo foo)))))))))

(deftest mod-is-empty-list
  (let [result (generate {:synsem {:sem {:pred :name
                                         :mod '()}}})]
    (is (= (get-in result [:synsem :sem :mod]
                   :undefined-should-not-return-this)
           '()))))

(deftest her-name-is-luisa
  (is (= "her name is Luisa"
         (fo (generate {:modified false
                        :synsem {:cat :verb
                                 :sem {:mod '()
                                       :iobj {:pred :luisa}
                                       :pred :be-called
                                       :subj {:pred :lei}}}})))))
                                       
(deftest jean-s
  (is (not (empty? (parse "Jean's")))))

(deftest the-dog-s-house
  (is (not (empty? (parse "the dog's house")))))

(deftest the-dogs-house
  (is (not (empty? (parse "the dogs' house")))))

(deftest generate-with-possessive-1
  (let [result
        (generate {:synsem {:cat :noun
                            :sem {:number :sing
                                  :mod '()
                                  :spec {:pred :of
                                         :of {:pred :Juana}}
                                  :pred :cane}}})]
    (is (not (nil? result)))
    (is (= "Juana's dog" (fo result)))))

(deftest generate-with-possessive-2
  (let [result
        (generate {:synsem {:cat :noun
                            :sem {:mod {:pred :rosso}
                                  :number :sing
                                  :spec {:pred :of
                                         :of {:pred :Juana}}
                                  :pred :cane}}})]
    (is (not (nil? result)))
    (is (= "Juana's red dog" (fo result)))))

(deftest no-failed-bolts
  (let [result
        (->>
         (repeatedly #(println 
                       (let [generated
                             (generate 
                              {:comp {:synsem {:pronoun true}}
                               :modified false
                               :synsem {:sem {:pred :wash
                                              :mod nil
                                              :reflexive true}}}
                              :model medium)]
                         {:f (fo generated :from-language "it")
                          :sem (get-in generated [:synsem :sem :mod])})))
         (take 5))]
    (= (count result) 5)))


                                        
