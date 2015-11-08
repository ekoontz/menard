(ns babel.test.en
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.english.grammar :as eng]
            [babel.english.writer :as en]
            [babel.english.morphology :refer [fo get-string-1]]
            [babel.writer :as writer]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [dag-unify.core :refer [get-in]]))

(deftest generate-irregular-present
  (let [form {:english {:a {:english {:past {:1sing "was",
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
                                                :2sing "are"}}}
                        :b {:agr {:gender :fem
                                  :number :sing
                                  :person :3rd}
                            :cat :noun
                            :english "Antonia"
                            :pronoun false}}}]
    (is (= (fo form)
           "is Antonia"))))

(deftest generate-irregular-present-1
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

(deftest generate-irregular-present-2
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




