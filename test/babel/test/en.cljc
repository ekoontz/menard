(ns babel.test.en
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.english.grammar :refer [small small-plus-vp-pronoun medium]]
            [babel.english.morphology :refer [fo get-string]]
            [babel.english.workbook :refer [analyze generate parse]]
            [babel.parse :as parse]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [get-in]]))

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

(deftest generate-irregular-present-4
  (let [form {:a {:a {:english "your"}
                  :b {:english "name",
                      :pronoun false,
                      :cat :noun,
                      :agr {:pronoun false,
                            :gender :masc,
                            :number :sing,
                            :person :3rd}}}
              :b {:a {:agr {:pronoun false,
                            :gender :masc,
                            :number :sing,
                            :person :3rd},
                      :past {:2plur "were",
                             :3sing "was",
                             :2sing "were",
                             :1sing "was",
                             :3plur "were",
                             :1plur "were"},
                      :english "be",
                      :present {:2plur "are",
                                :3sing "is",
                                :2sing "are",
                                :1sing "am",
                                :3plur "are",
                                :1plur "are"},
                      :infl :present,
                      :cat :verb}
                  :b {:cat :noun,
                      :pronoun false,
                      :agr {:pronoun false,
                            :gender :masc,
                            :number :sing,
                            :person :3rd},
                      :english "Juan"}}}]
    (is (or false (= (fo form)
                    "your name is Juan")))))
