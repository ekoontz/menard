(ns babel.test.en
  (:refer-clojure :exclude [get-in])
  (:require [babel.directory :refer [models]]
            [babel.english :refer [analyze fo-ps generate medium morph parse sentences]]
            [babel.english.grammar :as grammar]
            [babel.english.lexicon :refer [lexicon]]
            [babel.english.morphology :refer [fo get-string]]

            [babel.over :refer [overc overh]]
            
            ;; TODO: add parsing tests
            [babel.parse :as parse]

            [clojure.repl :refer [doc]]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [fail? fail-path-between get-in strip-refs unify]]))

(defn display-expression [expr]
  {:en (morph expr)
   :subj (get-in expr [:synsem :sem :subj :pred])
   :pred (get-in expr [:synsem :sem :pred])
   :obj (get-in expr [:synsem :sem :obj :pred])})

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

(deftest future-parse
  (is (not (empty? (parse "he will speak")))))

(deftest conditional-parse
  (is (not (empty? (parse "she would speak")))))

(deftest imperfect-parse
  (is (not (empty? (parse "she used to speak"))))
  (is (not (empty? (parse "she was loving"))))
  (is (not (empty? (parse "she was speaking")))))

(deftest parse-with-gender-symbols
  (is (not (empty? (parse "I (♀) speak"))))
  (is (not (empty? (parse "I (♂) speak"))))
  (is (not (empty? (parse "you (♀) speak"))))
  (is (not (empty? (parse "you (♂) speak"))))
  (is (not (empty? (parse "you all (♀) speak"))))
  (is (not (empty? (parse "you all (♂) speak")))))

(deftest go-upstairs
  (is (not (empty? (parse "I go upstairs"))))
  (is (not (empty? (parse "you go upstairs"))))
  (is (not (empty? (parse "she goes upstairs"))))
  (is (not (empty? (parse "we go upstairs"))))
  (is (not (empty? (parse "you all go upstairs"))))
  (is (not (empty? (parse "they go upstairs")))))

(deftest be-called
  (is (= 10
         (count
          (filter #(not (= % ""))
                  (take
                   10
                   (repeatedly #(let [expression (generate {:modified false
                                                            :synsem {:cat :verb
                                                                     :sem {:pred :be-called}}})]
                                  (is (not (= "" (fo expression))))
                                  (log/info (display-expression expression))
                                  (fo expression)))))))))

(deftest mod-is-empty-list
  (let [result (generate {:modified false
                          :synsem {:cat :noun
                                   :sem {:pred :name
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
                                       :subj {:pred :lei}
                                       :tense :present}}}
                       :truncate false)))))
                                       
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
                                  :pred :dog}}})]
    (is (not (nil? result)))
    (is (= "Juana's dog" (fo result)))))

(deftest generate-with-possessive-2
  (let [result
        (generate {:synsem {:cat :noun
                            :sem {:mod {:pred :rosso}
                                  :number :sing
                                  :spec {:pred :of
                                         :of {:pred :Juana}}
                                  :pred :dog}}})]
    (is (not (nil? result)))
    (is (= "Juana's red dog" (fo result)))))

(deftest no-failed-bolts
  (let [result
        (->>
         (repeatedly #(let [generated
                            (generate 
                             {:comp {:synsem {:pronoun true}}
                              :modified false
                              :synsem {:sem {:pred :wash
                                             :mod nil
                                             :reflexive true}}})]
                        {:f (fo generated :from-language "it")
                         :sem (get-in generated [:synsem :sem :mod])}))
         (take 5))]
    (= (count result) 5)))

(deftest in-front-of
  (let [expr (generate {:synsem {:cat :prep
                                 :reflexive false
                                 :sem {:pred :in-front-of
                                       :obj {:pred :table
                                             :mod '()
                                             :number :sing
                                             :spec {:def :def
                                                    :pred :definite}}}}})]
    (is (= (fo expr)
           "in front of the table"))))

(deftest furniture-sentence
  (let [expr (generate {:modified false
                        :comp {:synsem {:agr {:person :3rd}}}
                        :synsem {:cat :verb
                                 :sem {:aspect :progressive
                                       :obj {:pred :table
                                             :mod '()
                                             :number :sing
                                             :spec {:def :def
                                                    :pred :definite}}
                                       :pred :in-front-of
                                       :subj {:pred :chair
                                              :mod '()
                                              :number :sing
                                              :spec {:def :def
                                                     :pred :definite}}
                                       :tense :present}}})]
    (log/info (str "furniture-sentence: " 
                   (display-expression expr)))

    (is (= (fo expr)
           "the chair is in front of the table"))))

(deftest reflexive-furniture-sentence
  (let [expr (generate {:modified false
                        :synsem {:cat :verb
                                 :sem {:pred :in-front-of
                                       :tense :present
                                       :reflexive true
                                       :subj {:pred :chair
                                              :mod '()
                                              :number :sing
                                              :spec {:def :def
                                                     :pred :definite}}
                                       :tense :present}}})]
    (log/info (str "reflexive furniture expression:" (display-expression expr)))
    (is (= (fo expr)
           "the chair is in front of itself"))))

(deftest infinitive-vp
  (let [medium (medium)
        vp-infinitive (:vp-infinitive (:grammar-map medium))
        expr (-> vp-infinitive
                 (overh (get (:lexicon medium) "speak"))
                 (overc (generate {:synsem {:sem {:pred :word
                                                  :mod '()
                                                  :spec {:def :def
                                                         :pred :definite}}
                                            :cat :noun}})))]
    (is true)))

(deftest past-and-gender-agreement
  (= (fo (generate {:synsem {:sem {:pred :go
                                   :aspect :perfect
                                   :tense :past
                                   :subj {:gender :fem
                                          :pred :loro}}}}))
     "they (♀) went"))

(deftest noun-number-agreement
  (= (fo (generate {:synsem {:agr {:number :plur}
                             :cat :noun
                             :sem {:pred :cat :spec {:def :def}
                                   :mod '()}}}))
     "the cats"))

(deftest phrasal-verbs
  (is (not (empty? (parse "I turned on the radio"))))
  (is (not (empty? (parse "I turned the radio on"))))
  (is (not (empty? (parse "I turned off the radio"))))
  (is (not (empty? (parse "I turned the radio off"))))
  (let [generated (morph (generate {:synsem {:sem {:pred :turn-on
                                                   :tense :present
                                                   :obj {:mod '()
                                                         :number :sing
                                                         :pred :radio
                                                         :spec {:def :def
                                                                :of {:pred nil}}}
                                                   :subj {:pred :lei}}
                                             :cat :verb}}))]
    (is (or (= "she turns the radio on"
                generated)
            (= "she turns on the radio"
               generated)))))







