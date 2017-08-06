(ns ^{:doc "French testing code"}
    babel.test.fr
  (:refer-clojure :exclude [get-in])
  (:require [babel.directory :refer [models]]
            [babel.francais :as fr :refer [generate parse medium]]
            [babel.francais.grammar :as grammar :refer [fo-ps]]
            [babel.francais.morphology :refer [analyze conjugate fo get-string]]
            [babel.generate :as generate]
            [babel.over :as over]
            [babel.parse :as parse]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [fail-path fail? get-in strip-refs unify]]))

(defn small [] (fr/small))

;; TODO: these defns (lookup) are convenience functions are duplicated in
;; babel.workbook.francais: factor out to babel.francais.
;; TODO: do morphological analysis
;; do find non-infinitives (e.g. find 'parler' given 'parle')
;; and then apply conjugated parts to lexeme
;; i.e. if input is 'parle', return
;; list of lexemes; for each, [:synsem :agr :person] will be
;; 1st, 2nd, or 3rd, and for all, number will be singular.

(defn over
  ([arg1]
   (over/over (vals (:grammar-map (medium))) (analyze arg1 (:lexicon (medium)))))
  ([grammar arg1]
   (over/over grammar (analyze arg1 (:lexicon (medium)))))
  ([grammar arg1 arg2]
   (cond (string? arg1)
         (over grammar (analyze arg1 (:lexicon (medium)))
               arg2)

         (string? arg2)
         (over grammar arg1 (analyze arg2 (:lexicon (medium))))

         true
         (over/over grammar arg1 arg2))))

(deftest conjugate-present-er
  (is (= "parlons" (conjugate "parler"
                              {:français {:present :regular}
                               :synsem {:cat :verb
                                        :infl :present
                                         :agr {:number :plur
                                               :person :1st}}}))))
(deftest conjugate-present-er-with-g
  (is (= "mangeons" (conjugate "manger"
                               {:français {:present :regular}
                                :synsem {:cat :verb
                                         :infl :present
                                         :agr {:number :plur
                                               :person :1st}}}))))
(deftest conjugate-present-re-with-d
  (is (= "apprenons" (conjugate "apprendre"
                                {:français {:present :regular}
                                 :synsem {:cat :verb
                                          :infl :present
                                          :agr {:number :plur
                                                :person :1st}}}))))
(deftest conjugate-conditional
  (is (= "parlerions"
         (conjugate "parler"
                    {:français {:conditional :regular}
                     :synsem {:cat :verb
                              :infl :conditional
                              :agr {:number :plur
                                    :person :1st}}}))))
(deftest conjugate-future
  (is (= "parleront"
         (conjugate "parler"
                    {:français {:future :regular}
                     :synsem {:cat :verb
                              :infl :future
                              :agr {:number :plur
                                    :person :3rd}}}))))
(deftest conjugate-irregular-future
  (is (= "irez"
         (conjugate "aller"
                    {:français {:future-stem "ir"}
                     :synsem {:cat :verb
                              :infl :future
                              :agr {:number :plur
                                    :person :2nd}}}))))

(deftest conjugate-irregular-conditional
  (is (= "iraient"
         (conjugate "aller"
                    {:français {:future-stem "ir"}
                     :synsem {:cat :verb
                              :infl :conditional
                              :agr {:number :plur
                                    :person :3rd}}}))))

(deftest analyze-aller
  (is (not (empty? (analyze "aller" (:lexicon (small)))))))

(deftest analyze-irez
  (is (not (empty? (analyze "irez" (:lexicon (small)))))))

(deftest conjugate1
  (let [from #"s'([aeéiou].*)er$"
        infinitive "s'amuser"
        to "$1é"]
    (is (= "amusé" (string/replace infinitive from to)))))

(deftest conjugate2
  (is (= (conjugate "s'amuser"
                    {:synsem {:cat :verb
                              :infl :past-p
                              :agr {:number :sing}}})
         "amusé")))
         
(deftest conjugate3
  (is (= (conjugate "s'amuser"
                    {:synsem {:cat :verb
                              :infl :past-p
                              :agr {:number :plur}}})
         "amusés")))

(deftest conjugate4
  (is (= (conjugate "s'amuser"
                    {:synsem {:cat :verb
                              :infl :past-p
                              :agr {:gender :masc
                                    :number :plur}}})
         "amusés")))

(deftest conjugate5
  (is (= (conjugate "s'amuser"
                    {:synsem {:cat :verb
                              :infl :past-p
                              :agr {:gender :fem
                                    :number :plur}}})
         "amusées")))

(deftest conjugate6
  (is (= (conjugate "se blesser"
                    {:synsem {:cat :verb
                              :infl :past-p
                              :agr {:gender :masc
                                    :number :sing}}})
         "blessé")))

(deftest conjugate7
  (is (= (conjugate "se blesser"
                    {:synsem {:cat :verb
                              :infl :past-p
                              :agr {:gender :fem
                                    :number :plur}}})
         "blessées")))

(deftest conjugate8
  (is (= (conjugate "se blesser"
                    {:français {:present :regular}
                     :synsem {:cat :verb
                              :infl :present
                              :agr {:person :2nd
                                    :number :sing}}})
         "blesses")))

(deftest conjugate-boot-stem-present
  (is (= "buvons"
         (conjugate "boire"
                    {:français {:boot-stem2 "buv"
                                :present {:boot-stem2 true}
                                :français "boire"}
                     :synsem {:cat :verb
                              :infl :present
                              :agr {:person :1st
                                    :number :plur}}}))))

(deftest conjugate-conditional-reflexive
  (is (= "amuserais"
         (conjugate "s'amuser"
                    {:synsem {:cat :verb,
                              :infl :conditional,
                              :agr {:gender :fem, :person :1st, :number :sing}},
                     :français {:cat :verb, :essere true,
                                :future :regular, :present :regular,
                                :imperfect :regular, :infl :conditional,
                                :infinitive "s'amuser",
                                :agr {:gender :fem, :person :1st, :number :sing},
                                :français "s'amuser",
                                :initial false, :exception true,
                                :conditional :regular}}))))

(deftest generate-present-by-root-regular-1
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :top
                                         :subj {:pred :I}
                                         :tense :present
                                         :aspect :progressive}}
                          :root {:français {:français "parler"}}
                          :rule "s-present-nonphrasal"}
                         :model (small))]
    (is (= "je parle" (fo result)))))

(deftest generate-present-by-semantics-regular-1
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :abandon
                                         :subj {:pred :I}
                                         :tense :present
                                         :aspect :progressive}}}
                         :model (small))]
    (is (= "j'abandone" (fo result)))))

(deftest generate-present-by-root-regular-2
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:aspect :progressive
                                         :tense :present}}
                          :root {:français {:français "abandoner"}}
                          :comp {:synsem {:agr {:person :1st, :number :sing}}}}
                         :model (small))]
    (is (= "j'abandone" (fo result)))))

(deftest generate-present-by-semantics-irregular-1
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :sleep
                                         :subj {:pred :I}
                                         :tense :present
                                         :aspect :progressive}}}
                         :model (small))]
    (is (= "je dors" (fo result)))))

(deftest generate-present-by-root-irregular-1
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:aspect :progressive
                                         :tense :present}}
                          :root {:français {:français "dormir"}}
                          :comp {:synsem {:agr {:person :1st, :number :sing}}}}
                         :model (small))]
    (is (= "je dors" (fo result)))))

(deftest generate-present-by-root-with-boot-stem
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:subj {:pred :noi}
                                         :tense :present
                                         :aspect :progressive}}
                          :root {:français {:français "boire"}}}
                         :model (small))]
    (is (= "nous buvons" (fo result)))))

(deftest generate-irregular-conditional
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:subj {:pred :I}
                                         :tense :conditional}}
                          :root {:français {:français "aller"}}}
                         :model (small))]
    (is (= "j'irais" (fo result)))))

(deftest generate-regular-conditional
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:subj {:pred :I}
                                         :tense :conditional}}
                          :root {:français {:français "dormir"}}}
                         :model (small))]
    (is (= "je dormirais" (fo result)))))

(deftest generate-regular-conditional-reflexive
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:subj {:pred :I}
                                         :tense :conditional}}
                          :root {:français {:français "s'amuser"}}}
                         :model (small))]
    (is (= "je m'amuserais" (fo result)))))

(deftest generate-present-irregular-2
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :be
                                         :subj {:pred :I}
                                         :tense :present}}}
                         :model (small))]
    (is (= "je suis" (fo result)))))

(deftest generate-imparfait-regular-er
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :study
                                         :subj {:pred :I}
                                         :tense :past
                                         :aspect :progressive}}}
                         :model (small))]
    (is (= :imperfect (get-in result [:head :synsem :infl])))
    (is (= "j'étudiais" (fo result)))))

(deftest generate-imparfait-regular-ir
  (let [result (generate {:head {:agr {:gender :masc}}
                          :synsem {:subcat '()
                                   :sem {:pred :sleep
                                         :subj {:pred :loro}
                                         :tense :past
                                         :aspect :progressive}}}
                         :model (small))]
    (is (= :imperfect (get-in result [:head :synsem :infl])))
    (is (= :masc (get-in result [:head :français :agr :gender])))
    (is (= "ils dormaient" (fo result)))))

(deftest generate-imparfait-regular-re
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :sell
                                         :subj {:pred :voi}
                                         :tense :past
                                         :aspect :progressive}}}
                         :model (small))]
    (is (= :imperfect (get-in result [:head :synsem :infl])))
    (is (= :masc (get-in result [:head :français :agr :gender])))
    (is (= "vous vendiez" (fo result)))))

(deftest generate-imparfait-finir
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:subj {:pred :noi}
                                         :tense :past
                                         :aspect :progressive}}
                          :root {:français {:français "finir"}}}
                         :model (small))]
    (is (= "nous finissions" (fo result)))))

(deftest generate-imperfect-irregular-être
  (let [result (generate {:synsem {:subcat '()
                                   :infl :imperfect
                                   :sem {:pred :be
                                         :subj {:pred :I}}}}
                         :model (small))]
    (is (= "j'étais" (fo result)))))

(deftest generate-imperfect-irregular-avoir
  (let [result (generate/generate {:synsem {:subcat '()
                                            :infl :imperfect
                                            :sem {:pred :have
                                                  :subj {:pred :I}}}}
                                  (small)
                                  :truncate-children false)]
    (is (not (nil? result)))
    (is (= "av" (get-in result [:head :français :imperfect-stem])))
    (is (= "j'avais" (fo result)))))

(deftest être-as-aux
  (let [lex (:lexicon (small))
        result
        (filter #(not (fail? %))
                (map (fn [rule]
                       (unify rule
                              ;; TODO: instead of using "suis", use
                              ;; (francais.morphology/lookup-in)
                              ;; with {:agr {:person :1st :number :sing}}.
                              {:head (last (get lex "suis"))}))
                     (:grammar (small))))]
    (is (not (empty? result)))
    (is (= (get-in (first result) [:rule]) "vp-aux"))))

(deftest vp-aux-test
  (let [rule (first (filter #(= (:rule %) "vp-aux")
                            (:grammar (small))))]
    (is (not (nil? rule)))))

(def etre-test
  (is (not (nil? (first (filter #(= true (get-in % [:synsem :aux]))
                                (get (:lexicon (small)) "être")))))))
(deftest over-test
  (let [lex (:lexicon (small))
        grammar (:grammar (small))
        result
        (over grammar
              (get lex "nous")
              (over grammar
                    (get lex "sommes") (get lex "aller")))]
    (is (= 2 (count result)))
    (is (or (= (fo (nth result 0))
               "nous sommes allées")
            (= (fo (nth result 1))
               "nous sommes allées")))
    (is (or (= (fo (nth result 0))
               "nous sommes allés")
            (= (fo (nth result 1))
               "nous sommes allés")))))

(deftest passe-compose-morphology
  (let [result
        {:français
         {:a {:initial true,
              :cat :noun
              :français "nous"},
          :b {:b {:cat :verb
                  :future-stem "ir",
                  :agr {:number :plur,
                        :gender :fem
                        :person :1st},
                  :present {:3plur "vont",
                            :2plur "allez",
                            :3sing "va",
                            :1sing "vais",
                            :2sing "vas",
                            :1plur "allons"},
                  :français "aller",
                  :infl :past-p,
                  :essere true,
                  :initial false},
              :initial false,
              :a {:cat :verb
                  :infl :present,
                  :infinitive "être",
                  :initial true,
                  :agr {:number :plur,
                        :person :1st},
                  :essere false,
                  :passato "été",
                  :imperfect {:2sing "étais",
                              :3plur "étaient",
                              :2plur "étiez",
                              :1sing "étais",
                              :3sing "était",
                              :1plur "étions"},
                  :present {:2sing "es",
                            :3plur "sont",
                            :2plur "êtes",
                            :1sing "suis",
                            :3sing "est",
                            :1plur "sommes"},
                  :futuro {:2sing "seras",
                           :3plur "seront",
                           :2plur "serez",
                           :1sing "serai",
                           :3sing "sera",
                           :1plur "serons"},
                  :futuro-stem "ser",
                  :exception true,
                  :français "sommes"}}}}]
    (is (= (fo result) "nous sommes allées"))))

(deftest generate-passe-compose-1
  (let [result
        (generate (unify
                   {:synsem {:subcat '()}}
                   {:synsem {:sem {:subj {:pred :noi
                                          :gender :fem}
                                   :pred :go
                                   :aspect :perfect
                                   :tense :past}}})
                  :model (small))]
    (and (is (not (nil? result)))
         (is (= (fo result) "nous sommes allées")))))

(deftest generate-passe-compose
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :go
                                         :subj {:pred :noi
                                                :gender :fem}
                                         :aspect :perfect
                                         :tense :past}}})]
    (is (not (nil? result)))
    (is (= (fo result) "nous sommes allées"))))

(deftest generate-reflexive-present
  (let [rules {:s-present-phrasal
               (first (filter #(= (get % :rule) "s-present-phrasal")
                              (:grammar (medium))))
               :vp-pronoun-nonphrasal
               (first (filter #(= (get % :rule) "vp-pronoun-nonphrasal")
                              (:grammar (medium))))}

        result (over (get rules :s-present-phrasal)
                     "je" 
                     (over (get rules :vp-pronoun-nonphrasal)
                           "me" "s'amuser"))]
    (is (= (fo (first result))
           "je m'amuse"))))

(deftest generate-have-fun-sentence
  (let [result (generate
                {:synsem {:sem {:pred :have-fun}}})]
    (is (= (get-in result [:synsem :sem :pred]) :have-fun))))

(deftest generate-vp-aux-reflexive
  (let [result
        (generate
         {:synsem {:subcat '()
                   :sem {:subj {:pred :lei}
                         :pred :have-fun :tense :past}}})]
    (is (= (fo result) "elle l'est amusée"))))

(deftest generate-vp-present-reflexive
  (let [result
        (generate
         {:synsem {:subcat '() :sem {:subj {:pred :lei}
                                     :pred :have-fun :tense :present}}})]
  (is (= (fo result) "elle l'amuse"))))

(deftest generate-named-sentence
  (let [result
        (generate
         {:synsem {:subcat '()
                   :sem {:pred :be-called
                         :subj {:pred :lui}
                         :obj {:pred :Jean}}}})]
    (is (= (fo result) "il l'appele Jean"))))

(deftest parse-reflexive
  (let [result (first (parse "il l'amuse"))]
    (not (nil? result))
    (is (= (get-in result [:synsem :sem :pred])
           :have-fun))
    (is (= (get-in result [:synsem :sem :subj :pred])
           :lui))))

(deftest parse-reflexive
  (let [result (first (parse "tu t'amuses"))]
    (not (nil? result))
    (is (= (get-in result [:synsem :sem :pred])
           :have-fun))
    (is (= (get-in result [:synsem :sem :subj :pred])
           :tu))))

(deftest parse-past-nonreflexive
  (let [result (parse "j'ai parlé")]
    (not (nil? result))
    (is (= 4 (count result)))
    (is (or (= (get-in (first result) [:synsem :sem :pred])
               :talk)
            (= (get-in (first result) [:synsem :sem :pred])
               :speak)))
    (is (= (get-in (first result) [:synsem :sem :tense])
           :past))))

(deftest parse-reflexive-past
  (let [result (first (parse "tu t'es amusé"))]
    (is (not (nil? result)))))

(deftest parse-present
  (let [result (parse "nous parlons")]
    (is (not (nil? result)))
    (is (or (= (get-in (first result)
                       [:synsem :sem :pred])
               :speak)
            (= (get-in (first result)
                       [:synsem :sem :pred])
               :talk)))
    (is (= (get-in (first result)
                   [:synsem :infl])
           :present))))

(deftest parse-present-with-g
  (let [result (parse "nous mangeons")]
    (is (not (nil? result)))
    (is (= (get-in (first result)
                   [:synsem :sem :pred])
           :eat))
    (is (= (get-in (first result)
                   [:synsem :infl])
           :present))))

(deftest have-fun
  (let [have-fun-expression
        (generate
         {:synsem {:subcat '()
                   :sem {:subj {:pred :lui}
                         :pred :have-fun
                         :tense :present}}})]
    (is (not (empty? have-fun-expression)))
    (is (= (fo have-fun-expression)
           "il l'amuse"))))

(deftest se-plaindre
  (is (= (conjugate "se plaindre"
                    {:français {:present :regular}
                     :synsem {:cat :verb
                              :agr {:number :sing,
                                    :person :1st,
                                    :gender :masc},
                              :essere true,
                              :infl :present}})
         "plains")))

(deftest nous-acheterons
  (let [spec {:synsem {:sem {:tense :future}, :subcat ()},
              :root {:français {:français "acheter"}}, :comp {:synsem {:agr {:person :1st, :number :plur}}}}
        generated (babel.generate/generate spec (babel.francais.grammar/medium))]
    (is (= (not (nil? generated))))
    (is (= "nous acheterons" (fo generated)))))






