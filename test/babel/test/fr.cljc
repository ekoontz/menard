(ns ^{:doc "French testing code"}
    babel.test.fr
  (:refer-clojure :exclude [get-in])
  (:require [babel.directory :refer [models]]
            [babel.francais :as fr :refer [generate medium morph parse]]
            [babel.francais.grammar :as grammar :refer [fo-ps]]
            [babel.francais.morphology :refer [analyze conjugate fo get-string]]
            [babel.generate :as generate]
            [babel.over :as over]
            [babel.parse :as parse]
            [clojure.repl :refer [doc]]
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
                    {:français {:past-p {:regular true}}
                     :synsem {:cat :verb
                              :infl :past-p
                              :agr {:number :sing}}})
         "amusé")))
         
(deftest conjugate3
  (is (= (conjugate "s'amuser"
                    {:français {:past-p {:regular true}}
                     :synsem {:cat :verb
                              :infl :past-p
                              :agr {:number :plur}}})
         "amusés")))

(deftest conjugate4
  (is (= (conjugate "s'amuser"
                    {:français {:past-p {:regular true}}
                     :synsem {:cat :verb
                              :infl :past-p
                              :agr {:gender :masc
                                    :number :plur}}})
         "amusés")))

(deftest conjugate5
  (is (= (conjugate "s'amuser"
                    {:français {:past-p {:regular true}}
                     :synsem {:cat :verb
                              :infl :past-p
                              :agr {:gender :fem
                                    :number :plur}}})
         "amusées")))

(deftest conjugate6
  (is (= (conjugate "se blesser"
                    {:français {:past-p {:regular true}}
                     :synsem {:cat :verb
                              :infl :past-p
                              :agr {:gender :masc
                                    :number :sing}}})
         "blessé")))

(deftest conjugate7
  (is (= (conjugate "se blesser"
                    {:français {:past-p {:regular true}}
                     :synsem {:cat :verb
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
                                   :cat :verb
                                   :sem {:pred :top
                                         :subj {:pred :I}
                                         :tense :present
                                         :aspect :simple}}
                          :root {:français {:français "parler"}}
                          :rule "s-present-nonphrasal"}
                         :model (small))]
    (is (= "je parle" (fo result)))))

(deftest generate-present-by-semantics-regular-1
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:pred :abandon
                                         :subj {:pred :I}
                                         :tense :present
                                         :aspect :simple}}}
                         :model (small))]
    (is (= "j'abandone" (fo result)))))

(deftest generate-present-by-root-regular-2
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:aspect :simple
                                         :tense :present}}
                          :root {:français {:français "abandoner"}}
                          :comp {:synsem {:agr {:person :1st, :number :sing}}}}
                         :model (small))]
    (is (= "j'abandone" (fo result)))))

(deftest generate-present-by-semantics-irregular-1
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:pred :sleep
                                         :subj {:pred :I}
                                         :tense :present
                                         :aspect :simple}}}
                         :model (small))]
    (is (= "je dors" (fo result)))))

(deftest generate-present-by-root-irregular-1
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:aspect :simple
                                         :tense :present}}
                          :root {:français {:français "dormir"}}
                          :comp {:synsem {:agr {:person :1st, :number :sing}}}}
                         :model (small))]
    (is (= "je dors" (fo result)))))

(deftest generate-present-by-root-with-boot-stem
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:subj {:pred :noi}
                                         :tense :present
                                         :aspect :simple}}
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
                                   :cat :verb
                                   :sem {:subj {:pred :I}
                                         :tense :conditional}}
                          :root {:français {:français "dormir"}}}
                         :model (small))]
    (is (= "je dormirais" (fo result)))))

(deftest generate-future-with-irregular-future-stem-1
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:pred :call
                                         :subj {:pred :tu}
                                         :tense :future}}}
                         :model (medium))]
    (is (= "tu appelas" (fo result)))))

(deftest generate-future-with-irregular-future-stem-2
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:subj {:pred :tu}
                                         :tense :future}}
                          :root {:français {:français "appeler"}}}
                         :model (medium))]
    (is (= "tu appelas" (fo result)))))

(deftest generate-regular-conditional-reflexive
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:subj {:pred :I}
                                         :tense :conditional}}
                          :root {:français {:français "s'amuser"}}})]
    (is (= "je m'amuserais" (fo result)))))

(deftest generate-present-irregular-2
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:pred :be
                                         :subj {:pred :I}
                                         :tense :present
                                         :aspect :simple}}}
                         :model (small))]
    (is (= "je suis" (fo result)))))

(deftest generate-imparfait-regular-er
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:pred :study
                                         :subj {:pred :I}
                                         :tense :past
                                         :aspect :progressive}}}
                         :model (small))]
    (is (= :imperfect (get-in result [:synsem :infl])))
    (is (= "j'étudiais" (fo result)))))

(deftest imparfait-insert-e-before-g
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:subj {:pred :I}
                                         :tense :past
                                         :aspect :progressive}}
                          :root {:français {:français "manger"}}})]
    (is (= "je mangeais" (fo result)))))

(deftest imparfait-insert-cedilla-before-c
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:subj {:pred :loro
                                                :gender :fem}
                                         :tense :past
                                         :aspect :progressive}}
                          :root {:français {:français "commencer"}}})]
    (is (= "elles commençaient" (fo result)))))

(deftest generate-imparfait-regular-ir
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :agr {:gender :masc}
                                   :sem {:pred :sleep
                                         :subj {:pred :loro
                                                :gender :masc}
                                         :tense :past
                                         :aspect :progressive}}}
                         :model (small))]
    (is (= :imperfect (get-in result [:synsem :infl])))
    (is (= "ils dormaient" (fo result)))))

(deftest generate-imparfait-regular-re
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:pred :sell
                                         :subj {:pred :voi}
                                         :tense :past
                                         :aspect :progressive}}}
                         :model (small))]
    (is (= :imperfect (get-in result [:synsem :infl])))
    (is (= "vous vendiez" (fo result)))))

(deftest generate-imparfait-finir
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:subj {:pred :noi}
                                         :tense :past
                                         :aspect :progressive}}
                          :root {:français {:français "finir"}}}
                         :model (small))]
    (is (= "nous finissions" (fo result)))))

(deftest generate-imperfect-irregular-être
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :infl :imperfect
                                   :sem {:pred :be
                                         :subj {:pred :I}}}}
                         :model (small))]
    (is (= "j'étais" (fo result)))))

(deftest generate-imperfect-irregular-avoir
  (let [result (generate/generate {:synsem {:subcat '()
                                            :cat :verb
                                            :infl :imperfect
                                            :sem {:pred :have
                                                  :subj {:pred :I}}}}
                                  (small))]
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
                  :past-p {:regular true}
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

(deftest generate-passe-compose
  (let [result (generate {:synsem {:cat :verb
                                   :subcat '()
                                   :sem {:pred :go
                                         :subj {:pred :noi
                                                :gender :fem}
                                         :aspect :perfect
                                         :tense :present}}})]
    (is (not (nil? result)))
    (is (= (fo result) "nous sommes allées"))))

(deftest generate-passe-compose-essere-false
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:subj {:pred :tu
                                                :gender :fem}
                                         :aspect :perfect
                                         :tense :present}}
                          :root {:français {:français "couper"}}})]
    (is (not (nil? result)))
    (is (= (fo result) "tu as coupé"))))

(deftest generate-passe-compose-irregular
  (is (not (empty? (get (:lexicon (small)) "pris"))))
  (is (= false (get-in (first (get (:lexicon (small)) "prendre"))
                       [:français :past-p :regular])))
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:subj {:pred :noi}
                                         :aspect :perfect
                                         :tense :present}}
                          :root {:français {:français "prendre"}}}
                         :model (small))]

    (is (not (nil? result)))
    (is (= (fo result) "nous avons pris"))))

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
                {:synsem {:cat :verb
                          :subcat '()
                          :sem {:pred :have-fun}}})]
    (is (= (get-in result [:synsem :sem :pred]) :have-fun))))

(deftest generate-vp-aux-reflexive
  (let [result
        (generate
         {:synsem {:cat :verb
                   :subcat '()
                   :sem {:subj {:pred :lei}
                         :pred :have-fun
                         :tense :present
                         :aspect :perfect}}})]
    (is (= (fo result) "elle l'est amusée"))))

(deftest generate-vp-present-reflexive
  (let [result
        (generate
         {:synsem {:cat :verb
                   :subcat '() :sem {:subj {:pred :lei}
                                     :pred :have-fun
                                     :aspect :simple
                                     :tense :present}}})]
  (is (= (fo result) "elle l'amuse"))))

(deftest generate-named-sentence
  (let [result
        (generate
         {:synsem {:cat :verb
                   :subcat '()
                   :sem {:pred :be-called
                         :aspect :simple
                         :tense :present
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
           :present))
    (is (= (get-in (first result) [:synsem :sem :aspect])
           :perfect))))

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
                   :cat :verb
                   :sem {:subj {:pred :lui}
                         :pred :have-fun
                         :tense :present
                         :aspect :simple}}})]
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


(def standard-number-of-times 10)
(defn speed-test [spec & [times]]
  (let [times (or times standard-number-of-times)]
    (is (= times
           (count (take times
                        (repeatedly
                         #(time (println (fo (generate spec :model (medium))))))))))))
(deftest speed-test-1
  ;; these should all take relatively the same time, but
  ;; for now, the more general the spec, the longer it takes to generate.
  (log/info (str "speed-test-1"))
  (speed-test {:synsem {:cat :verb
                        :subcat '()
                        :sem {:pred :speak
                              :subj {:pred :noi}
                              :tense :past
                              :aspect :progressive}}}))
(deftest speed-test-2
  (log/info (str "speed-test-2"))
  (speed-test {:synsem {:cat :verb
                        :subcat '()
                        :sem {:pred :speak
                              :subj {:pred :noi}
                              :tense :conditional}}}))
(deftest speed-test-3
  (log/info (str "speed-test-3"))
  (speed-test {:synsem {:cat :verb
                        :subcat '()
                        :sem {:pred :speak
                              :tense :conditional}}}))
(deftest speed-test-4
  (log/info (str "speed-test-4"))
  (speed-test {:synsem {:cat :verb
                        :subcat '()
                        :sem {:pred :speak}}}))
(deftest speed-test-5
  (log/info (str "speed-test-5"))
  (speed-test {:synsem {:cat :verb
                        :subcat '()
                        :sem {:tense :conditional}}}))
(deftest speed-test-6
  (log/info (str "speed-test-6"))
  (speed-test {:synsem {:cat :verb
                        :subcat '()
                        :sem {:pred :speak
                              :subj {:pred :noi}
                              :tense :present
                              :aspect :perfect}}}))
(def spec-7  {:synsem {:cat :verb
                        :subcat '()
                        :sem {:pred :speak
                              :tense :present
                              :aspect :perfect}}})
(deftest speed-test-7
  (log/info (str "speed-test-7"))
  (speed-test spec-7))

(deftest speed-test-8
  (log/info (str "speed-test-8"))
  (speed-test {:synsem {:cat :verb
                        :subcat '()
                        :sem {:tense :present
                              :reflexive false
                              :aspect :perfect}}}))
(deftest speed-test-9
  (log/info (str "speed-test-9"))
  (speed-test {:synsem {:cat :verb
                        :subcat '()}}))

(defn add-heads [rule & [spec]]
  (let [grammar (grammar/medium)]
    (remove #(= :fail %)
            (map (fn [lexeme]
                   (dag_unify.core/assoc-in
                    (get-in grammar
                            [:grammar-map rule]) [:head] lexeme))
                 (flatten (vals (get-in grammar [:lexicon])))))))

(defn heads [rule]
  (map morph (add-heads rule)))

(defn add-comps [rule]
  (if rule
    (let [grammar (grammar/medium)
          rule (cond (keyword? rule)
                     (get-in grammar [:grammar-map rule])
                     (map? rule)
                     rule
                     true (throw (Exception. (str ": rule must be either a keyword or a map, not: " (type rule)))))]
      (remove #(= :fail %)
              (map (fn [lexeme]
                     (do
                       (println "assoc-in candidate comp lexeme: " (morph lexeme))
                       (dag_unify.core/assoc-in
                        rule
                      [:comp] lexeme)))
                   (flatten (vals (get-in grammar [:lexicon]))))))))
    
(defn comps [rule]
  (map morph (add-comps rule)))

(def spec-10
  {:synsem {:subcat '()
            :cat :verb
            :sem {:tense :present
                  :reflexive true
                  :aspect :perfect}}})

(deftest speed-test-10
  (log/info (str "speed-test-10"))
  (speed-test spec-10))

