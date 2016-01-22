(ns ^{:doc "French Testing Code"}
    babel.test.fr
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.forest :as forest]
            [babel.francais.grammar :refer [small medium]]
            [babel.francais.lexicon :refer [lexicon]]
            [babel.francais.morphology :refer [analyze conjugate fo get-string
                                               possible-lexemes replace-patterns]]
            [babel.francais.workbook :refer [generate lookup parse tokenize]]
            [babel.over :as over]
            [babel.parse :as parse]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [fail-path fail? get-in strip-refs unifyc]]))

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
   (over/over (vals (:grammar-map medium)) (lookup arg1)))
  ([grammar arg1]
   (over/over grammar (lookup arg1)))
  ([grammar arg1 arg2]
   (cond (string? arg1)
         (over grammar (lookup arg1)
               arg2)

         (string? arg2)
         (over grammar arg1 (lookup arg2))

         true
         (over/over grammar arg1 arg2))))

(deftest generate-conditional
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :sleep
                                                :subj {:pred :I}
                                                :tense :conditional}}}
                                small)]
    (is (= "je dormirais" (fo result)))))

(deftest generate-present-irregular
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :be
                                                :subj {:pred :I}
                                                :tense :present}}}
                                small)]
    (is (= "je suis" (fo result)))))

(deftest generate-imperfect-irregular-être
  (let [result (engine/generate {:synsem {:subcat '()
                                          :infl :imperfect
                                          :sem {:pred :be
                                                :subj {:pred :I}}}}

                                small)]
    (is (= "j'étais" (fo result)))))

(deftest generate-imperfect-irregular-avoir
  (let [result (engine/generate {:synsem {:subcat '()
                                          :infl :imperfect
                                          :sem {:pred :have
                                                :subj {:pred :I}}}}
                                small)]
    (is (not (nil? result)))
    (is (= "av" (get-in result [:head :français :imperfect-stem])))
    (is (= "j'avais" (fo result)))))

(deftest être-as-aux
  (let [lexicon (:lexicon small)
        result
        (filter #(not (fail? %))
                (map (fn [rule]
                       (unifyc rule
                               {:head (last (get lexicon "être"))}))
                     (:grammar small)))]
    (is (not (empty? result)))
    (is (= (get-in (first result) [:rule]) "vp-aux"))))

(deftest vp-aux-test
  (let [rule (first (filter #(= (:rule %) "vp-aux")
                            (:grammar small)))]
    (is (not (nil? rule)))))

(def etre-test
  (is (not (nil? (first (filter #(= true (get-in % [:synsem :aux]))
                                (get (:lexicon small) "être")))))))

(deftest over-test
  (let [lexicon (:lexicon small)
        grammar (:grammar small)
        result
        (over grammar
              (get lexicon "nous")
              (over grammar
                    (get lexicon "sommes") (get lexicon "aller")))]
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
              :français "nous"},
          :b {:b {:future-stem "ir",
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
              :a {:infl :present,
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
        (forest/generate (unifyc
                          {:synsem {:subcat '()}}
                          {:synsem {:sem {:subj {:pred :noi
                                                 :gender :fem}
                                          :pred :go
                                          :aspect :perfect
                                          :tense :past}}})
         (:grammar small)
         (:lexicon small)
         (:index small)
         (:morph small))]
    (and (is (not (nil? result)))
         (is (= (fo result) "nous sommes allées")))))

(deftest generate-passe-compose
  (let [result (engine/generate {:synsem {:sem {:pred :go
                                                :subj {:pred :noi
                                                       :gender :fem}
                                                :aspect :perfect
                                                :tense :past}}}
                                small)]
    (is (not (nil? result)))
    (is (= (fo result) "nous sommes allées"))))

(deftest generate-reflexive-present
  (let [rules {:s-present-phrasal
               (first (filter #(= (get % :rule) "s-present-phrasal")
                              (:grammar medium)))
               :vp-pronoun-nonphrasal
               (first (filter #(= (get % :rule) "vp-pronoun-nonphrasal")
                              (:grammar medium)))}

        result (over (get rules :s-present-phrasal)
                     "je" 
                     (over (get rules :vp-pronoun-nonphrasal)
                           "me" "s'amuser"))]
    (is (= (fo (first result))
           "je m'amuse"))))

(deftest generate-have-fun-sentence
  (let [result (engine/expression medium
                                  {:synsem {:sem {:pred :have-fun}}})]
    (is (= (get-in result [:synsem :sem :pred]) :have-fun))))

(deftest generate-vp-aux-reflexive
  (let [result
        (engine/expression
         medium
         {:synsem {:subcat '() :sem {:subj {:pred :lei}
                                     :pred :have-fun :tense :past}}})]
    (is (= (fo result) "elle l'est amusée"))))

(deftest generate-named-sentence
  (let [result (engine/expression medium
                                  {:synsem {:sem {:pred :be-called
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

(deftest conjugate1
  (let [from #"s'([aeéiou].*)er$"
        infinitive "s'amuser"
        to "$1é"]
    (is (= "amusé" (string/replace infinitive from to)))))

(deftest conjugate2
  (is (= "amusé" (conjugate "s'amuser"
                            {:synsem {:infl :past-p
                                      :subcat {:1 {:agr {:number :sing}}}}}))))
(deftest conjugate3
  (is (= (conjugate "s'amuser"
                    {:synsem {:infl :past-p
                              :subcat {:1 {:agr {:number :plur}}}}})
         "amusés")))

(deftest conjugate4
  (is (= (conjugate "s'amuser"
                    {:synsem {:infl :past-p
                              :subcat {:1 {:agr {:gender :masc
                                                 :number :plur}}}}})
         "amusés")))

(deftest conjugate5
  (is (= (conjugate "s'amuser"
                    {:synsem {:infl :past-p
                              :subcat {:1 {:agr {:gender :fem
                                                 :number :plur}}}}})
         "amusées")))

(deftest conjugate6
  (is (= (conjugate "se blesser"
                    {:synsem {:infl :past-p
                              :subcat {:1 {:agr {:gender :masc
                                                 :number :sing}}}}})
         "blessé")))

(deftest conjugate7
  (is (= (conjugate "se blesser"
                    {:synsem {:infl :past-p
                              :subcat {:1 {:agr {:gender :fem
                                                 :number :plur}}}}})
         "blessées")))

(deftest conjugate7
  (is (= (conjugate "se blesser"
                    {:synsem {:infl :present
                              :subcat {:1 {:agr {:person :2nd
                                                 :number :sing}}}}})
         "blesses")))

(defn get-lex [exp]
  (filter #(not (nil? (:lookup %)))
          (map #(let [from (first %)
                      to (second %)
                      unify-with (nth % 2)
                      lex (string/replace exp from to)]
                  {:lex lex
                   :lookup (lookup lex)
                   :unified (map (fn [entry]
                                   (unifyc unify-with entry))
                                 (lookup lex))})
               replace-patterns)))

(defn get-lex2 [exp]
  (filter (fn [result] (not (= :fail result)))
          (mapcat #(let [from (first %)
                         to (second %)
                         unify-with (nth % 2)
                         lex (string/replace exp from to)]
                     (map (fn [entry]
                            (unifyc unify-with entry))
                          (lookup lex)))
                  replace-patterns)))
