(ns babel.test.fr
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.forest :as forest]
            [babel.francais.writer :as fr :refer [small lexicon]]
            [babel.francais.morphology :refer [fo]]
            [babel.over :refer [over]]
            [babel.writer :as writer]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [dag-unify.core :refer [fail-path fail? get-in strip-refs unifyc]]))

(deftest conditional
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :sleep
                                                :subj {:pred :I}
                                                :tense :conditional}}}
                                fr/small)]
    (is (= "je dormirais" (fo result)))))

(deftest present-irregular
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :be
                                                :subj {:pred :I}
                                                :tense :present}}}
                                fr/small)]
    (is (= "je suis" (fo result)))))

(deftest imperfect-irregular-être
  (let [result (engine/generate {:synsem {:subcat '()
                                          :infl :imperfect
                                          :sem {:pred :be
                                                :subj {:pred :I}}}}

                                fr/small)]
    (is (= "j'étais" (fo result)))))

(deftest imperfect-irregular-avoir
  (let [result (engine/generate {:synsem {:subcat '()
                                          :infl :imperfect
                                          :sem {:pred :avere
                                                :subj {:pred :I}}}}
                                fr/small)]
    (and (is (not (nil? result)))
         (is (= "av" (get-in result [:head :français :imperfect-stem])))
         (is (= "j'avais" (fo result))))))

(deftest être-as-aux
  (let [result
        (filter #(not (fail? %))
                (map (fn [rule]
                       (unifyc rule
                               {:head (last (get @lexicon "être"))}))
                     (:grammar @small)))]
    (and (is (not (empty? result)))
         (is (= (get-in (first result) [:rule]) "vp-aux")))))

(def vp-aux (first (filter #(= (:rule %) "vp-aux")
                           (:grammar @small))))

(def etre (first (filter #(= true (get-in % [:synsem :aux]))
                         (get @lexicon "être"))))
(deftest over-test
  (let [result
        (over (get @fr/small :grammar)
              (get @fr/lexicon "je")
              (over (get @fr/small :grammar)
                    (get @fr/lexicon "sommes") (get @fr/lexicon "aller")))]
    (and (not (nil? result))
         (not (empty? result))
         (= 1 (.size result)))))

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
                  :infl :passe-compose,
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

(deftest passe-compose-1
  (let [result
        (forest/generate
         {:synsem {:subcat '()
                   :sem {:tense :passe-compose
                         :subj {:pred :noi}
                         :pred :andare}}}
         (:grammar @fr/small)
         (:lexicon @fr/small)
         (:index @fr/small)
         (:morph @fr/small))]
    (and (is (not (nil? result)))
         (is (= (fo result) "nous sommes allées")))))

(deftest passe-compose
  (let [result (engine/generate {:synsem {:sem {:pred :andare
                                                :tense :passe-compose
                                                :subj {:pred :noi
                                                       :gender :fem}}}}
                                fr/small)]
    (and (is (not (nil? result)))
         (is (= (fo result) "nous sommes allées")))))

