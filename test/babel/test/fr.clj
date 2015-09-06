(ns babel.test.fr
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.francais :as fr :refer [lexicon]]
            [babel.francais.morphology :refer [fo]]
            [babel.writer :as writer]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [dag-unify.core :refer [get-in]]))

(deftest conditional
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :sleep
                                                :subj {:pred :I}
                                                :tense :conditional}}}
                                fr/small
                                :enrich true)]
    (is (= :1st (get-in result [:comp :synsem :agr :person])))
    (is (= :sing (get-in result [:comp :synsem :agr :number])))
    (is (= "je dormirais" (fo result)))))

(deftest present-irregular
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :be
                                                :subj {:pred :I}
                                                :tense :present}}}
                                fr/small)]
    (is (= "je suis" (fo result)))))

(deftest imperfect-irregular
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :be
                                                :subj {:pred :I}
                                                :tense :imperfect}}}
                                fr/small)]
    (is (= "j'étais" (fo result)))))





