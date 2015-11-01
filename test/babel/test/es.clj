(ns babel.test.es
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.espanol.grammar :as esg]
            [babel.espanol.writer :as es]
            [babel.espanol.morphology :refer [fo]]
            [babel.writer :as writer]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [dag-unify.core :refer [get-in]]))

(deftest generate-conditional
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :sleep
                                                :subj {:pred :I}
                                                :tense :conditional}}}
                                esg/small
                                :enrich true)]
    (is (= :1st (get-in result [:comp :synsem :agr :person])))
    (is (= :sing (get-in result [:comp :synsem :agr :number])))
    (is (or (= "yo dormiría" (fo result))
            (= "dormiría" (fo result))))))

(deftest llamarse
  (let [result (es/expression {:synsem {:sem {:pred :be-called}}})]
    (not (empty? (fo result)))))

(deftest llamo
  (let [result (fo (es/expression {:synsem {:sem {:tense :present :aspect :progressive  :subj {:pred :I} :pred :be-called :obj {:pred :Juan}}}}))]
    (is (or (= result
               "yo me llamo Juan")
            (= result "me llamo Juan")))))




