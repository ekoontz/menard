(ns babel.test.fr
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.english :as en]
            [babel.francais :as es]
            [babel.francais.morphology :refer [fo]]
            [babel.writer :as writer]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [dag-unify.core :refer [get-in]]))


(deftest generate-conditional
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :dormire
                                                :subj {:pred :io}
                                                :tense :conditional}}}
                                es/small
                                :enrich true)]
    (is (= :1st (get-in result [:comp :synsem :agr :person])))
    (is (= :sing (get-in result [:comp :synsem :agr :number])))
    (is (= "je dormirais" (fo result)))))





