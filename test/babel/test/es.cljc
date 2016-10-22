(ns babel.test.es
  (:refer-clojure :exclude [get-in])
  (:require [babel.directory :refer [models]]
            [babel.engine :as engine]
            [babel.espanol :refer [analyze generate small medium parse]]
            [babel.espanol.grammar :as grammar]
            [babel.espanol.morphology :refer [fo]]
            [clojure.repl :refer [doc]]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [get-in]]))

(deftest generate-conditional
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :sleep
                                         :subj {:pred :I}
                                         :tense :conditional}}}
                         (small)
                         {:truncate-children false})]
    (is (= :1st (get-in result [:comp :synsem :agr :person])))
    (is (= :sing (get-in result [:comp :synsem :agr :number])))
    (is (or (= "yo dormiría" (fo result))
            (= "dormiría" (fo result))))))

(deftest zar-preterito
  (let [result (generate 
                {:root {:espanol {:espanol "abrazar"}}
                 :synsem {:sem {:subj {:pred :I}}
                          :infl :preterito}}
                (small)
                {:truncate-children false})]
    (is (or (= "yo abracé" (fo result))
            (= "abracé" (fo result))))))
                
(deftest llamarse
  (let [result (engine/expression (small) {:synsem {:sem {:pred :be-called}}})]
    (is (not (empty? (fo result))))))

(deftest llamo
  (let [result (fo (engine/expression (small)
                                      {:synsem {:sem {:tense :present
                                                      :aspect :progressive
                                                      :subj {:pred :I}
                                                      :pred :be-called
                                                      :obj {:pred :Juan}}}}))]
    (is (or (= result
               "yo me llamo Juan")
            (= result "me llamo Juan")))))
