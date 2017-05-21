(ns babel.test.es
  (:refer-clojure :exclude [get-in])
  (:require [babel.directory :refer [models]]
            [babel.espanol :as espanol :refer [analyze generate parse small]]
            [babel.espanol.grammar :as grammar]
            [babel.espanol.morphology :refer [fo]]
            [clojure.repl :refer [doc]]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [get-in]]))

(deftest generate-regular-conditional
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :sleep
                                         :subj {:pred :I}
                                         :tense :conditional}}}
                         :model (small)
                         :truncate-children false)]
    (is (= :1st (get-in result [:comp :synsem :agr :person])))
    (is (= :sing (get-in result [:comp :synsem :agr :number])))
    (is (or (= "yo dormiría" (fo result))
            (= "dormiría" (fo result))))))

(deftest generate-irregular-future
  (let [result (fo (generate {:synsem {:sem {:tense :future
                                             :subj {:pred :I}}}
                              :root {:espanol {:espanol "venir"}}}
                             :model (small)))]
    (is (or (= result
               "yo vendré")
            (= result
               "vendré")))))

(deftest zar-preterito
  (let [result (generate 
                {:root {:espanol {:espanol "abrazar"}}
                 :synsem {:sem {:subj {:pred :I}}
                          :infl :preterito}}
                :model (small)
                :truncate-children false)]
    (is (or (= "yo abracé" (fo result))
            (= "abracé" (fo result))))))
                
(deftest llamarse
  (let [result (generate {:synsem {:sem {:pred :be-called}}} :model (small))]
    (is (not (empty? (fo result))))))

(deftest llamo
  (let [result (fo (generate {:synsem {:subcat '()
                                       :sem {:tense :present
                                             :aspect :simple
                                             :subj {:pred :I}
                                             :pred :be-called
                                             :obj {:pred :Juan}}}}
                             :model (small)))]
    (is (or (= result
               "yo me llamo Juan")
            (= result "me llamo Juan")))))

    
