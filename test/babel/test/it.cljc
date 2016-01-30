(ns babel.test.it
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :refer [generate]]
            [babel.italiano.grammar :refer [small medium]]
            [babel.italiano.lexicon :refer [lexicon]]
            [babel.italiano.morphology :refer [fo]]
            [babel.italiano.workbook :refer [analyze parse]]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [get-in strip-refs]]))

(deftest analyze-1
  (let [singular (analyze "compito" (:lookup medium))
        plural  (analyze "compiti" (:lookup medium))]
    (is (not (nil? singular)))
    (is (not (nil? plural)))))

(deftest analyze-2
  (let [singular (analyze "difficile" (:lookup medium))
        plural  (analyze "difficili" (:lookup medium))]
    (is (not (nil? singular)))
    (is (not (nil? plural)))))

(deftest present-irregular
  (let [result (generate {:synsem {:subcat '()
                                   :sem {:pred :be
                                         :subj {:pred :I}
                                         :tense :present}}}
                         small)]
    (is (= "io sono" (fo result)))))

(deftest passato-prossimo
  (let [result (generate {:root {:italiano {:italiano "bere"}}
                          :synsem {:subcat ()
                                   :sem {:subj {:pred :I}
                                         :tense :past
                                         :aspect :perfect}}}
                         small)]
    (is (not (nil? result)))
    (is (= "io ho bevuto" (fo result)))))

(deftest passato-prossimo-reflexive
  (let [result (generate {:head {:synsem {:agr {:gender :fem}}}
                          :synsem {:subcat '()
                                   :infl :present
                                   :sem {:pred :get-up
                                         :subj {:pred :I}
                                         :tense :past
                                         :aspect :perfect}}}
                         small)]
    (is (not (nil? result)))
    (is (= "io mi sono alzata" (fo result)))))

(deftest parse-io-parlo
  (let [result (parse "io parlo")]
    (is (not (empty? result)))
    (is (= "io parlo") (fo (first result)))))

        
