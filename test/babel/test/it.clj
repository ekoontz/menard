(ns babel.test.it
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.italiano :as it :refer [lexicon]]
            [babel.italiano.lexicon :refer [lexicon-source]]
            [babel.italiano.morphology :refer [fo]]
            [babel.writer :as writer]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [dag-unify.core :refer [get-in]]))

(deftest present-irregular
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :be
                                                :subj {:pred :I}
                                                :tense :present}}}
                                it/small)]
    (is (= "io sono" (fo result)))))

(deftest passato-prossimo
  (let [result (engine/generate {:root {:italiano {:italiano "bere"}}
                                 :synsem {:subcat ()
                                          :sem {:subj {:pred :I}
                                                :tense :past
                                                :aspect :perfect}}}
                                it/small)]
    (and (is (not (nil? result)))
         (is (= "io ho bevuto" (fo result))))))

(deftest passato-prossimo-reflexive
  (let [result (engine/generate {:head {:synsem {:agr {:gender :fem}}}
                                 :synsem {:subcat '()
                                          :infl :present
                                          :sem {:pred :get-up
                                                :subj {:pred :I}
                                                :tense :past
                                                :aspect :perfect}}}
                                it/small)]
    (and (is (not (nil? result)))
         (is (= "io mi sono alzata" (fo result))))))



