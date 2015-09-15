(ns babel.test.it
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :refer [generate]]
            [babel.italiano.grammar :refer [small]]
            [babel.italiano.lexicon :refer [lexicon]]
            [babel.italiano.morphology :refer [fo]]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [dag-unify.core :refer [get-in]]))

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
    (and (is (not (nil? result)))
         (is (= "io ho bevuto" (fo result))))))

(deftest passato-prossimo-reflexive
  (let [result (generate {:head {:synsem {:agr {:gender :fem}}}
                          :synsem {:subcat '()
                                   :infl :present
                                   :sem {:pred :get-up
                                         :subj {:pred :I}
                                         :tense :past
                                         :aspect :perfect}}}
                         small)]
    (and (is (not (nil? result)))
         (is (= "io mi sono alzata" (fo result))))))
