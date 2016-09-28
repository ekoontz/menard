(ns babel.test.translate
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.test.it :as it]
   [babel.test.en :as en]
   [clojure.test :refer [deftest is]]
   [dag_unify.core :refer [get-in strip-refs]]))

;; test that gender is correctly translated
(deftest past-and-gender-agreement
  (let [semantics (strip-refs (get-in (first (:parses (first (babel.italiano/parse "loro sono andate"))))
                                      [:synsem :sem]))
        english-translation (babel.english/generate {:synsem {:sem semantics}}
                                                    :model en/small)]
    (= "they (♀) went" (babel.english.morphology/fo english-translation))))



