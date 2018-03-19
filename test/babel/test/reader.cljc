(ns babel.test.reader
  (:require [babel.reader :as reader]
            [babel.test.test :as btest]
            #?(:clj [clojure.test :as realtest :refer [deftest is]])
            #?(:clj [clojure.test :as realtest :refer [deftest is]])))

(btest/init-db)

(deftest chiamarsi
  (let [generated
        (reader/generate-question-and-correct-set 
         {:root {:italiano {:italiano "chiamarsi"}}
          :synsem {:sem {:tense :present
                         :aspect :simple}}
          :comp {:synsem {:agr {:number :plur
                                :person :3rd}}}}
         "en"
         "US"
         "it"
         "IT"
         (fn [x] true))]
    (is (map? generated))
    (is (not (nil? (:source generated))))))



