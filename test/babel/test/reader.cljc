(ns babel.test.reader
  (:require [babel.reader :as reader]
            [babel.test.test :as btest]
            [clojure.pprint :refer [pprint]]
            [dag_unify.core :as u]
            #?(:clj [clojure.test :as realtest :refer [deftest is]])
            #?(:clj [clojure.test :as realtest :refer [deftest is]])))

(btest/init-db)

(deftest chiamarsi
  (let [generated
        (binding [reader/target-lexical-filter
                    #(or (= :verb (u/get-in % [:synsem :cat]))
                         (and (= :noun (u/get-in % [:synsem :cat]))
                              (not (= :acc (u/get-in % [:synsem :case]))))
                         (and (= :noun (u/get-in % [:synsem :cat]))
                              (= :acc (u/get-in % [:synsem :case]))
                              (= true (u/get-in % [:synsem :reflexive]))))]
          (reader/generate-question-and-correct-set 
            {:root {:italiano {:italiano "chiamarsi"}}
             :synsem {:sem {:tense :present
                            :aspect :simple}}
             :comp {:synsem {:agr {:number :plur
                                   :person :3rd}}}}
            "en"
            "US"
            "it"
            "IT"))]
    (is (map? generated))
    (is (not (nil? (:source generated))))
    (is (not (empty? (:targets generated))))))



