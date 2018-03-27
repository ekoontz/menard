(ns babel.test.reader
  (:require [babel.reader :as reader]
            [babel.test.test :as btest]
            [clojure.pprint :refer [pprint]]
            [dag_unify.core :as unify]
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

         (fn [lexeme]
           (or (and (= :verb (unify/get-in lexeme
                                           [:synsem :cat]))
                    (or (= [] (unify/get-in lexeme
                                            [:synsem :subcat :2]))
                        (= true (unify/get-in lexeme [:synsem :aux]))
                        (= true (unify/get-in lexeme [:synsem :sem :reflexive]))))
               (and (= :noun (unify/get-in lexeme
                                           [:synsem :cat]))
                    (= false (unify/get-in lexeme
                                           [:synsem :sem :city] false))))))]
         
    (is (map? generated))
    (is (not (nil? (:source generated))))))



