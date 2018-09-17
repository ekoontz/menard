(ns babel.test.reader
  (:require [babel.reader :as reader]
            [babel.test.test :as btest]
            [clojure.pprint :refer [pprint]]
            [dag_unify.core :as u]
            #?(:clj [clojure.test :as realtest :refer [deftest is]])
            #?(:clj [clojure.test :as realtest :refer [deftest is]])))

(btest/init-db)

(deftest chiamarsi
  (let [target-grammar-rules
        #{"sentence-phrasal-head"
          "vp-32"
          "vp-pronoun-phrasal"}
        source-grammar-rules
        #{"sentence-phrasal-head"
          "transitive-vp-nonphrasal-head"
          "transitive-vp-phrasal-head"
          "noun-phrase"}
        
        generated
        (binding [babel.reader/target-grammar-subset target-grammar-rules
                  babel.reader/source-grammar-subset source-grammar-rules]
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



