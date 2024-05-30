(ns menard.test.español
  (:require [menard.model :refer [load-model]]
            [menard.nederlands :as nl
             :refer [analyze expressions generate morph
                     parse syntax-tree]]
            [menard.español.complete :as complete]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest subject-agreement
  (is (= "yo quiero"
         (morph (generate {:cat :noun
                           :rule "np:2"
                           :subcat []
                           :root "querer"
                           :agr {:number :sing}
                           :max-depth 2
                           :sem {:pred :want
                                 :subj {:first {:pred :I}}}}
                          complete/model)))))



