(ns menard.test.nederlands
  (:require [menard.model :refer [load-model]]
            [menard.nederlands :as nl
             :refer [analyze expressions generate morph parse syntax-tree]]
            [menard.nederlands.basic :as basic]
            [menard.nederlands.complete :as complete]            
            [menard.nederlands.woordenlijst :as woordenlijst]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest adjective-agreement
  (is (= "het oude huis"
         (morph (generate {:cat :noun
                           :rule "np:2"
                           :subcat []
                           :root "huis"
                           :agr {:number :sing}
                           :max-depth 2
                           :sem {:quant :the
                                 :pred :house
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}})))))

