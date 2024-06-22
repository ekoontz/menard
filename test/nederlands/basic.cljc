(ns menard.test.nederlands.basic
  (:require [menard.model :refer [load-model]]
            [menard.nederlands.basic :as basic
             :refer [generate morph syntax-tree]]
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
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}))))

    (is (= "een oud huis"
         (morph (generate {:cat :noun
                           :rule "np:2"
                           :subcat []
                           :max-depth 2                             
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :some
                                 :mod {:first {:pred :old
                                                 :mod []}
                                       :rest []}}}))))

  (is (= "de oude huizen"
         (morph (generate {:cat :noun
                           :rule "np:2"
                           :subcat []
                           :max-depth 2                             
                             :root "huis"
                           :agr {:number :plur}
                           :sem {:quant :the
                                 :mod {:first {:pred :old
                                               :number? false
                                               :mod []}
                                       :rest []}}})))))

