(ns menard.test.crossproduct
  (:require [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            [clojure.test :refer [deftest is]]
            [menard.crossproduct :refer [cross-product]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest cp-1
  ;; can unify the whole thing into one map:
  (is (= (cross-product [{:a 4}{:b 5}{:c 6}{:d 7}{:e 8}])
         #{{:a 4, :d 7, :b 5, :c 6, :e 8}}))

  ;; can't unify anything, so keep as the same maps:
  (is (= (cross-product [{:a 4}{:a 5}{:a 6}{:a 7}{:a 8}])
         #{{:a 4} {:a 5} {:a 6} {:a 7} {:a 8}}))

  ;; some maps can unify, some can't; the result is 2 sets:
  (is (= (-> [{:a 42}{:b 43}{:c 44}{:a 45}]
              shuffle
              cross-product)
         #{{:a 42 :b 43 :c 44}
           {:a 45 :b 43 :c 44}}))

  ;; some maps (that is, {:a 1}) need to be cleaned up:
  (is (= (-> [{:a 1}{:a 2}{:a 3}{:a 4}{:a 1 :b 43}] shuffle cross-product)
         #{{:a 1
            :b 43}
           {:a 2}
           {:a 3}
           {:a 4}})))
