(ns menard.test.translate.basic
  (:require [menard.nederlands.basic :as basic]
            [menard.test.translate :refer [transfer-fn]]
            [clojure.test :refer [deftest]]))

(deftest transfer-basic
  (->>
   (range 0 (count nl/expressions))
   (map (fn [i]
          (println (str "transfering with nl/expression number: " i))
          (doall
           (take 10
                 (repeatedly #(transfer-fn i @basic/model))))))
   doall))
