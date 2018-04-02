(ns babel.test.test
  (:require
   [babel.generate :as generate]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [clojure.tools.logging :as log]))

(defn init-db []
  (->
   "postgres://localhost/babel"
   babel.korma/init-db 
   babel.korma/create-tables))

(defn generate-speed-test [spec model & [times]]
  "generate an expression according to the given _spec_ and _model_."
  (let [standard-number-of-times 10
        times (or times standard-number-of-times)]
    (is (= times
           (count (take times
                        (repeatedly
                         #(let [expr ((:morph-ps model) (time ((:generate-fn model) spec)))]
                            (println expr)))))))))

(defn speed-test
  "generate an expression and then parse that expression, both according to the given _spec_ and _model_."
  [spec model & [times]]
  (let [standard-number-of-times 10
        times (or times standard-number-of-times)
        fo (:morph model)
        generate (:generate-fn model)
        parse (:parse model)]
    (is
     (= times
        (count
         (take times
               (repeatedly
                #(let [expr (fo (generate spec :model model))]
                   (println expr)
                   (println
                    (string/join ","
                                 (map (:morph-ps model)
                                      (take 1
                                            (time (parse expr
                                                         :model model
                                                         :parse-with-truncate false))))))))))))))
(defn sentences [n model spec]
  (doall
   (take n
         (repeatedly #(let [expr (generate/generate
                                  spec
                                  model)]
                        (time (println ((:morph model) expr))))))))





