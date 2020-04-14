(ns babylon.test.nederlands
  (:require [babylon.nederlands :as nl :refer [analyze morph syntax-tree]]
            [babylon.lexiconfn :as l]
            [babylon.parse :as p]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; exclude non-production expressions:
(def expressions
  (->> nl/expressions
       (filter #(= true (u/get-in % [:prod?] true)))))

;; exclude non-production grammar rules:
(def grammar
  (->> nl/grammar
       (filter #(= true (u/get-in % [:prod?] true)))))


;; generate with the above resources:
(defn generate [spec]
  (binding [nl/grammar-for-generation grammar]
    (nl/generate spec)))

;; parse with the above resources:
(defn parse [expression]
  (binding [p/grammar grammar
            p/syntax-tree syntax-tree
            l/lexicon nl/lexicon
            l/morphology nl/morphology
            p/lookup-fn l/matching-lexemes]
    (p/parse expression morph)))

(deftest adjective-agreement
  (is (= "het oude huis"
         (morph (generate {:rule "np"
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :def
                                 :mod {:first {:pred :old}}}
                           :head {:rule "nbar"}}))))
  (is (= "een oud huis"
         (morph (generate {:rule "np"
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :indef
                                 :mod {:first {:pred :old}}}
                           :head {:rule "nbar"}}))))
  (is (= "de oude huizen"
         (morph (generate {:rule "np"
                           :root "huis"
                           :agr {:number :plur}
                           :sem {:quant :def
                                 :mod {:first {:pred :old}}}
                           :head {:rule "nbar"}})))))

(deftest all-expressions-work
  (let [generate-per-expression 5
        expression-sets
        (->>
         (range 0 (count expressions))
         (pmap (fn [index]
                 (take generate-per-expression
                       (repeatedly #(generate (nth expressions index)))))))]
    (is (or true (= (count expressions)
                    (count expression-sets))))
    (is (empty?
         (->> expression-sets
              (pmap (fn [expression-set]
                      (->> expression-set
                           (pmap (fn [expression]
                                   (log/info (str (morph expression)))
                                   (parse (morph expression)))))))
              (map count)
              (remove #(= % generate-per-expression)))))))
