(ns menard.test.español
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def spec {:cat :verb
           :rule "s"
           :subcat []
           :root "querer"
           :sem {:pred :want
                 :subj {:pred :i}}})

(def spec2 {:cat :verb
            :rule "s"
            :subcat []
            :sem {:pred :want}})

(deftest subject-agreement
  (count
   (take 10
         (repeatedly #(is (= "yo quiero"
                             (-> {:cat :verb
                                  :rule "s"
                                  :subcat []
                                  :root "querer"
                                  :sem {:pred :want
                                        :subj {:pred :i}}}
                                 generate
                                 ((fn [x]
                                    (log/info (str "syntax-tree: " (syntax-tree x)))
                                    x))
                                 morph
                                 ((fn [x]
                                    (log/info (str "morph: " x))
                                    x))))))))
  (count
   (take 10
         (repeatedly #(is (= "ellas quieren"
                             (-> {:cat :verb
                                  :rule "s"
                                  :subcat []
                                  :root "querer"
                                  :agr {:number :plur
                                        :gender :fem}
                                  :sem {:pred :want
                                        :subj {:pred :they}}}
                                 generate
                                 morph)))))))

(deftest find-root-verb
  (is (= "querer"
         (-> spec generate (u/get-in [:root]))))
  (is (= "querer"
         (-> spec2 generate (u/get-in [:root])))))

(deftest analyze-1
  (let [analysis (analyze "quiero")]
    (is (seq analysis))
    (is (= "querer" (->> analysis (map #(u/get-in % [:canonical])) set first)))
    (is (= :present (->> analysis (map #(u/get-in % [:infl])) set first)))
    (is (= {:person :1st :number :sing} (->> analysis (map #(u/get-in % [:agr])) set first)))))

(deftest parse-1
  (let [parses (parse "yo quiero")]
    (is (= "[s .yo +quiero]" (-> parses first syntax-tree)))))



