(ns menard.test.español.preterito-perfecto
  (:require [menard.español :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; These are for convenience so you don't
;; have to reload these language-model files every time you make a change:
;; you can just reload this file (menard.test.español.preterito-perfecto.clj)
;; file and it will reload the changed language-model files.
(load "../../../../src/menard/subcat")
(load "../../../../src/menard/español")

;; https://es.wiktionary.org/wiki/comer#Conjugaci%C3%B3n

(deftest analyze-test
  (let [analysis (analyze "comido")]
    (is (seq analysis))
    (is (= 6 (count analysis)))
    (is (= #{true} (->> analysis (map map?) set)))
    (is (= #{:participio} (->> analysis (map #(u/get-in % [:infl])) set)))))

(deftest parse-test-explicit-subject
  (let [non-reflexive (-> "yo he comido" parse first)]
    (is (= (-> non-reflexive syntax-tree)
           "[s-aux(:preterito-perfecto) .yo +[vp-aux-non-reflexive(:preterito-perfecto) +he(:explicit-subj-non-reflexive-intransitive) .comido]]"))
    (is (= (-> non-reflexive (u/get-in [:sem :aspect])) :perfect))
    (is (= (-> non-reflexive (u/get-in [:sem :tense])) :past)))
  (let [reflexive (-> "yo me he lastimado" parse first)]
    (is (= (-> reflexive syntax-tree)
           "[s-aux(:preterito-perfecto){+} .yo +[vp-aux-reflexive-3(:preterito-perfecto){+} .me +[vp-aux-reflexive-2(:preterito-perfecto){+} +he(:explicit-subj-reflexive) .lastimado(:explicit-subj)]]]"))))

(deftest parse-test-null-subject
  ;; TODO: add lexical rule
  ;; that produces all 4 of the following subcat frames:
  ;; 1. <> (intransitive and implicit subject)
  ;; 2. <noun(subj)> (intransitive and explicit subject)
  ;; 3. <noun(obj)> (transitive and implicit subject)
  ;; 4. <noun(subj), noun(obj)> (transitive and explicit subject)
  ;; Adding this lexical rule will likely simplify many of the rules in grammar.edn.
  ;;  {:rule "s-aux-nonreflexive-subj-implicit" ;; "he comido"

  ;; Until then, this test is commented out:
  ;;  (let [non-reflexive (-> "he comido" parse first)]
  ;;    (is (= (-> non-reflexive syntax-tree)
  ;;           "[s-aux(:preterito-perfecto) +he .comido]")))

  (let [reflexive (->> "me he lastimado"
                       parse
                       ;; filter out partial parses:
                       (filter #(= [] (u/get-in % [:subcat])))
                       first)]
    (is (= (-> reflexive syntax-tree)
           "[s-aux(:preterito-perfecto){+} .me +[vp-aux-reflexive-1(:preterito-perfecto){+} +he(:implicit-subj-reflexive) .lastimado(:implicit-subj)]]"))
    (is (= (-> reflexive (u/get-in [:sem :aspect])) :perfect))
    (is (= (-> reflexive (u/get-in [:sem :tense])) :past)))


  (let [non-reflexive (->> "yo he comido"
                           parse
                           (filter #(= [] (u/get-in % [:subcat])))
                           first)]
    (is (= (-> non-reflexive syntax-tree)
           "[s-aux(:preterito-perfecto) .yo +[vp-aux-non-reflexive(:preterito-perfecto) +he(:explicit-subj-non-reflexive-intransitive) .comido]]"))))

(def non-reflexive-spec
  {:cat :verb
   :subcat []
   ;; TODO: this {:reflexive? false} is required for now,
   ;; otherwise generation gets stuck.
   :reflexive? false

   ;; TODO: this is required for now also to avoid generating
   ;; incorrrectly present tense sentences:
   :head {:rule "vp-aux-non-reflexive"},

   :sem {:pred :eat
         :subj {:pred :i}
         :tense :past
         :aspect :perfect}})

(def use-head-canonical-spec
  {:comp {:agr {:number :sing, :person :3rd}
          :canonical "ella"}
   :subcat []
   :sem {:tense :past, :aspect :perfect}
   ;; TODO: this {:reflexive? false} is required for now,
   ;; otherwise generation gets stuck.
   :reflexive? false
   :head {:canonical "mirar"}})

(deftest generate-test
  (is (= (-> non-reflexive-spec generate syntax-tree)
         "[s-aux(:preterito-perfecto) .yo +[vp-aux-non-reflexive(:preterito-perfecto) +he(:explicit-subj-non-reflexive-intransitive) .comido]]"))
;;  (is (= (-> use-head-canonical-spec generate syntax-tree)
;;         "[s-aux(:preterito-perfecto) .ella +[vp-aux-non-reflexive(:preterito-perfecto) +ha(:explicit-subj-non-reflexive-intransitive) .mirado]]")))
  )

(defn inner-function [[firstm & restms]]
  (when firstm
    (lazy-cat
     (map #(unify firstm %) restms)
     (inner-function restms))))

(defn cleanup [[firstm & restms]]
  (when firstm
    (let [overs (->> restms
                     (map (fn [m]
                            (= (serialize m)
                               (serialize (unify firstm m)))))
                     (filter true?))]
      (if (seq overs)
        (cleanup restms)
        (cons firstm (cleanup restms))))))

(defn cross-product-1 [maps]
  (->
   maps
   (->> inner-function
        (remove #(= % :fail)))
   set
   ((fn [s]
      (if (empty? s)
        (set maps)
        (cross-product-1 s))))))

(defn cross-product [maps]
  (-> maps set cleanup cross-product-1))

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

  (is (= (-> [{:a 1}{:a 2}{:a 3}{:a 4}{:a 1 :b 43}] shuffle cross-product)
         #{{:a 1
            :b 43}
           {:a 2}
           {:a 3}
           {:a 4}})))


