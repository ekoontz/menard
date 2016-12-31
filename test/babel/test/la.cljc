(ns babel.test.la
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.directory :refer [models]]
   [babel.english :as source]
   [babel.latin.morphology :refer [analyze conjugate]]
   [babel.latin :as target :refer [fo read-one]]
   [clojure.repl :refer [doc]]
   [clojure.test :refer [deftest is]]
   [clojure.tools.logging :as log]
   [dag_unify.core :refer [get-in strip-refs unify]]))

(def source-language :en)

(defn latin-model [] (-> ((-> models :la)) deref))

(defn generate [spec]
  ((-> (latin-model) :generate-fn) spec))

;; https://en.wikipedia.org/wiki/Latin_conjugation#Present_indicative
(deftest analyze-ere
  (let [lexicon (-> ((-> models :la)) deref :lexicon)]
    (is (= :verb
           (-> (analyze "ardeo" lexicon)
               first
               (get-in [:synsem :cat]))))))

(deftest conjugate-ere
  (is (= "ardemus"
         (conjugate "ardēre"
                    {:synsem {:agr {:person :1st :number :plur}}}))))

(deftest generate-present
  (is (= "ardetis"
         (fo (generate
              {:root "ardēre"
               :synsem {:agr {:person :2nd :number :plur}
                        :sem {:tense :present}}})))))

(deftest generate-imperfect
  (is (= "ardebam"
         (fo (generate
              {:root "ardēre"
               :synsem {:agr {:person :1st :number :sing}
                        :sem {:tense :past
                              :aspect :progressive}}})))))

(deftest generate-future
  (is (= "ardebunt"
         (fo (generate
              {:root "ardēre"
               :synsem {:agr {:person :3rd :number :plur}
                        :sem {:tense :future}}})))))
(deftest reader1
  (let [spec (let [agreement (atom {:person :3rd :number :sing :gender :masc})]
               {:synsem {:agr agreement
                         :sem {:obj :unspec
                               :tense :past
                               :subj {:pred :lui}
                               :aspect :progressive
                               :pred :answer}}
                :comp {:synsem {:agr agreement}}})
        source-format-fn (-> ((-> models source-language)) deref :morph)
        source-generate-fn (-> ((-> models source-language)) deref :generate-fn)
        target-format-fn (-> ((-> models :la)) deref :morph)

        source (->
                spec
                source-generate-fn
                source-format-fn)
        target (->
                spec
                generate
                target-format-fn)]
    (log/info (str "source: " source))
    (log/info (str "target: " target))
    (is (or (= source "he used to answer")
            (= source "he was answering")
            (= source "he used to respond")
            (= source "he was responding")))
    (is (or (= target "respondebat")))))

(deftest reader2
  (let [source-model (-> ((-> models :en)) deref)

        ;; use a specific :root and verb conjugation so that we can test
        ;; for specific literal strings in the result.
        results (take 10 (repeatedly #(read-one {:root "ardēre"
                                                 :synsem {:sem {:tense :past
                                                                :aspect :progressive}}}
                                                (latin-model) source-model)))]
    (doall
     (->> results
          (map (fn [result]
                 (let [possible-answer (first (shuffle (get result :targets)))]
                   (log/debug (str "reader2 possible-answer:" possible-answer))
                   (is
                    (or
                     (and
                      (= "ardebam"
                         possible-answer))
                     (and
                      (= "ardebas"
                         possible-answer))
                     (and
                      (= "ardebamus"
                         possible-answer))
                     (and
                      (= "ardebatis"
                         possible-answer))
                     (and
                      (= "ardebat"
                         possible-answer))
                     (and
                      (= "ardebant"
                         possible-answer))
                     (or
                      (log/info (str "failsafe: result:" result))
                      (log/info (str "failsafe: possible-answer:" possible-answer))
                      false))))))))))
