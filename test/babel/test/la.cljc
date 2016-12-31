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
                    {:synsem {:sem {:subj {:pred :noi}}}}))))

(deftest generate-present
  (is (= "ardetis"
         (fo (generate
              {:root "ardēre"
               :synsem {:sem {:subj {:pred :voi}
                              :tense :present}}})))))

(deftest generate-imperfect
  (is (= "ardebam"
         (fo (generate
              {:root "ardēre"
               :synsem {:sem {:subj {:pred :I}
                              :tense :past
                              :aspect :progressive}}})))))

(deftest generate-future
  (is (= "ardebunt"
         (fo (generate
              {:root "ardēre"
               :synsem {:sem {:subj {:pred :loro}
                              :tense :future}}})))))
(deftest reader1
  (let [spec {:synsem {:sem {:subj {:pred :lui}
                             :obj :unspec
                             :tense :past
                             :aspect :progressive
                             :pred :answer}}}
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
    (is (or (= source "he used to answer")
            (= source "he was answering")
            (= source "he used to respond")
            (= source "he was responding")))
    (is (or (= target "respondebat")))))

(deftest reader2
  (let [source-model (-> ((-> models :en)) deref)

        ;; use a specific :root and verb conjugation so that we can test
        ;; for specific literal strings in the result.
        results (take 1 (repeatedly #(read-one {:root "ardēre"
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
