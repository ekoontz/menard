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

(defn model [] (-> ((-> models :la)) deref))

(defn generate [spec]
  ((-> (model) :generate-fn) spec))

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
        result (read-one {:root "ardēre"
                          :synsem {:sem {:tense :past
                                         :aspect :progressive}}}
                         (model) source-model)
        ;; for specific literal strings in the result.
        subj (get-in result [:semantics :subj :pred])
        possible-answer (first (get result :targets))]
    (log/info (str "reader2 test: result:" result))
    (is
     (or
      (and
       (= subj :I)
       (= "ardebam"
          possible-answer))
      (and
       (= subj :tu)
       (= "ardebas"
          possible-answer))
      (and
       (= subj :noi)
       (= "ardebamus"
          possible-answer))
      (and
       (= subj :voi)
       (= "ardebatis"
          possible-answer))
      (and
       (or (= subj :lui)
           (= subj :lei))
       (= "ardebat"
          possible-answer))
      (and
       (= subj :loro)
       (= "ardebant"
          possible-answer))))))
