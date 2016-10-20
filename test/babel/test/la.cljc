(ns babel.test.la
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.directory :refer [models]]
   [babel.engine :as engine]
   [babel.english :as source]
   [babel.latin.morphology :refer [analyze conjugate]]
   [babel.latin :as target :refer [fo generate lexicon model read-one source-model]]
   [clojure.repl :refer [doc]]
   [clojure.test :refer [deftest is]]
   [clojure.tools.logging :as log]
   [dag_unify.core :refer [get-in strip-refs unify]]))

(def source-language :en)
(def target-language :la)

;; function to generate expression in target language
(def source-generate-fn (-> models source-language deref :generate-fn))

;; function to render target expression as text. show-notes=false because
;; we are translating from Latin which (at least for the purposes of this
;; implemetation) lacks the same kind of gender differences that Italian, for
;; example, has.
(def source-format-fn #((-> models source-language deref :morph) % :show-notes false))

;; function to generate expression in target langage
(def target-generate-fn (-> models target-language deref :generate-fn))

(def target-format-fn (-> models target-language deref :morph))

;; https://en.wikipedia.org/wiki/Latin_conjugation#Present_indicative
(deftest analyze-ere
  (is (= :verb
         (-> (analyze "ardeo" lexicon)
             first
             (get-in [:synsem :cat])))))

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
(deftest engine-generate
  (is (= "ardetis"
         (fo (engine/generate
              {:root "ardēre"
               :synsem {:sem {:subj {:pred :voi}
                              :tense :present}}}
              model)))))

(deftest reader1
  (let [spec {:synsem {:sem {:subj {:pred :lui}
                             :obj :unspec
                             :tense :past
                             :aspect :progressive
                             :pred :answer}}}
        source (->
                spec
                source-generate-fn
                source-format-fn)
        target (->
                spec
                target-generate-fn
                target-format-fn)]
    (is (or (= source "he used to answer")
            (= source "he was answering")
            (= source "he used to respond")
            (= source "he was responding")))
    (is (or (= target "respondebat")))))

(deftest reader2
  (let [;; use a specific :root and verb conjugation so that we can test
        result (read-one {:root "ardēre"
                          :synsem {:sem {:tense :past
                                         :aspect :progressive}}})
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
