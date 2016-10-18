(ns babel.test.la
  (:require
   [babel.directory :refer [models]]
   [babel.engine :as engine]
   [babel.latin.morphology :refer [analyze conjugate]]
   [babel.latin :refer [fo generate lexicon model]]
   [clojure.test :refer [deftest is]]))

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
  (let [source-language :en

        ;; function to generate expression in target langage
        source-generate-fn (-> models source-language :generate-fn)

        ;; function to render target expression as text. show-notes=false because
        ;; we are translating from Latin which (at least for the purposes of this
        ;; implemetation) lacks the same kind of gender differences that Italian, for
        ;; example, has.
        source-format-fn #((-> models source-language :morph) % :show-notes false)

        generated (->
                   {:synsem {:sem {:subj {:pred :lui}
                                   :obj :unspec
                                   :tense :past
                                   :aspect :progressive
                                   :pred :answer}}}
                   source-generate-fn
                   source-format-fn)]
    
    (or (= generated "he used to answer")
        (= generated "he was answering"))))





