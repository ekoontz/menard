(ns babel.test.la
  (:require
   [babel.directory :refer [models]]
   [babel.engine :as engine]
   [babel.latin.morphology :refer [analyze conjugate]]
   [babel.latin :as la :refer [fo generate lexicon model]]
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

(def source-language :en)

;; function to generate expression in target langage
(def source-generate-fn (-> models source-language :generate-fn))

;; function to render target expression as text. show-notes=false because
;; we are translating from Latin which (at least for the purposes of this
;; implemetation) lacks the same kind of gender differences that Italian, for
;; example, has.
(def source-format-fn #((-> models source-language :morph) % :show-notes false))

(def target-language :la)

        ;; function to generate expression in target langage
(def target-generate-fn (-> models target-language :generate-fn))

(def target-format-fn (-> models target-language :morph))

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


(defn intersection [curriculum model]
  ;; TODO implement this stub
  model)

(defn choose-spec [curriculum model]
  ;; TODO: implement this stub
  :top)

(deftest reader2
  (let [curriculum
        {:nouns ["lui" "lei"]
         :verbs :all
         :tenses :all}

        custom-model
        (intersection
         curriculum
         la/model)

        spec (choose-spec curriculum la/model)]
    (is (= :top
           spec))))
