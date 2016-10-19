(ns babel.test.la
  (:require
   [babel.directory :refer [models]]
   [babel.engine :as engine]
   [babel.english :as source]
   [babel.latin.morphology :refer [analyze conjugate]]
   [babel.latin :as target :refer [fo generate lexicon model]]
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

;; function to generate expression in target language
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
  "choose a random spec based on the given curriculum and model"
  ;; for now, stubbed out: imagine a curriculum narrowly based on a single verb and
  ;; the imperfect tense.
  (target/get-spec
   {:root "ardēre"
    :synsem {:sem {:tense :past
                   :aspect :progressive}}}))
  
(def curriculum
  {:nouns ["lui" "lei"]
   :verbs :all
   :tenses :all})

(def custom-model
  (intersection
   curriculum
   target/model))

(def source-model (babel.english.grammar/small))

(deftest reader2
  (let [spec (choose-spec curriculum target/model)
        target-expression (target/generate spec)]

    (is (string? (target/morph target-expression)))
    (let [;; this is to show the user question in their native (i.e. 'source') language.
          semantics (get-in target-expression [:synsem :sem])
          pose-question-to-user
          (source/morph (source/generate {:synsem {:sem semantics}}
                                         :model source-model))
          result
          (-> spec
              target/generate)]
      (is (not (nil? result))))))
