(ns babel.test.fr
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.francais.writer :as fr :refer [small lexicon]]
            [babel.francais.morphology :refer [fo]]
            [babel.writer :as writer]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [dag-unify.core :refer [get-in unifyc]]))

(deftest conditional
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :sleep
                                                :subj {:pred :I}
                                                :tense :conditional}}}
                                fr/small)]
    (is (= "je dormirais" (fo result)))))

(deftest present-irregular
  (let [result (engine/generate {:synsem {:subcat '()
                                          :sem {:pred :be
                                                :subj {:pred :I}
                                                :tense :present}}}
                                fr/small)]
    (is (= "je suis" (fo result)))))

(deftest imperfect-irregular-être
  (let [result (engine/generate {:synsem {:subcat '()
                                          :infl :imperfect
                                          :sem {:pred :be
                                                :subj {:pred :I}}}}

                                fr/small)]
    (is (= "j'étais" (fo result)))))

(deftest imperfect-irregular-avoir
  (let [result (engine/generate {:synsem {:subcat '()
                                          :infl :imperfect
                                          :sem {:pred :avere
                                                :subj {:pred :I}}}}
                                fr/small)]
    (and (is (not (nil? result)))
         (is (= "av" (get-in result [:head :français :imperfect-stem])))
         (is (= "j'avais" (fo result))))))

(deftest passe-compose
  (let [result (engine/generate {:synsem {;:subcat '()
                                          :sem {:pred :andare
                                                :tense :passe-compose
                                                :subj {:pred :loro
                                                       :gender :fem}}}}
                                fr/small)]
    (and (is (not (nil? result)))
         (is (= "nous sommes allées")))))
