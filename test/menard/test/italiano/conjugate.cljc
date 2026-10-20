(ns menard.test.italiano.conjugate
  (:require [menard.italiano :as it]
            [menard.italiano.conjugate :as conjugate]
            [clojure.test :refer [deftest is testing]]))

(deftest regular-verbs
  (is (= "parlo" (conjugate/verb-form "parlare" :present-simple :1st :sing)))
  (is (= "parlavi" (conjugate/verb-form "parlare" :imperfetto :2nd :sing)))
  (is (= "parleremo" (conjugate/verb-form "parlare" :future :1st :plur)))
  (is (= "parlerebbero" (conjugate/verb-form "parlare" :conditional :3rd :plur))))

(deftest irregular-verbs
  (testing "irregular present, future and conditional forms come from the lexicon"
    (is (= "vado" (conjugate/verb-form "andare" :present-simple :1st :sing)))
    (is (= "ho" (conjugate/verb-form "avere" :present-simple :1st :sing)))
    (is (= "sono" (conjugate/verb-form "essere" :present-simple :1st :sing)))
    (is (= "faccio" (conjugate/verb-form "fare" :present-simple :1st :sing)))
    (is (= "andrò" (conjugate/verb-form "andare" :future :1st :sing)))
    (is (= "andrei" (conjugate/verb-form "andare" :conditional :1st :sing)))
    (is (= "facevo" (conjugate/verb-form "fare" :imperfetto :1st :sing)))))

(deftest compound-tenses
  (testing "the auxiliary is essere with verbs marked :essere"
    (is (= "sono andato" (conjugate/verb-form "andare" :passato :1st :sing)))
    (is (= "ero andato" (conjugate/verb-form "andare" :trapassato :1st :sing))))
  (testing "the auxiliary is avere otherwise, and the participle is invariant"
    (is (= "ho camminato" (conjugate/verb-form "camminare" :passato :1st :sing)))
    (is (= "hanno camminato" (conjugate/verb-form "camminare" :passato :3rd :plur)))
    (is (= "ho letto" (conjugate/verb-form "leggere" :passato :1st :sing)))
    (is (= "avevo letto" (conjugate/verb-form "leggere" :trapassato :1st :sing))))
  (testing "the participle agrees with the subject's number after essere"
    (is (= "sono andati" (conjugate/verb-form "andare" :passato :3rd :plur)))))

(deftest progressive
  (is (= "sto camminando" (conjugate/verb-form "camminare" :present-progressive :1st :sing)))
  (is (= "stanno facendo" (conjugate/verb-form "fare" :present-progressive :3rd :plur))))

(deftest reflexive-verbs
  (is (= "mi alzo" (conjugate/form-with-clitic "alzarsi" :present-simple :1st :sing)))
  (is (= "si alzano" (conjugate/form-with-clitic "alzarsi" :present-simple :3rd :plur)))
  (testing "reflexive verbs take essere"
    (is (= "mi sono alzato" (conjugate/form-with-clitic "alzarsi" :passato :1st :sing)))))

(deftest chart-shape
  (let [chart (conjugate/generate-chart "parlare" false)]
    (is (= "parlare" (:canonical chart)))
    (is (= 3 (count (:moods chart))))
    (is (= "parlo" (get-in chart [:moods 0 :inflections 0 :singular :1st])))))
