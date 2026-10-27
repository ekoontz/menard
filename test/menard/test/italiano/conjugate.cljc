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
  (testing "the auxiliary is essere with verbs marked :essere?"
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

(deftest direct-object-clitics
  (testing "the clitic is the noun's agreement"
    (is (= "lo" (conjugate/direct-object-clitic :3rd :sing :masc)))
    (is (= "la" (conjugate/direct-object-clitic :3rd :sing :fem)))
    (is (= "li" (conjugate/direct-object-clitic :3rd :plur :masc)))
    (is (= "le" (conjugate/direct-object-clitic :3rd :plur :fem)))
    (is (= "mi" (conjugate/direct-object-clitic :1st :sing)))
    (is (= "ci" (conjugate/direct-object-clitic :1st :plur)))
    (is (= "ti" (conjugate/direct-object-clitic :2nd :sing)))
    (is (= "vi" (conjugate/direct-object-clitic :2nd :plur))))
  (testing "every clitic is a non-reflexive accusative pronoun in the lexicon"
    (let [lexicon (:lexicon @it/model)]
      (doseq [[person numbers] conjugate/person-number-gender->direct-object-clitic
              [number genders] numbers
              [gender clitic] (if (map? genders)
                                genders
                                {:masc genders})
              :let [lexemes (get lexicon clitic)]]
        (is (some #(and (= :noun (:cat %))
                        (= :acc (:case %))
                        (not (true? (:reflexive? %)))
                        (= (cond-> {:person person :number number}
                             (map? genders) (assoc :gender gender))
                           (select-keys (:agr %) [:person :number :gender])))
                  lexemes)
            (str clitic " should be a " person " " number " " gender
                 " accusative pronoun")))))
  (testing "the clitic goes before the verb, eliding lo and la before a vowel"
    (is (= "lo vedo" (conjugate/with-direct-object-clitic :3rd :sing :masc
                                                         (conjugate/verb-form "vedere" :present-simple :1st :sing))))
    (is (= "l'amo" (conjugate/with-direct-object-clitic :3rd :sing :fem
                                                        (conjugate/verb-form "amare" :present-simple :1st :sing))))
    (is (= "l'ho visto" (conjugate/with-direct-object-clitic :3rd :sing :masc
                                                             (conjugate/verb-form "vedere" :passato :1st :sing))))
    (is (= "li vedo" (conjugate/with-direct-object-clitic :3rd :plur :masc
                                                          (conjugate/verb-form "vedere" :present-simple :1st :sing))))
    (testing "only lo and la elide"
      (is (= "mi alzo" (conjugate/with-direct-object-clitic :1st :sing :masc "alzo"))))))

(deftest object-less-verbs
  (testing "the lexicon marks the verbs that take no direct object"
    (is (conjugate/object-less? "andare"))
    (is (conjugate/object-less? "arrivare"))
    (is (conjugate/object-less? "ballare"))
    (is (conjugate/object-less? "stare"))
    (is (conjugate/object-less? "frequentare"))
    (is (conjugate/object-less? "sembrare"))
    (is (conjugate/object-less? "cenare"))
    (is (conjugate/object-less? "venire"))
    (is (conjugate/object-less? "entrare"))
    (is (conjugate/object-less? "riposare"))
    (is (conjugate/object-less? "vivere"))
    (is (conjugate/object-less? "cadere"))
    (is (conjugate/object-less? "nascere"))
    (is (conjugate/object-less? "chiacchierare")))
  (testing "a verb that can take one is not marked"
    (is (not (conjugate/object-less? "vedere")))
    (is (not (conjugate/object-less? "bere")))
    (is (not (conjugate/object-less? "mangiare")))
    (is (not (conjugate/object-less? "commentare")))
    (is (not (conjugate/object-less? "no-such-verb")))))

(deftest chart-shape
  (let [chart (conjugate/generate-chart "parlare" false)]
    (is (= "parlare" (:canonical chart)))
    (is (= 3 (count (:moods chart))))
    (is (= "parlo" (get-in chart [:moods 0 :inflections 0 :singular :1st])))))

(deftest takes-human-object
  (testing "verbs that take a person as their direct object"
    (doseq [canonical ["vedere" "commentare"]]
      (is (conjugate/takes-human-object? canonical) canonical)))
  (testing "not the verbs that take no direct object"
    (doseq [canonical ["andare" "arrivare" "ballare" "stare" "frequentare"
                       "sembrare" "cenare" "venire" "entrare"
                       "riposare" "vivere" "cadere" "nascere"
                       "chiacchierare"]]
      (is (not (conjugate/takes-human-object? canonical)) canonical)))
  (testing "not the verbs whose object must be inanimate"
    (doseq [canonical ["giocare" "suonare" "mangiare" "pensare" "aprire"
                       "avere" "fare" "bere" "leggere" "mettere"
                       "cancellare" "riparare" "parlare" "pulire"
                       "tagliare" "guidare" "spendere" "sapere" "conservare"
                       "formare" "prestare" "abbassare" "comprare"
                       "riempire" "spegnere" "cantare" "comprare"
                       "imparare"]]
      (is (not (conjugate/takes-human-object? canonical)) canonical))))

(deftest indirect-object-clitics
  (testing "the dative clitic is the noun's agreement"
    (is (= "gli" (conjugate/indirect-object-clitic :3rd :sing :masc)))
    (is (= "le" (conjugate/indirect-object-clitic :3rd :sing :fem)))
    (is (= "gli" (conjugate/indirect-object-clitic :3rd :plur :masc)))
    (is (= "gli" (conjugate/indirect-object-clitic :3rd :plur :fem)))
    (is (= "mi" (conjugate/indirect-object-clitic :1st :sing)))
    (is (= "ci" (conjugate/indirect-object-clitic :1st :plur)))
    (is (= "ti" (conjugate/indirect-object-clitic :2nd :sing)))
    (is (= "vi" (conjugate/indirect-object-clitic :2nd :plur))))
  (testing "it goes before the verb"
    (is (= "gli do" (conjugate/with-indirect-object-clitic :3rd :sing :masc "do")))
    (is (= "le do" (conjugate/with-indirect-object-clitic :3rd :sing :fem "do")))
    (is (= "mi dai" (conjugate/with-indirect-object-clitic :1st :sing "dai"))))
  (testing "with a direct object the dative comes first"
    (is (= "glielo do" (conjugate/with-clitics "do" {:direct [:3rd :sing :masc]
                                                     :indirect [:3rd :sing :masc]})))
    (is (= "gliela do" (conjugate/with-clitics "do" {:direct [:3rd :sing :fem]
                                                     :indirect [:3rd :sing :fem]})))
    (is (= "glieli do" (conjugate/with-clitics "do" {:direct [:3rd :plur :masc]
                                                     :indirect [:3rd :sing :masc]})))
    (is (= "me lo dai" (conjugate/with-clitics "dai" {:direct [:3rd :sing :masc]
                                                      :indirect [:1st :sing]})))
    (is (= "ce lo danno" (conjugate/with-clitics "danno" {:direct [:3rd :sing :masc]
                                                          :indirect [:1st :plur]}))))
  (testing "lo and la still elide"
    (is (= "gliel'ho dato" (conjugate/with-clitics "ho dato" {:direct [:3rd :sing :masc]
                                                              :indirect [:3rd :sing :masc]})))
    (is (= "me l'ho dato" (conjugate/with-clitics "ho dato" {:direct [:3rd :sing :masc]
                                                             :indirect [:1st :sing]})))
    (is (= "l'ho dato" (conjugate/with-clitics "ho dato" {:direct [:3rd :sing :masc]})))))

(deftest takes-indirect-object
  (testing "the verbs the lexicon marks as taking a recipient"
    (doseq [canonical ["dare" "dire" "mostrare" "portare" "scrivere"]]
      (is (conjugate/takes-indirect-object? canonical) canonical)))
  (testing "not the others"
    (doseq [canonical ["vedere" "andare" "mangiare" "no-such-verb"]]
      (is (not (conjugate/takes-indirect-object? canonical)) canonical))))
