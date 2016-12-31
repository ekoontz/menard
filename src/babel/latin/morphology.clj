(ns babel.latin.morphology
  (:require [babel.morphology :as morph :refer [conjugation]]
            [clojure.tools.logging :as log]
            [dag_unify.core :refer [strip-refs]]))

;; Present indicative
;; https://en.wikipedia.org/wiki/Latin_conjugation#Present_indicative
(def present {:synsem {:sem {:tense :present}}})

(def are-present-indicative
  (conjugation 
   {:infinitive "āre"     
    :common present
    :forms
    [["o"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["as"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["at"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["at"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["amus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["atis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["ant"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ere-bar-present-indicative
  (conjugation 
   {:infinitive "ēre"     
    :common present
    :forms
    [["eo"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["es"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["et"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["et"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["emus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["etis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["ent"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ere-present-indicative
  (conjugation 
   {:infinitive "ere"     
    :common present
    :forms
    [["o"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["is"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["it"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["it"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["imus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["itis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["unt"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ere-i-present-indicative
  (conjugation 
   {:infinitive "ere-i" ;; FIXME: use {:conjugation :3i}
    :common present
    :forms
    [["io"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["is"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["it"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["it"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["imus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["itis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["iunt"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ire-present-indicative
  (conjugation 
   {:infinitive "īre"     
    :common present
    :forms
    [["o"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["is"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["it"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["it"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["imus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["itis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["unt"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

;;IMPERFECT BEGINS HERE
(def imperfect {:synsem {:sem {:tense :past
                               :aspect :progressive}}})

(def are-imperfect-indicative
  (conjugation 
   {:infinitive "āre"     
    :common imperfect
    :forms
    [["abam"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["abas"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["abat"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["abat"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]

     ["abamus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["abatis"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["abant"
      {:synsem {:agr {:person :1st :number :plur}}}]]}))

(def ere-bar-imperfect-indicative
  (conjugation 
   {:infinitive "ēre"     
    :common imperfect
    :forms
    [["ebam"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["ebas"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["ebat"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["ebat"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["ebamus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["ebatis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["ebant"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ere-imperfect-indicative
  (conjugation 
   {:infinitive "ere"     
    :common imperfect
    :forms
    [["ebam"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["ebas"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["ebat"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]
     
     ["ebat"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]

     ["ebamus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["ebatis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["ebant"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ere-i-imperfect-indicative
  (conjugation 
   {:infinitive "ere-i" ;; FIXME: use {:conjugation :3i}
    :common imperfect
    :forms
    [["iebam"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["iebas"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["iebat"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["iebat"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["iebamus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["iebatis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["iebant"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ire-imperfect-indicative
  (conjugation 
   {:infinitive "īre"     
    :common imperfect
    :forms
    [["iebam"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["iebas"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["iebat"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["iebat"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["iebamus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["iebatis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["iebant"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

;;FUTURE BEGINS HERE

(def future-tense {:synsem {:sem {:tense :future}}})

(def are-future-indicative
  (conjugation 
   {:infinitive "āre"     
    :common future-tense
    :forms
    [["abo"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["abis"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["abit"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["abit"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["abimus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["abitis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["abunt"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ere-bar-future-indicative
  (conjugation 
   {:infinitive "ēre"     
    :common future-tense
    :forms
    [["ebo"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["ebis"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["ebit"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["ebit"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["ebimus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["ebitis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["ebunt"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ere-future-indicative
  (conjugation 
   {:infinitive "ere"     
    :common future-tense
    :forms
    [["am"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["es"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["et"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["et"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["emus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["etis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["ent"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ere-i-future-indicative
  (conjugation 
   {:infinitive "ere-i" ;; FIXME: use {:conjugation :3i}
    :common future-tense
    :forms
    [["iam"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["ies"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["iet"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["iet"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["iemus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["ietis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["ient"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def ire-future-indicative
  (conjugation 
   {:infinitive "īre"     
    :common future-tense
    :forms
    [["iam"
      {:synsem {:agr {:person :1st :number :sing}}}]

     ["ies"
      {:synsem {:agr {:person :2nd :number :sing}}}]
     
     ["iet"
      {:synsem {:agr {:person :3rd :number :sing :gender :masc}}}]

     ["iet"
      {:synsem {:agr {:person :3rd :number :sing :gender :fem}}}]
     
     ["iemus"
      {:synsem {:agr {:person :1st :number :plur}}}]
     
     ["ietis"
      {:synsem {:agr {:person :2nd :number :plur}}}]
     
     ["ient"
      {:synsem {:agr {:person :3rd :number :plur}}}]]}))

(def replace-patterns (concat are-present-indicative
                              ere-bar-present-indicative
                              ere-present-indicative
                              ire-present-indicative
                              
                              are-imperfect-indicative
                              ere-bar-imperfect-indicative
                              ere-imperfect-indicative
                              ire-imperfect-indicative

                              are-future-indicative
                              ere-bar-future-indicative
                              ere-future-indicative
                              ire-future-indicative))

(defn analyze [surface-form lexicon]
  "find the lexical structure for a surface form given a lexicon"
  (morph/analyze surface-form lexicon replace-patterns))

(defn conjugate [infinitive structure]
  "reverse of analyze: find the surface form given an infinitive form and a structure"
  (log/debug (str "latin morphology: infinitive:" infinitive))
  (log/debug (str "latin morphology: structure:" (dissoc (strip-refs structure) :dag_unify.core/serialized)))
  (morph/conjugate infinitive structure replace-patterns))
