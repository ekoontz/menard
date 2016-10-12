(ns babel.latin.morphology (:require [babel.morphology :as morph :refer [conjugation]]))

;; Present indicative
;; https://en.wikipedia.org/wiki/Latin_conjugation#Present_indicative
(def present {:synsem {:sem {:tense :present}}})

(def are-present-indicative
  (conjugation 
   {:infinitive "āre"     
    :common present
    :forms
    [["o"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["as"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["at"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["amus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["atis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["ant"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ere-bar-present-indicative
  (conjugation 
   {:infinitive "ēre"     
    :common present
    :forms
    [["eo"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["es"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["et"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["emus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["etis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["ent"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ere-present-indicative
  (conjugation 
   {:infinitive "ere"     
    :common present
    :forms
    [["o"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["is"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["it"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["imus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["itis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["unt"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ere-i-present-indicative
  (conjugation 
   {:infinitive "ere-i" ;; FIXME: use {:conjugation :3i}
    :common present
    :forms
    [["io"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["is"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["it"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["imus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["itis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["iunt"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ire-present-indicative
  (conjugation 
   {:infinitive "īre"     
    :common present
    :forms
    [["o"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["is"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["it"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["imus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["itis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["unt"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

;;IMPERFECT BEGINS HERE
(def imperfect {:synsem {:sem {:tense :past
                               :aspect :progressive}}})

(def are-imperfect-indicative
  (conjugation 
   {:infinitive "āre"     
    :common imperfect
    :forms
    [["abam"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["abas"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["abat"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["abamus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["abatis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["abant"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ere-bar-imperfect-indicative
  (conjugation 
   {:infinitive "ēre"     
    :common imperfect
    :forms
    [["ebam"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["ebas"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["ebat"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["ebamus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["ebatis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["ebant"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ere-imperfect-indicative
  (conjugation 
   {:infinitive "ere"     
    :common imperfect
    :forms
    [["ebam"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["ebas"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["ebat"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["ebamus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["ebatis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["ebant"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ere-i-imperfect-indicative
  (conjugation 
   {:infinitive "ere-i" ;; FIXME: use {:conjugation :3i}
    :common imperfect
    :forms
    [["iebam"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["iebas"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["iebat"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["iebamus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["iebatis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["iebant"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ire-imperfect-indicative
  (conjugation 
   {:infinitive "īre"     
    :common imperfect
    :forms
    [["iebam"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["iebas"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["iebat"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["iebamus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["iebatis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["iebant"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

;;FUTURE BEGINS HERE

(def future {:synsem {:sem {:tense :future}}})

(def are-future-indicative
  (conjugation 
   {:infinitive "āre"     
    :common future
    :forms
    [["abo"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["abis"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["abit"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["abimus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["abitis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["abunt"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ere-bar-future-indicative
  (conjugation 
   {:infinitive "ēre"     
    :common future
    :forms
    [["ebo"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["ebis"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["ebit"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["ebimus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["ebitis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["ebunt"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ere-future-indicative
  (conjugation 
   {:infinitive "ere"     
    :common future
    :forms
    [["am"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["es"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["et"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["emus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["etis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["ent"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ere-i-future-indicative
  (conjugation 
   {:infinitive "ere-i" ;; FIXME: use {:conjugation :3i}
    :common future
    :forms
    [["iam"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["ies"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["iet"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["iemus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["ietis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["ient"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ire-future-indicative
  (conjugation 
   {:infinitive "īre"     
    :common future
    :forms
    [["iam"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["ies"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["iet"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["iemus"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["ietis"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["ient"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

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

(defn conjugate [structure lexicon]
  "reverse of analyze: find the surface form given a lexical structure and a lexicon"
  (morph/conjugate structure lexicon replace-patterns))
