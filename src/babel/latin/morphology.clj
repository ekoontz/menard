(ns babel.latin.morphology (:require [babel.morphology :as morph :refer [conjugation]]))

;; Present indicative
;; https://en.wikipedia.org/wiki/Latin_conjugation#Present_indicative
(def are-present-indicative
  (conjugation 
   {:infinitive "āre"     
    :common {:synsem {:infl :present
                      :sem {:tense :present}}}
    :forms
    [["^(.+)o$"
      {:synsem {:sem {:subj {:pred :I}}}}]

     ["^(.+)as$"
      {:synsem {:sem {:subj {:pred :tu}}}}]
     
     ["^(.+)at$"
      {:synsem {:sem {:subj {:pred :lui}}}}]
     
     ["^(.+)o$"
      {:synsem {:sem {:subj {:pred :noi}}}}]
     
     ["^(.+)as$"
      {:synsem {:sem {:subj {:pred :voi}}}}]
     
     ["^(.+)at$"
      {:synsem {:sem {:subj {:pred :loro}}}}]]}))

(def ere-present-indicative
  (conjugation 
   {:infinitive "ēre"     
    :common {:synsem {:infl :present
                      :sem {:tense :present}}}
    :forms
    [["eo"
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

(def replace-patterns (concat are-present-indicative
                              ere-present-indicative))

(defn analyze [surface-form lexicon]
  (morph/analyze surface-form lexicon replace-patterns))

(defn conjugate [surface-form lexicon]
  (morph/conjugate surface-form lexicon replace-patterns))


  

