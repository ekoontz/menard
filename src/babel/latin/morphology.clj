(ns babel.latin.morphology (:require [babel.morphology :as morph :refer [conjugation]]))

;; Present indicative
;; https://en.wikipedia.org/wiki/Latin_conjugation#Present_indicative
(def are-present-indicative
  (conjugation 
   {:infinitive "āre"     
    :common {:synsem {:infl :present
                      :sem {:tense :present}}}
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
    :common {:synsem {:infl :present
                      :sem {:tense :present}}}
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
    :common {:synsem {:infl :present
                      :sem {:tense :present}}}
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



(def ere-present-indicative
  (conjugation 
   {:infinitive "ere-i"     
    :common {:synsem {:infl :present
                      :sem {:tense :present}}}
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
    :common {:synsem {:infl :present
                      :sem {:tense :present}}}
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

(def replace-patterns (concat are-present-indicative
                              ere-bar-present-indicative
                              ere-present-indicative
                              ire-present-indicative))









(defn analyze [surface-form lexicon]
  (morph/analyze surface-form lexicon replace-patterns))

(defn conjugate [surface-form lexicon]
  (morph/conjugate surface-form lexicon replace-patterns))


  

