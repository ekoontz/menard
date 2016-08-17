(ns babel.english.lab
  (:require [babel.english :refer [generate]]
            [babel.english.grammar :refer [verbcoach]]
            [babel.english.morphology :as morph :refer [fo]]))

(def foo
  (generate {:synsem {:sem {:pred :giocare
                            :subj {:pred :I}
                            :tense :past
                            :aspect :perfect
                            }}} :model verbcoach))

;; TODO: make this a test
(defn run []
  (repeatedly 
   #(println 
     (fo (generate {:synsem {:sem {:pred :go
                                   :subj {:pred :lui}
                                   :tense :present
                                   :aspect :progressive
                                   }}} :model verbcoach)
         :from-language "it"))))

(defn run2 [n]
  (take (Integer. n)
        (repeatedly
         #(println
           (fo
            (generate {:synsem {:cat :verb
                                :sem {:pred :top
                                      :subj {:mod '()}
                                      :obj {:pred :top
                                            :mod '()}}}
                     :modified false
                       :head {:comp {:synsem {:pronoun false}}}})
            :for "it")))))




