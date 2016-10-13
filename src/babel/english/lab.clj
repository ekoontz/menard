(ns babel.english.lab
  (:require [babel.english :refer [generate]]
            [babel.english.grammar :refer [small]]
            [babel.english.morphology :as morph :refer [fo]]))

(def foo
  (generate {:synsem {:sem {:pred :giocare
                            :subj {:pred :I}
                            :tense :past
                            :aspect :perfect
                            }}} :model small))

;; TODO: make this a test
(defn run []
  (repeatedly 
   #(println 
     (fo (generate {:synsem {:sem {:pred :go
                                   :subj {:pred :lui}
                                   :tense :present
                                   :aspect :progressive
                                   }}} :model small)
         :from-language "it"))))

;; lein run -m babel.english.lab/run2 20 5
(defn run2 [batches at-a-time]
  (count
   (take (Integer. batches)
         (repeatedly
          #(println
            (clojure.string/join
             "\n"
             (pmap
              (fn [x]
                (fo
                 (generate {:synsem {:cat :verb
                                     :sem {:pred :top
                                           :subj {:mod '()}
                                           :obj {:pred :top
                                                 :mod '()}}}
                            :modified false
                            :head {:comp {:synsem {:pronoun false}}}})
                 :from-language "it"))
              (range 0 (Integer. at-a-time)))))))))



        


