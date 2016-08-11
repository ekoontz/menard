(in-ns 'babel.english)
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





