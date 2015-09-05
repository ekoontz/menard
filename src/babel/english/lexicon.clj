(ns babel.english.lexicon
  (:require
   [babel.lexiconfn :refer (unify)]
   [babel.english.pos :refer :all]))

(def lexicon-source
  {
   "Antonia"
   {:synsem {:sem {:pred :antonia
                   :human true}
             :agr {:number :sing
                   :person :3rd
                   :gender :fem}
             :propernoun true}}

   "Antonio"
   {:synsem {:agr {:number :sing
                   :person :3rd
                   :gender :masc}
             :sem {:pred :antonio
                   :human true}
             :propernoun true}}

   "a"
   {:synsem {:cat :det
             :def :indef
             :mass false
             :number :sing}}

   "abandon" {:synsem {:cat :verb
                       :sem {:pred :abandon}}}

   "accept"  {:synsem {:cat :verb
                       :sem {:pred :accept}}}
   
   "accompany" {:synsem {:cat :verb
                         :sem {:pred :accompany}}
                :english {:past "accompanied"}}
   
   "add" {:synsem {:cat :verb
                   :sem {:pred :add}}}

   "admire" {:synsem {:cat :verb
                      :sem {:pred :admire}}}

   "announce" {:synsem {:cat :verb
                        :sem {:pred :announce}}}

   "answer" {:synsem {:cat :verb
                      :sem {:pred :answer
                            :subj {:human true}}}}

   "approve" {:synsem {:cat :verb
                :sem {:pred :approve}}}

   "ask" {:synsem {:cat :verb
                   :sem {:pred :chiedere
                         :subj {:human true}}}}

   "assure" {:synsem {:cat :verb
                      :sem {:pred :assure}}}

   "attend" {:synsem {:cat :verb
                      :sem {:pred :frequentare}}}

   "avoid" {:synsem {:cat :verb
                     :sem {:pred :avoid}}}

   "bag" {:synsem {:cat :noun
                   :sem {:pred :bag
                         :place false}}}

   "base" {:synsem {:cat :verb
                    :sem {:pred :support}}}
   
   "be"
   {:synsem {:cat :verb
             :sem {:pred :essere}}
    :english {:present {:1sing "am"
                        :2sing "are"
                        :3sing "is"
                        :1plur "are"
                        :2plur "are"
                        :3plur "are"}
              :past {:1sing "was"
                     :2sing "were"
                     :3sing "was"
                     :1plur "were"
                     :2plur "were"
                     :3plur "were"}}}
   
   "be missed" {:synsem {:cat :verb
                         :sem {:pred :mancare}}
                :english {:participle "be missed"
                          :future "be missed"
                          :conditional "be missed"
                          :past {:english "was missed"
                                 :2sing "were missed"
                                 :2plur "were missed"
                                 :3plur "were missed"}
                          :present {:1sing "am missed"
                                    :2sing "are missed"
                                    :3sing "is missed"
                                    :1plur "are missed"
                                    :2plur "are missed"
                                    :3plur "are missed"}}}

   ;; TODO: for imperfect, generates things like 'he was be missinging'.
   "be missing" {:english {:imperfect {:1sing "was missing"
                                       :2sing "were missing"
                                       :3sing "was missing"
                                       :1plur "were missing"
                                       :2plur "were missing"
                                       :3plur "were missing"}
                           :present {:1sing "am missing"
                                     :2sing "are missing"
                                     :3sing "is missing"
                                     :1plur "are missing"
                                     :2plur "are missing"
                                     :3plur "are missing"}
                           :past {:1sing "was missing"
                                  :2sing "were missing"
                                  :3sing "was missing"
                                  :1plur "were missing"
                                  :2plur "were missing"
                                  :3plur "were missing"}}
                 :synsem {:cat :verb
                          :sem {:pred :to-be-missing}}}

   "begin" {:synsem {:cat :verb
                     :sem {:pred :begin}}
            :english {:past "began"}}

   "bicycle" {:synsem {:cat :noun
                       :sem {:pred :bicycle
                             :place false}}}

   "black"
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :nero
                          :comparative false
                          :physical-object true
                          :human false}}})

   "book"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem {:pred :libro
                          :legible true
                          :speakable false
                          :mass false
                          :buyable true
                          :place false
                          :consumable false
                          :artifact true}}})

   "break" {:synsem {:cat :verb
                     :sem {:pred :break }}
            :english {:past "break"}}

   "buy" {:synsem {:cat :verb
                   :sem {:pred :comprare
                         :subj {:human true}
                         :obj {:buyable true}}}
          :english {:past "bought"
                    :present {:3sing "buys"}}}

   "call" {:synsem {:cat :verb
                     :sem {:pred :call}}}

   "car" {:synsem {:cat :noun
                   :sem {:pred :car
                         :place false}}}

   "carry" {:synsem {:cat :verb
                     :sem {:pred :carry}}
            :english {:past "carried"}}

   "cat"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem (unify animal {:pred :gatto
                                        :pet true})}})

   "change" {:synsem {:cat :verb
                      :sem {:pred :cambiare}}} ;; TODO: add reflexive sense

   "chat" {:synsem {:cat :verb
                    :english {:participle "chatting"
                              :past "chatted"}
                    :sem {:pred :chat}}}

   "charge" {:synsem {:cat :verb
                      :sem {:pred :caricare}}}

   "check" {:synsem {:cat :verb
                     :sem {:pred :check}}}

   "come" {:synsem {:cat :verb
                    :sem {:pred :venire}}
           :english {:past "came"}}

   "comment" {:synsem {:cat :verb
                       :sem {:pred :comment}}}

   "conserve" {:synsem {:cat :verb
                        :sem {:pred :conserve}}}

   "consider" {:synsem {:cat :verb
                        :sem {:pred :consider}}}

   "correspond" {:synsem {:cat :verb
                          :sem {:pred :correspond}}}

   "create" {:synsem {:cat :verb
                      :sem {:pred :create}}}

   "cry" {:synsem {:cat :verb
                    :sem {:pred :cry}}}

   "cut" {:english {:past "cut"
                    :participle "cutting"}
          :synsem {:cat :verb
                   :sem {:pred :cut}}}

   "decide" {:synsem {:cat :verb
                      :sem {:pred :decide}}}

   "desire" {:synsem {:cat :verb
                      :sem {:pred :desire}}}

   "develop" {:synsem {:cat :verb
                       :sem {:pred :develop}}}

   "dine" {:synsem {:cat :verb
                    :sem {:pred :cenare
                          :subj {:human true}}}}

   "displace" {:synsem {:cat :verb
                :sem {:pred :displace}}}

   "divide" {:synsem {:cat :verb
                      :sem {:pred :divide}}}
   
   "drink" {:synsem {:cat :verb
                     :sem {:pred :bere
                           :discrete false
                           :subj {:animate true}
                           :obj {:drinkable true}}}
            :english {:past "drank"}}

   "drive" {:synsem {:cat :verb
                     :sem {:pred :guidare}}
            :english {:past "drove"}}

   "disappoint" {:synsem {:cat :verb
                          :sem {:pred :deludere}}}
   "download" {:synsem {:cat :verb
                        :sem {:pred :scaricare}}}

   "dog"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem (unify animal {:pred :cane
                                        :pet true})}})


   "earn"  {:synsem {:cat :verb
                     :sem {:pred :earn
                           :subj {:human true
                                  :obj false}}}}

   "eat"
   {:english {:past "ate"}
    :synsem {:cat :verb
             :sem {:pred :mangiare
                   :subj {:animate true}
                   :obj {:edible true}}}}

   "eat dinner" {:synsem {:cat :verb
                          :sem {:pred :cenare
                                :subj {:human true}}}
                 :english {:present {:3sing "eats dinner"}
                           :participle "eating dinner"
                           :past "ate dinner"}}

   "embrace"
   {:synsem {:cat :verb
             :sem {:pred :abbracciare}
             :subj {:human true}
             :obj {:human true}}}

   "endure" {:synsem {:cat :verb
                :sem {:pred :endure}}}

   "engage" {:synsem {:cat :verb
                :sem {:pred :engage}}}

   "enjoy" {:english {:present {:3sing "enjoys"}}
            :synsem {:cat :verb
                     :sem {:pred :enjoy}}}
  
   "enter"  {:synsem {:cat :verb
                      :sem {:pred :enter}}}

   "erase"  {:synsem {:cat :verb
                      :sem {:pred :cancellare}}}

   "escape" {:synsem {:cat :verb
                      :sem {:pred :escape}}}

   "exist" {:synsem {:cat :verb
                     :sem {:pred :exist}}}

   "express" {:synsem {:cat :verb
                       :sem {:pred :express}}}

  "faint" {:synsem {:cat :verb
                    :sem {:pred :faint}}}

   "finish" {:synsem {:cat :verb
                      :sem {:pred :finish}}}

   "fold" {:synsem {:cat :verb
                    :sem {:pred :fold}}}

   "forget" {:synsem {:cat :verb
                      :sem {:pred :forget}}
             :english {:past "forgot"}}

   "form" {:synsem {:cat :verb
                    :sem {:pred :form}}}


   "furnish"  {:synsem {:cat :verb
                        :sem {:pred :furnish}}}

   "game" {:synsem {:cat :noun
                    :sem {:pred :game
                          :games true}}}

   "get dressed"
   (let [subject-semantics (ref {:human true})]
     {:synsem {:cat :verb
               :sem {:pred :get-dressed
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting dressed"
                :present {:3sing "gets dressed"}
                :past "got dressed"}})
   "get ready"
   (let [subject-semantics (ref {:human true})]
     {:synsem {:cat :verb
               :sem {:pred :get-ready
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting ready"
                :present {:3sing "gets ready"}
                :past "got ready"}})
   "get up"
   (let [subject-semantics (ref {:animate true})]
     {:synsem {:cat :verb
               :sem {:pred :get-up
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting up"
                :present {:3sing "gets up"}
                :past "got up"}})

   ;; TODO: account for "give" being ditransitive.
   "give" {:synsem {:cat :verb
                    :sem {:pred :dare}}
           :english {:past "gave"}}

   "go"
   {:synsem {:cat :verb
              :sem {:activity true
                    :discrete false
                    :pred :andare
                    :subj {:animate true}}}
    :english {:past "went"}}

   "grab"  {:synsem {:cat :verb
                     :sem {:pred :prendere}}
            :english {:participle "grabbing"
                      :past "grabbed"}}

   "have" {:synsem {:cat :verb
                    :sem {:activity false
                          :discrete false
                          :pred :avere
                          :subj {:human true}
                          :obj {:buyable true}}}
           :english {:present {:3sing "has"}
                             :past "had"}}

   "have dinner" {:synsem {:cat :verb
                            :sem {:pred :cenare}}
                   :english {:present {:3sing "has dinner"}
                             :past "had dinner"
                             :participle "having dinner"}}
   "have fun"
   (let [subject-semantics (ref {:human true})]
     {:synsem {:cat :verb
               :sem {:pred :have-fun
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "having fun"
                :present {:3sing "has fun"}
                :past "had fun"}})
   
   "have to" {:synsem {:cat :verb
                       :sem {:pred :have-to}}
              :english {:present {:1sing "have to"
                                  :2sing "have to"
                                  :3sing "has to"
                                  :1plur "have to"
                                  :2plur "have to"
                                  :3plur "have to"}
                        :future "have to"
                        :participle "having to"
                        :past "had to"}}

   "he"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :masc
                   :number :sing}
             :sem {:human true
                   :pred :lui}
             :subcat '()}}

   "help"
   {:synsem {:cat :verb
             :essere false
             :sem {:pred :aiutare
                   :activity true
                   :obj {:human true}}}}

   "herself"
   {:synsem {:cat :noun
             :pronoun true
             :case :acc
             :reflexive true
             :agr {:person :3rd
                   :gender :fem
                   :number :sing}
             :sem {:human true
                   :pred :lei}
             :subcat '()}}

   "himself"
   {:synsem {:cat :noun
             :pronoun true
             :case :acc
             :reflexive true
             :agr {:person :3rd
                   :gender :masc
                   :number :sing}
             :sem {:human true
                   :pred :lui}
             :subcat '()}}

   
   "hold"
   {:synsem {:cat :verb
             :sem {:pred :tenere}}
    :english {:past "held"}}

   "hope"
   {:synsem {:cat :verb
             :sem {:pred :hope}}}
   
   "hug"
   {:synsem {:cat :verb
             :sem {:pred :abbracciare
                   :subj {:human true}
                   :obj {:animate true}}}
    :english {:past "hugged"
              :participle "hugging"}}
   "I" 
   [{:english {:note "♂"}
     :synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :masc
                    :person :1st
                    :number :sing}
              :sem {:human true
                    :pred :io}
             :subcat '()}}

    {:english {:note "♀"}
     :synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :fem
                    :person :1st
                    :number :sing}
              :sem {:human true
                    :pred :io}
              :subcat '()}}]

   "imagine" {:synsem {:cat :verb
                     :sem {:pred :imagine}}}

   "import" {:synsem {:cat :verb
                :sem {:pred :import}}}

   "improve" {:synsem {:cat :verb
                :sem {:pred :improve}}}

   "increase" {:synsem {:cat :verb
                        :sem {:pred :increase}}}

   "insist" {:synsem {:cat :verb
                      :sem {:pred :insist}}}

   "insure" {:synsem {:cat :verb
                      :sem {:pred :insure}}}

   "interrupt" {:synsem {:cat :verb
                :sem {:pred :interrupt}}}
   "it (♂)"
   {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing}
              :sem {:pred :lui
                    :gender :masc
                    :human false}
              :subcat '()}}

   "it (♀)"
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing}
              :sem {:pred :lei
                    :gender :fem
                    :human false}
              :subcat '()}}

   "itself"
   [{:note "♂"
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :3rd
                    :gender :masc
                    :number :sing}
              :sem {:human false
                    :pred :lui}
              :subcat '()}}
    {:note "♀"
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :3rd
                    :gender :fem
                    :number :sing}
              :sem {:human false
                    :pred :lei}
              :subcat '()}}]

   "keep"
   [{:synsem {:cat :verb
              :sem {:pred :tenere}}
     :english {:past "kept"}}
    {:synsem {:cat :verb
              :sem {:pred :keep-safe}}
     :english {:note "(something safe)"
               :past "kept"}}]

   "key" {:synsem {:cat :noun
                   :sem {:pred :key
                         :place false}}}

   "kill" {:synsem {:cat :verb
                    :sem {:pred :kill}}}

   "learn" {:synsem {:cat :verb
                     :sem {:pred :imparare}}}

   "leave" {:english {:past "left"}
            :synsem {:cat :verb
                     :sem {:pred :leave-behind
                           :obj {:place false}}}}

   "lie" {:synsem {:cat :verb
                :sem {:pred :lie}}}
   
   "lift" {:synsem {:cat :verb
                    :sem {:pred :lift}}}
             
   "listen to" {:synsem {:cat :verb
                         :sem {:pred :listen-to}}
                :english {:participle "listening to"
                          :past "listened to"
                          :present {:3sing "listens to"}}}

   "look" {:synsem {:cat :verb
                    :sem {:pred :look}}}

   "look for" {:synsem {:cat :verb
                        :sem {:pred :cercare}}
               :english {:participle "looking for"
                         :past "looked for"
                         :present {:3sing "looks for"}}}

   "look up" {:synsem {:cat :verb
                       :sem {:pred :cercare}}
              :english {:participle "looking up"
                        :past "looked up"
                        :present {:3sing "looks up"}}}

   "love" {:synsem {:cat :verb
                    :sem {:pred :amare
                          :subj {:human true}}}}

   "lower" {:synsem {:cat :verb
                     :sem {:pred :lower}}}

   "manage" {:synsem {:cat :verb
                :sem {:pred :manage}}}
   "me" 
   {:synsem {:cat :noun
             :pronoun true
             :case :acc
             :reflexive false
             :agr {:person :1st
                   :number :sing}
             :sem {:human true
                   :pred :io}
             :subcat '()}}

   "measure" {:synsem {:cat :verb
                :sem {:pred :measure}}}

   "meet"  {:synsem {:cat :verb
                     :sem {:pred :incontrare}}
            :english {:past "met"}}

   "mother"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem {:human true
                          :pred :madre
                          :child false}}})

   "move" {:synsem {:cat :verb
                    :sem {:pred :move}}}

   "multiply" {:synsem {:cat :verb
                :sem {:pred :multiply}}}

   "music" 
   {:synsem {:cat :noun
             :sem {:pred :music}}}
   
   "myself" 
   {:synsem {:cat :noun
             :pronoun true
             :case :acc
             :reflexive true
             :agr {:person :1st
                   :number :sing}
             :sem {:human true
                   :pred :io}
             :subcat '()}}

   "note" {:synsem {:cat :verb
                    :sem {:pred :note}}}

   "observe" {:synsem {:cat :verb
                :sem {:pred :observe}}}

   "obtain" {:synsem {:cat :verb
                :sem {:pred :obtain}}}

   "organize" {:synsem {:cat :verb
                :sem {:pred :organize}}}

   "ourselves"
   [{:note "♀" 
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :1st
                    :number :plur
                    :gender :fem}
              :sem {:human true
                    :pred :noi}
              :subcat '()}}
    
    {:note "♂" 
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :1st
                    :number :plur
                    :gender :masc}
              :sem {:human true
                    :pred :noi}
              :subcat '()}}]
   
   "paint"  {:synsem {:cat :verb
                      :sem {:pred :dipingere}}}

   "participate"  {:synsem {:cat :verb
                            :sem {:pred :participate}}}
   
   ;; TODO: 3sing present exception used below to avoid "playies" is not an exception: it's a rule: y->ys.
   ;; the exceptional case is when "ys" is not used (e.g. "tries").
   "play" [{:comment "We are talking about playing games or sports."
            :synsem {:cat :verb
                     :sem {:pred :giocare
                           :subj {:human true}
                           :obj {:games true}}}}

           {:comment "We are talking about playing music or sounds."
            :synsem {:cat :verb
                     :sem {:pred :suonare
                           :subj {:human true}
                           :obj {:music true}}}}]

   "prepare" (let [subject-semantics (ref {:human true})]
               {:synsem {:cat :verb
                         :sem {:pred :get-ready
                               :subj subject-semantics
                               :obj subject-semantics}
                         :subcat {:1 {:sem subject-semantics}
                                  :2 {:pronoun true
                                      :reflexive true
                                      :sem subject-semantics}}}})
   "preserve" {:synsem {:cat :verb
                        :sem {:pred :preserve}}}

   "print"  {:synsem {:cat :verb
                      :sem {:pred :stampare}}}

   "read" ;; if this was a phonetic dictionary, there would be two entries for each pronounciation (i.e. both "reed" or "red" pronounciations)
   {:english {:past "read (past)"}
    :synsem {:cat :verb
             :sem {:pred :leggere
                   :discrete false
                   :subj {:human true}
                   :obj {:legible true}}}}
   
   "receive"  {:synsem {:cat :verb
                        :sem {:pred :ricevere}}}

   "reciprocate" {:synsem {:cat :verb
                           :sem {:pred :reciprocate}}}

   "recognize" {:synsem {:cat :verb
                :sem {:pred :recognize}}}

   "recount" {:synsem {:cat :verb
                :sem {:pred :recount}}}

   "recover" {:synsem {:cat :verb
                :sem {:pred :recover}}}
   "red"
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :rosso
                          :comparative false
                          :physical-object true
                          :human false}}})

   "remember"  {:synsem {:cat :verb
                         :sem {:pred :ricordare}}}

   "reserve" {:synsem {:cat :verb
                :sem {:pred :reserve}}}

   "respond"  {:synsem {:cat :verb
                        :sem {:pred :answer}}}

   "rest" {:synsem {:cat :verb
                :sem {:pred :rest}}}

   "return" [{:synsem {:cat :verb
                       :sem {:pred :ritornare}}}
             {:synsem {:cat :verb
                       :sem {:pred :tornare}}}
             {:synsem {:cat :verb
                       :sem {:pred :giveback-return}}
              :english {:note "(give back)"}}]

   "run" {:english {:past "ran"
                    :participle "running"
                    :past-participle "run"}
          :synsem {:cat :verb
                   :sem {:pred :run}}}

   "scold" {:synsem {:cat :verb
                :sem {:pred :scold}}}

   ;; TODO: search _within_ or _on_: depends on the object.
   ;;   "search"  {:synsem {:sem {:pred :cercare}}})

   "see"  {:synsem {:cat :verb
                    :sem {:pred :vedere}}
           :english {:past "saw"
                     :past-participle "seen"}}

   "sell" {:synsem {:cat :verb
                    :sem {:pred :vendere
                          :subj {:human true}
                          :obj {:human false}}}
           :english {:past "sold"}}

   "send" {:synsem {:cat :verb
                    :sem {:pred :send}}
           :english {:past "sent"}}

   "set" {:synsem {:cat :verb
                     :sem {:pred :set}}}

   "share" {:synsem {:cat :verb
                     :sem {:pred :share}}}
   "she"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :sing}
             :sem {:human true
                   :pred :lei}
             :subcat '()}}

   "show" {:synsem {:cat :verb
                    :sem {:pred :show}}}

   "sigh" {:synsem {:cat :verb
                :sem {:pred :sigh}}}

   "sing" {:synsem {:cat :verb
                    :sem {:pred :cantare}}
           :english {:past "sang"}}

   "sleep" {:synsem {:cat :verb
                     :sem {:subj {:animate true}
                           :discrete false
                           :pred :dormire}}
            :english {:past "slept"}}

   "snap" {:synsem {:cat :verb
                    :sem {:pred :snap-pictures}}
           :english {:past "snapped"
                     :note "pictures"}}

   "some"
   [{:synsem {:cat :det
              :def :partitivo
              :number :plur}}
    {:synsem {:cat :det
              :def :indef
              :number :sing}}]

   "speak"
   {:english {:past "spoke"
              :past-participle "spoken"}
    :synsem {:cat :verb
             :sem {:pred :speak
                   :subj {:human true}
                   :obj {:speakable true}}}}

   "stain" {:synsem {:cat :verb
                :sem {:pred :stain}}}

   "steal" {:synsem {:cat :verb
                     :sem {:pred :steal}}
            :english {:past "stole"}}

   "strike" {:english {:past "struck"}
             :synsem {:cat :verb
                      :sem {:pred :strike}}}

   "study"  {:synsem {:cat :verb
                      :sem {:pred :study}}
             :english {:past "studied"}}

   "supply" {:synsem {:cat :verb
                :sem {:pred :supply}}}

   "support" {:synsem {:cat :verb
                       :sem {:pred :support}}}

   "take"  {:synsem {:cat :verb
                     :sem {:pred :take}}
            :english {:past "took"}}

   "take advantage of" {:english {:past "took advantage of"
                                  :participle "taking advantage of"
                                  :past-participle "taken advantage of"
                                  :present {:3sing "takes advantage of"}}
                        :synsem {:cat :verb
                                 :sem {:pred :take-advantage-of}}}

   "talk"
   {:synsem {:cat :verb
             :sem {:pred :talk
                   :subj {:human true}}}}
   
   "teach"  {:synsem {:cat :verb
                      :sem {:pred :teach}}
                            :english {:past "taught"}}

   "telephone" {:synsem {:cat :verb
                         :sem {:pred :telefonare}}}

   "the"
   {:synsem {:cat :det
             :def :def
             :mass false}}

   "themselves"
   [{:note "♀" 
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :1st
                    :number :plur
                    :gender :fem}
              :sem {:human true
                    :pred :loro}
              :subcat '()}}
    
    {:note "♂" 
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :1st
                    :number :plur
                    :gender :masc}
              :sem {:human true
                    :pred :loro}
              :subcat '()}}]
   
   "they (♂)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :masc
                   :number :plur}
             :sem {:gender :masc
                   :human true
                   :pred :loro}
             :subcat '()}}

   "they (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :plur}
             :sem {:gender :fem
                   :human true
                   :pred :loro}
             :subcat '()}}

   "throw" {:english {:past "threw"}
            :synsem {:cat :verb
                     :sem {:pred :throw}}}

   "throw out"
   {:synsem {:cat :verb
             :sem {:pred :throw-out}}
    :english {:past "threw out"
              :present {:3sing "throws out"}
              :participle "throwing out"}}

   "transfer" {:english {:past "transferred"
                         :participle "transferring"}
               :synsem {:cat :verb
                        :sem {:pred :transfer}}}

   "try" {:synsem {:cat :verb
                   :sem {:pred :try}}
          :english {:past "tried"}}
            
   "understand" {:english {:past "understood"}
                 :synsem {:cat :verb
                          :sem {:pred :understand}}}

   "upload"  {:synsem {:cat :verb
                       :sem {:pred :caricare}}}

   "use"  {:synsem {:cat :verb
                    :sem {:pred :usare}}}

   "wait"  {:synsem {:cat :verb
                     :sem {:pred :wait-for}}}
   "wake up"
   (let [subject-semantics (ref {:animate true})]
     {:synsem {:cat :verb
               :sem {:pred :wake-up
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "waking up"
                :present {:3sing "wakes up"}
                :past "woke up"}})

   "walk" {:synsem {:cat :verb
                :sem {:pred :walk}}}

   "warm" {:synsem {:cat :verb
                :sem {:pred :warm}}}

   "wash" (let [subject-semantics (ref :top)]
            {:synsem {:cat :verb
                      :sem {:pred :wash
                            :subj subject-semantics
                            :obj subject-semantics}
                      :subcat {:1 {:sem subject-semantics}
                               :2 {:pronoun true
                                   :reflexive true
                                   :sem subject-semantics}}}})
   "waste" {:synsem {:cat :verb
                :sem {:pred :waste}}}

   "watch" {:synsem {:cat :verb
                    :sem {:pred :watch}}}
   "we (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :gender :fem
                   :pred :noi}
             :subcat '()}}
   "we (♂)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :gender :masc
                   :number :plur}
             :sem {:human true
                   :gender :masc
                   :pred :noi}
             :subcat '()}}

   "wear"  {:english {:past "wore"}
            :synsem {:cat :verb
                     :sem {:pred :wear}}}
   
   "win"  {:synsem {:cat :verb
                    :sem {:pred :win
                          :subj {:human true}
                          :obj {:human false}}}
           :english {:past "won"
                     :participle "winning"}}
   
   "woman"
   (unify agreement-noun
          common-noun
          countable-noun
          {:english {:plur "women"}
           :synsem {:sem {:human true
                          :pred :donna
                          :child false}}})

   "work" [
           {:synsem {:cat :verb
                     :sem {:pred :work-human
                           :subj {:human true}}}
            :english {:note "(human)"}}
           
           {:english {:note "(nonliving or machines)"} ;; TODO: add support in UI for :note.
            :synsem {:cat :verb
                     :sem {:subj {:living false
                                  :human false ;; should not need to add human=false and animate=false: living=false should suffice.
                                  :animate false}
                           :pred :work-nonhuman}}}]
   
   "write"  {:english {:past "wrote"
                       :past-participle "written"}
             :synsem {:cat :verb
                      :sem {:pred :scrivere}}}

   "yell" {:synsem {:cat :verb
                :sem {:pred :yell}}}
   
   "you (♂)"
   {:note "♂"
    :target :it ;; Italian makes gender distinction for agreement with verbs and adjectives..
    :synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :masc
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}}

   "you (♀)"
   {:note "♀"
    :target :it ;; Italian makes gender distinction for agreement with verbs and adjectives..
    :synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :fem
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}}

   "you"
   {:target :es ;; ..but Spanish does not.
    :synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :fem
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}}

   "you all (♂)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :masc
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}

   "you all (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}

   "yourself"
   [{:note "♀"
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :2nd
                    :number :sing
                    :gender :fem}
              :sem {:human true
                   :pred :tu}
              :subcat '()}}
    
    {:note "♂"
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :2nd
                    :number :sing
                    :gender :masc}
              :sem {:human true
                    :pred :tu}
              :subcat '()}}]

   "yourselves"
   [{:note "♀" 
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :2nd
                    :number :plur
                    :gender :fem}
              :sem {:human true
                    :pred :voi}
              :subcat '()}}
    
    {:note "♂" 
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :2nd
                    :number :plur
                    :gender :masc}
              :sem {:human true
                    :pred :voi}
              :subcat '()}}]
   })


    

