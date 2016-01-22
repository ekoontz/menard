(ns babel.english.lexicon
  (:refer-clojure :exclude [get-in merge])
  (:require
   [babel.lexiconfn :refer (compile-lex if-then
                                        map-function-on-map-vals unify)]
   [babel.english.morphology :as morph]
   [babel.english.pos :refer [adjective
                              agreement-noun
                              animal
                              common-noun
                              countable-noun
                              intransitivize
                              subject-verb-agreement
                              transitivize]]
   [dag_unify.core :refer [fail? get-in merge strip-refs]]))

#?(:cljs
   (defn- future [expr]
     expr))

(def lexicon-source
  {
   "Antonia"
   {:synsem {:sem {:pred :antonia
                   :human true}
             :propernoun true
             :agr {:number :sing
                   :person :3rd
                   :gender :fem}}}
   "Antonio"
   {:synsem {:propernoun true
             :agr {:number :sing
                   :person :3rd
                   :gender :masc}
             :sem {:pred :antonio}
                   :human true}}
   "a"
   {:synsem {:cat :det
             :def :indef
             :mass false
             :agr {:number :sing}}}

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

   "arrive" {:synsem {:cat :verb
                      :sem {:pred :arrive}}}
   
   "ask" {:synsem {:cat :verb
                   :sem {:pred :chiedere
                         :subj {:human true}}}}

   "assure" {:synsem {:cat :verb
                      :sem {:pred :assure}}}

   "attend" {:synsem {:cat :verb
                      :sem {:pred :attend}}}

   "avoid" {:synsem {:cat :verb
                     :sem {:pred :avoid}}}

   "bag" {:synsem {:cat :noun
                   :sem {:pred :bag
                         :place false}}}

   "base" {:synsem {:cat :verb
                    :sem {:pred :support}}}
   
   "be" (let [number-agr (atom :top)
              common {:synsem {:cat :verb}
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
                                       :3plur "were"}}}]
          [;; intransitive
           (unify common
                  {:synsem {:subcat {:1 {:cat :noun}
                                     :2 '()}
                            :sem {:pred :be}}})


           ;; be + propernoun, e.g. "I am John"
           (let [subj-agr (atom :top)
                 infl (atom :top)
                 the-real-subj (atom :top)
                 the-obj (atom :top)]
             (unify common
                    subject-verb-agreement
                    {:intransitivize false
                     :transitivize false
                     :synsem {:agr subj-agr
                              :sem {:aspect :progressive
                                    :pred :be-called
                                    :tense :present
                                    :subj the-real-subj
                                    :obj the-obj}
                              :subcat {:1 {:cat :noun
                                           :agr subj-agr
                                           :sem {:pred :name
                                                 :subj the-real-subj}

                                           }
                                       :2 {:cat :noun
                                           :agr subj-agr
                                           :sem the-obj
                                           :propernoun true ;; "I am John"
                                           }
                                       } ;; subcat {
                              } ;; synsem {
                     } ;; end of map
                    ))])

   "be able to" {:english {:imperfect {:1sing "was able to"
                                       :2sing "were able to"
                                       :3sing "was able to"
                                       :1plur "were able to"
                                       :2plur "were able to"
                                       :3plur "were able to"}
                           :present {:1sing "am able to"
                                     :2sing "are able to"
                                     :3sing "is able to"
                                     :1plur "are able to"
                                     :2plur "are able to"
                                     :3plur "are able to"}
                           :past {:1sing "was able to"
                                  :2sing "were able to"
                                  :3sing "was able to"
                                  :1plur "were able to"
                                  :2plur "were able to"
                                  :3plur "were able to"}}
                 :synsem {:cat :verb
                          :sem {:pred :be-able-to}}}
   
   "be called" {:synsem {:cat :verb
                         :sem {:pred :mancare}}
                :english {:future "be called"
                          :participle "being called"
                          :conditional "be called"
                          :imperfect {:1sing "was being called"
                                      :2sing "were being called"
                                      :3sing "was being called"
                                      :1plur "were being called"
                                      :2plur "were being called"
                                      :3plur "were being called"}
                          :past {:english "was called"
                                 :2sing "were called"
                                 :2plur "were called"
                                 :3plur "were called"}
                          :present {:1sing "am called"
                                    :2sing "are called"
                                    :3sing "is called"
                                    :1plur "are called"
                                    :2plur "are called"
                                    :3plur "are called"}}}

   "be missed" {:synsem {:cat :verb
                         :sem {:pred :mancare}}
                :english {:future "be missed"
                          :participle "being missed"
                          :conditional "be missed"
                          :imperfect {:1sing "was being missed"
                                      :2sing "were being missed"
                                      :3sing "was being missed"
                                      :1plur "were being missed"
                                      :2plur "were being missed"
                                      :3plur "were being missed"}
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

   "believe" {:synsem {:cat :verb
                       :sem {:pred :believe}}}
   
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

   "boil" {:synsem {:cat :verb
                :sem {:pred :boil}}}
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
            :english {:past "broke"}}

   "bring" {:synsem {:cat :verb
                   :sem {:pred :bring
                         :subj {:human true}
                         :obj {:buyable true}}}
          :english {:past "brought"}}
   
   "buy" {:synsem {:cat :verb
                   :sem {:pred :comprare
                         :subj {:human true}
                         :obj {:buyable true}}}
          :english {:past "bought"
                    :present {:3sing "buys"}}}

   "call" {:synsem {:cat :verb
                     :sem {:pred :call}}}

   "can" 
   {:english {:past "could"
              :present {:3sing "can"}
              :future "be able to"
              :conditional "could"}
    :synsem {:cat :verb
                   
                     :sem {:pred :can}}}

   "car" {:synsem {:cat :noun
                   :sem {:pred :car
                         :place false}}}

   "carry" {:synsem {:cat :verb
                     :sem {:human true
                           :pred :carry}}
            :english {:past "carried"}}
   "cat"
   (unify agreement-noun
          common-noun
          countable-noun
          {:synsem {:sem (unify animal {:pred :gatto
                                        :pet true})}})

   "change" {:synsem {:cat :verb
                      :sem {:pred :change}}} ;; TODO: add reflexive sense

   "change clothes" {:synsem {:cat :verb
                              :sem {:pred :change-clothes
                                    :subj {:human true}}}
                     :english {:present {:3sing "changes clothes"}
                               :participle "changing clothes"
                               :past "changed clothes"}}
   
   "chat" {:synsem {:cat :verb
                    :sem {:pred :chat}}
           :english {:participle "chatting"
                     :past "chatted"}}

   "charge" {:synsem {:cat :verb
                      :sem {:pred :caricare}}}

   "check" {:synsem {:cat :verb
                     :sem {:pred :check}}}
                   
   "close" {:synsem {:cat :verb
                     :sem {:pred :close}}}                

   "come" {:synsem {:cat :verb
                    :sem {:pred :come}}
           :english {:past "came"}}

   "comment" {:synsem {:cat :verb
                       :sem {:pred :comment}}}

   "confess" {:synsem {:cat :verb
                       :sem {:pred :confess}}}

   "consent" {:synsem {:cat :verb
                        :sem {:pred :consent}}}
   
   "conserve" {:synsem {:cat :verb
                        :sem {:pred :conserve}}}

   "consider" {:synsem {:cat :verb
                        :sem {:pred :consider}}}

   "continue" {:synsem {:cat :verb
                      :sem {:pred :continue}}}
   
   "convert" {:synsem {:cat :verb
                       :sem {:pred :convert}}}

   "correspond" {:synsem {:cat :verb
                          :sem {:pred :correspond}}}

   "count" {:synsem {:cat :verb
                      :sem {:pred :count}}}
   
   "create" {:synsem {:cat :verb
                      :sem {:pred :create}}}

   "cry" {:synsem {:cat :verb
                    :sem {:pred :cry}}}

   "cut" {:english {:past "cut"
                    :participle "cutting"}
          :synsem {:cat :verb
                   :sem {:pred :cut}}}

   "dance" {:synsem {:cat :verb
                      :sem {:pred :dance}}}
   
   "decide" {:synsem {:cat :verb
                      :sem {:pred :decide}}}

   "defend" {:synsem {:cat :verb
                      :sem {:pred :defend}}}

   "deny" {:synsem {:cat :verb
                    :sem {:pred :deny}}}

   "desire" {:synsem {:cat :verb
                      :sem {:pred :desire}}}

   "develop" {:synsem {:cat :verb
                       :sem {:pred :develop}}}

   "dictate" {:synsem {:cat :verb
                      :sem {:pred :dictate}}}
   
   "dine" {:synsem {:cat :verb
                    :sem {:pred :cenare
                          :subj {:human true}}}}

   "displace" {:synsem {:cat :verb
                :sem {:pred :displace}}}

   "divide" {:synsem {:cat :verb
                      :sem {:pred :divide}}}
   
   "drink" {:synsem {:cat :verb
                     :sem {:pred :drink
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
                           :subj {:human true}}}}
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
                      :sem {:pred :erase}}}

   "escape" {:synsem {:cat :verb
                      :sem {:pred :escape}}}

   "exist" {:synsem {:cat :verb
                     :sem {:pred :exist}}}

   "express" {:synsem {:cat :verb
                       :sem {:pred :express}}}

   "faint" {:synsem {:cat :verb
                     :sem {:pred :faint}}}

   "fall asleep"
   (let [subject-semantics (atom {:animate true})]
     {:synsem {:cat :verb
               :sem {:pred :fall-asleep
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "falling asleep"
                :present {:3sing "falls asleep"}
                :past "fell asleep"}})
   
   "finish" {:synsem {:cat :verb
                      :sem {:pred :finish}}}

   "fold" {:synsem {:cat :verb
                    :sem {:pred :fold}}}
   
   "follow" {:synsem {:cat :verb
                      :sem {:pred :follow}}}
   
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
   "get angry"
   (let [subject-semantics (atom {:human true})]
     {:synsem {:cat :verb
               :sem {:pred :get-angry
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting angry"
                :present {:3sing "gets angry"}
                :past "got angry"}})
   
   "get dressed"
   (let [subject-semantics (atom {:human true})]
     {:synsem {:cat :verb
               :sem {:pred :get-dressed
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting dressed"
                :present {:3sing "gets dressed"}
                :past "got dressed"}})
   "get off"
   (let [subject-semantics (atom {:human true})]
     {:synsem {:cat :verb
               :sem {:pred :get-off
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting off"
                :present {:3sing "gets off"}
                :past "got off"}})
   "get on"
   (let [subject-semantics (atom {:human true})]
     {:synsem {:cat :verb
               :sem {:pred :get-on
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting on"
                :present {:3sing "gets on"}
                :past "got on"}})
   "get ready"
   (let [subject-semantics (atom {:human true})]
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
   (let [subject-semantics (atom {:animate true})]
     {:synsem {:cat :verb
               :sem {:pred :get-up
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting up"
                :present {:3sing "gets up"}
                :past "got up"}})

   "Gianluca"
   {:synsem {:agr {:number :sing
                   :person :3rd
                   :gender :masc}
             :sem {:pred :gianluca
                   :human true}
             :propernoun true}}

      "Giovanni and I"
      [{:synsem {:cat :noun
                 :pronoun true
                 :case :nom
                 :agr {:gender :masc
                       :person :1st
                       :number :plur}
                 :sem {:human true
                       :pred :giovanni-and-i}
                 :subcat '()}}]

      ;; TODO: account for "give" being ditransitive.
   "give" {:synsem {:cat :verb
                    :sem {:pred :give}}
           :english {:past "gave"}}

   "go"
   {:synsem {:cat :verb
              :sem {:activity true
                    :discrete false
                    :pred :go
                    :subj {:animate true}}}
    :english {:past "went"}}

   "go downstairs"
   {:synsem {:cat :verb
              :sem {:activity true
                    :discrete false
                    :pred :go-downstairs
                    :subj {:animate true}}}
    :english {:past "went downstairs"
              :present {:3sing "goes downstairs"}}}
   
   "go upstairs"
   {:synsem {:cat :verb
              :sem {:activity true
                    :discrete false
                    :pred :go-upstairs
                    :subj {:animate true}}}
    :english {:past "went upstairs"
              :present {:3sing "goes downstairs"}}}
   
   "grab"  {:synsem {:cat :verb
                     :sem {:pred :prendere}}
            :english {:participle "grabbing"
                      :past "grabbed"}}

   "guess" {:synsem {:cat :verb
                     :sem {:pred :guess}}}
   
   "have" {:synsem {:cat :verb
                    :sem {:activity false
                          :discrete false
                          :pred :have
                          :subj {:human true}
                          :obj {:buyable true}}}
           :english {:present {:3sing "has"}
                     :past "had"}}

   "have dinner" {:synsem {:cat :verb
                            :sem {:pred :have-dinner}}
                   :english {:present {:3sing "has dinner"}
                             :past "had dinner"
                             :participle "having dinner"}}
   "have fun"
   (let [subject-semantics (atom {:human true})]
     {:synsem {:cat :verb
               :sem {:pred :have-fun
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "having fun"
                :present {:3sing "has fun"}
                :past "had fun"}})
   
   "have lunch" {:synsem {:cat :verb
                            :sem {:pred :have-lunch}}
                   :english {:present {:3sing "has lunch"}
                             :past "had lunch"
                             :participle "having lunch"}}
   
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
    "her"
    (map #(unify %
                 {:synsem {:cat :det
                           :agr {:gender :fem}
                           :sem {:pred :lei}
                           :def :possessive}})
         [{:synsem {:agr {:number :sing}}}
          {:synsem {:agr {:number :plur}}}])
    
   "herself"
   {:synsem {:cat :noun
             :pronoun true
             :case :acc
             :reflexive true
             :agr {:person :3rd
                   :gender :fem
                   :number :sing}
             :sem {:human true}
             :subcat '()}}

   "himself"
   {:synsem {:cat :noun
             :pronoun true
             :case :acc
             :reflexive true
             :agr {:person :3rd
                   :gender :masc
                   :number :sing}
             :sem {:human true}
             :subcat '()}}
   "his"
   (map #(unify %
                {:synsem {:cat :det
                          :agr {:gender :masc}
                          :sem {:pred :lui}
                          :def :possessive}})
        [{:synsem {:agr {:number :sing}}}
         {:synsem {:agr {:number :plur}}}])
                 
   "hit" {:english {:past "hit"}
             :synsem {:cat :verb
                      :sem {:pred :hit}}}
   
   "hold"
   {:synsem {:cat :verb
             :sem {:pred :hold}}
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
            
   "hurt" (let [common {:english {:past "hurt"}
                        :synsem {:cat :verb}}]
            [(let [subject-semantics (atom {:human true})
                   subject-agr (atom :top)]
               (merge common
                      {:synsem {:sem {:pred :hurt-oneself
                                      :subj subject-semantics
                                      :obj subject-semantics}
                                :subcat {:1 {:agr subject-agr
                                             :sem subject-semantics}
                                         :2 {:agr subject-agr
                                             :pronoun true
                                             :reflexive true
                                             :sem subject-semantics}}}}))
             (merge common
                    {:synsem {:sem {:pred :hurt
                                    :obj {:animate true}}}})])

   "Jean" {:synsem {:sem {:pred :Jean
                          :human true}
                    :propernoun true
                    :agr {:number :sing
                          :person :3rd
                          :gender :masc}}}

   "Juan" {:synsem {:sem {:pred :Juan
                          :human true}
                    :propernoun true
                    :agr {:number :sing
                          :person :3rd
                          :gender :masc}}}
   "Juana" {:synsem {:sem {:pred :Juana
                          :human true}
                     :propernoun true
                     :agr {:number :sing
                           :person :3rd
                           :gender :fem}}}
   "Juan and I"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :masc
                    :person :1st
                    :number :plur}
              :sem {:human true
                    :pred :Juan-and-i}
              :subcat '()}}]        
   "I" 
   [{:english {:note "♂"}
     :synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :masc
                    :person :1st
                    :number :sing}
              :sem {:human true
                    :pred :I}
             :subcat '()}}

    {:english {:note "♀"}
     :synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :fem
                    :person :1st
                    :number :sing}
              :sem {:human true
                    :pred :I}
              :subcat '()}}]

   "imagine" {:synsem {:cat :verb
                       :sem {:pred :imagine
                             :subj {:human true}}}}

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
                     :sem {:pred :learn}}}

   "leave" {:english {:past "left"}
            :synsem {:cat :verb
                     :sem {:pred :leave-behind
                           :obj {:place false}}}}

   "lie" {:synsem {:cat :verb
                :sem {:pred :lie}}}
   
   "lift" {:synsem {:cat :verb
                    :sem {:pred :lift}}}
             
   "light" {:synsem {:cat :verb
                     :sem {:pred :light}}}
   
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

   "lose" {:english {:participle "losing"
                     :past "lost"}
           :synsem {:cat :verb
                    :sem {:pred :lose}}}

   "love" {:synsem {:cat :verb
                    :sem {:pred :amare
                          :subj {:human true}}}}

   "lower" {:synsem {:cat :verb
                     :sem {:pred :lower}}}
   "Luisa"
   {:synsem {:sem {:pred :luisa
                   :human true}
             :agr {:number :sing
                   :person :3rd
                   :gender :fem}
             :propernoun true}}

  "Luisa and I"
   [{:english {:note "♂"}
     :synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:gender :masc
                   :person :1st
                   :number :plur}
             :sem {:human true
                   :pred :luisa-and-i}
             :subcat '()}}
    {:english {:note "♀"}
     :synsem {:cat :noun
              :pronoun true
              :case :nom
             :agr {:gender :fem
                   :person :1st
                   :number :plur}
              :sem {:human true
                    :pred :luisa-and-i}
              :subcat '()}}]
   
   "manage" {:synsem {:cat :verb
                :sem {:pred :manage}}}
   "may" 
   {:english {:past "might"
              :present "may"
              :future "be able to"
              :conditional "might"}
    :synsem {:cat :verb
             :sem {:pred :may}}}
   "me" 
   {:synsem {:cat :noun
             :pronoun true
             :case :acc
             :reflexive false
             :agr {:person :1st
                   :number :sing}
             :sem {:human true
                   :pred :I}
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
   "my"
   (map #(unify %
                {:synsem {:cat :det
                          :sem {:pred :I}
                          :def :possessive}})
        [{:synsem {:agr {:number :sing}}}
         {:synsem {:agr {:number :plur}}}])
   
   "myself" 
   {:synsem {:cat :noun
             :pronoun true
             :case :acc
             :reflexive true
             :agr {:person :1st
                   :number :sing}
             :sem {:human true
                   :pred :I}
             :subcat '()}}
   "name"
   (let [of (atom :top)
         agr (atom :top)]
     (unify agreement-noun
            common-noun
            countable-noun
            {:synsem {:agr agr
                      :sem {:animate false
                            :pred :name
                            :subj of}
                      :subcat {:1 {:agr agr
                                   :cat :det
                                   :def :possessive
                                   :sem of}}}}))
   "note" {:synsem {:cat :verb
                    :sem {:pred :note}}}

   "observe" {:synsem {:cat :verb
                :sem {:pred :observe}}}

   "obtain" {:synsem {:cat :verb
                :sem {:pred :obtain}}}

   "organize" {:synsem {:cat :verb
                :sem {:pred :organize}}}

   "our"
   (map #(unify %
                {:synsem {:cat :det
                          :agr {:gender :masc}
                          :sem {:pred :noi}
                          :def :possessive}})
        [{:synsem {:agr {:number :sing}}}
         {:synsem {:agr {:number :plur}}}])

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
                      :sem {:pred :paint}}}

   "participate"  {:synsem {:cat :verb
                            :sem {:pred :participate}}}
   
   ;; TODO: 3sing present exception used below to avoid "playies" is not an exception: it's a rule: y->ys.
   ;; the exceptional case is when "ys" is not used (e.g. "tries").
   "play" [{:comment "We are talking about playing games or sports."
            :synsem {:cat :verb
                     :sem {:pred :giocare
                           :subj {:human true}
                           :obj {:human false
                                 :games true}}}}

           {:comment "We are talking about playing music or sounds."
            :synsem {:cat :verb
                     :sem {:pred :suonare
                           :subj {:human true}
                           :obj {:human false
                                 :music true}}}}]

   "prepare" (let [subject-semantics (atom {:human true})]
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

   "put" {:english {:past "put"
                    :participle "putting"}
          :synsem {:cat :verb
                   :sem {:pred :put}}}
   
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

 "remain" {:synsem {:cat :verb
                      :sem {:pred :remain}}}


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
              :english {:note "give back"}}]

   "run" {:english {:past "ran"
                    :participle "running"
                    :past-participle "run"}
          :synsem {:cat :verb
                   :sem {:pred :run}}}

   "say" {:english {:past "said"}
          :synsem {:cat :verb
                   :sem {:pred :say}}}
   
   "scold" {:synsem {:cat :verb
                :sem {:pred :scold}}}

   ;; TODO: search _within_ or _on_: depends on the object.
   ;;   "search"  {:synsem {:sem {:pred :cercare}}})

   "scrub"  {:synsem {:cat :verb
                     :sem {:pred :scrub}}
            :english {:participle "scrubbing"
                      :past "scrubbed"}}
   
   "see"  {:synsem {:cat :verb
                    :sem {:pred :see}}
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
                   :sem {:pred :set}}
          :english {:past "set (past)"}}

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
                    :sem {:pred :show
                          :past-participle "shown"}}}

   "sigh" {:synsem {:cat :verb
                :sem {:pred :sigh}}}

   "sing" {:synsem {:cat :verb
                    :sem {:pred :sing}}
           :english {:past "sang"}}

   "sit down" {:english {:past "sat down"
                         :participle "sitting down"
                         :past-participle "sat down"
                         :present {:3sing "sits down"}}
               :synsem {:cat :verb
                        :sem {:pred :sit-down}}}

   "sleep" {:synsem {:cat :verb
                     :sem {:subj {:animate true}
                           :discrete false
                           :pred :sleep}}
            :english {:past "slept"}}

   "snap" {:synsem {:cat :verb
                    :sem {:pred :snap-pictures}}
           :english {:past "snapped"
                     :participle "snapping"
                     :note "pictures"}}
   "some"
   [{:synsem {:cat :det
              :def :partitivo
              :agr {:number :plur}}}
    {:synsem {:cat :det
              :def :indef
              :agr {:number :sing}}}]
   "speak"
   {:english {:past "spoke"
              :past-participle "spoken"}
    :synsem {:cat :verb
             :sem {:pred :speak
                   :subj {:human true}
                   :obj {:speakable true}}}}

  "stay" {:synsem {:cat :verb
                     :sem {:pred :stay}}}

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

   "take" (let [common {:synsem {:cat :verb}
                        :english {:past "took"
                                  :past-participle "taken"}}]
            [(unify common
                    {:synsem {:sem {:pred :grab}}
                     :english {:note "grab"}})
             (unify common
                    {:synsem {:sem {:pred :take}}})])

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

   "tell" {:english {:past "told"}
           :synsem {:cat :verb
                    :sem {:pred :tell}}}
   "the" (map #(unify % 
                      {:synsem {:cat :det
                                :def :def
                                :mass false}})
              [{:synsem {:agr {:number :sing}}}
               {:synsem {:agr {:number :plur}}}])

   "their"
   (map #(unify %
                {:synsem {:cat :det
                          :sem {:pred :loro}
                          :def :possessive}})
        [{:synsem {:agr {:number :sing}}}
         {:synsem {:agr {:number :plur}}}])
    
   "themselves"
   [{:note "♀" 
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :3rd
                    :number :plur
                    :gender :fem}
              :sem {:human true}
              :subcat '()}}
    
    {:note "♂" 
     :synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :3rd
                    :number :plur
                    :gender :masc}
              :sem {:human true}
              :subcat '()}}]
   
   ;; TODO: move gender symbol to :notes for all "they" variants.
   "they (♂)"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :masc
                    :number :plur}
              :sem {:gender :masc
                    :human true
                    :pred :loro}
              :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :masc
                    :number :plur}
              :sem {:gender :masc
                   :human false
                    :pred :loro}
              :subcat '()}}]
   "they (♀)"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :fem
                    :number :plur}
              :sem {:gender :fem
                    :human true
                   :pred :loro}
              :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :fem
                    :number :plur}
              :sem {:gender :fem
                    :human false
                    :pred :loro}
              :subcat '()}}]
            
    "think" {:synsem {:cat :verb
                     :sem {:pred :think }}
            :english {:past "thought"}}        
            
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
                        
   "understand (deeply)" {:synsem {:cat :verb
                                   :sem {:pred :understand-deeply}}
                          :english {:present {:3sing "understands (deeply)"}
                                    :past "understood (deeply)"
                                    :participle "understanding (deeply)"}}                    
                        
   "understand (simply)" {:synsem {:cat :verb
                                   :sem {:pred :understand-simply}}
                          :english {:present {:3sing "understands (simply)"}
                                    :past "understood (simply)"
                                    :participle "understanding (simply)"}}
                           
   "upload"  {:synsem {:cat :verb
                       :sem {:pred :caricare}}}

   "use"  {:synsem {:cat :verb
                    :sem {:pred :usare}}}

   "wait"  {:synsem {:cat :verb
                     :sem {:pred :wait-for}}}
   "wake up"
   (let [subject-semantics (atom {:animate true})]
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
              
   "want" {:synsem {:cat :verb
                    :sem {:pred :want}}}        

   "warm" {:synsem {:cat :verb
                :sem {:pred :warm}}}

   "warn" {:synsem {:cat :verb
                     :sem {:pred :warn}}}
   
   "wash" (let [subject-semantics (atom :top)]
            {:synsem {:cat :verb
                      :sem {:pred :wash
                            :reflexive true
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

   "work" [{:synsem {:cat :verb
                     :sem {:pred :work-human
                           :subj {:human true}}}
            :english {:note "human"}}
           
           {:english {:note "nonliving or machines"} ;; TODO: add support in UI for :note.
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

   "your"
   (map #(unify %
                {:synsem {:cat :det
                          :sem {:pred :tu}
                          :def :possessive}})
        [{:synsem {:agr {:number :sing}}}
         {:synsem {:agr {:number :plur}}}])
   
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

(def lexicon (-> (compile-lex lexicon-source 
                              morph/exception-generator 
                              morph/phonize morph/english-specific-rules)
                 ;; make an intransitive version of every verb which has an
                 ;; [:sem :obj] path.
                 intransitivize

                 ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
                 transitivize
                         
                 ;; if a verb has an object,
                 ;; and the object is not {:reflexive true}
                 ;; then the object is {:reflexive false}
                 (if-then {:synsem {:cat :verb
                                    :subcat {:2 {:reflexive false}}}}
                          {:synsem {:subcat {:2 {:reflexive false}}}})))

