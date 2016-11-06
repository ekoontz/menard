(ns babel.english.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.encyclopedia :as encyc]
   [babel.english.morphology :as morph]
   [babel.lexiconfn :refer [apply-unify-key compile-lex default new-entries verb-pred-defaults]]
   [dag_unify.core :refer [dissoc-paths fail? get-in strip-refs unify]]))

(def lexicon-source
  {
   "Antonia"
   {:synsem {:sem {:pred :antonia
                   :human true}
             :propernoun true
             :agr {:number :sing
                   :person :3rd
                   :gender :fem}}}

   "Antonia and Luisa"
   {:synsem {:sem {:pred :antonia-and-luisa
                   :human true}
             :propernoun true
             :agr {:number :plur
                   :person :3rd
                   :gender :fem}}}
   "Antonio"
   {:synsem {:propernoun true
             :agr {:number :sing
                   :person :3rd
                   :gender :masc}
             :sem {:pred :antonio
                   :human true}}}
   "a"
   {:synsem {:cat :det
             :def :indef
             :mass false
             :sem {:pred :a-generic-instance-of
                   :of {:pred nil}}
             :agr {:number :sing}}}

   "abandon" [{:synsem {:cat :verb
                        :sem {:pred :abandon}
                        :subcat {:1 {:cat :noun}
                                 :2 {:cat :noun}}}}]

   "accept"  {:synsem {:cat :verb
                       :sem {:pred :accept}}}

   "accompany" {:synsem {:cat :verb
                         :sem {:pred :accompany}
                         :subcat {:1 {:cat :noun}
                                  :2 {:cat :noun}
                                  :3 '()}}
                :english {:past "accompanied"}}

   "add" {:synsem {:cat :verb
                   :sem {:pred :add}}}

   "admire" {:synsem {:cat :verb
                      :sem {:pred :admire}}}

   "announce" {:synsem {:cat :verb
                        :sem {:pred :announce}}}

   "answer" {:synsem {:cat :verb
                      :sem {:pred :answer}}}

   "approve" {:synsem {:cat :verb
                :sem {:pred :approve}}}

   "arrive" [{:synsem {:cat :verb
                       :sem {:pred :arrive
                             :subcat {:2 {:cat :prep
                                          :sem {:pred :at}}}}}}
             {:synsem {:cat :verb
                       :sem {:pred :arrive
                             :subcat {:2 '()}}}}]

   "ask" {:synsem {:cat :verb
                   :sem {:pred :ask-for}}}

   "assume" {:synsem {:cat :verb
                      :sem {:pred :assume}
                      :subcat {:1 {:cat :noun}
                               :2 {:cat :comp
                                   :comp-type :that
                                   :subcat '()}}}}
   "assure" {:synsem {:cat :verb
                      :sem {:pred :assure}}}

   "at" (let [obj (atom {:place true})]
          {:synsem {:cat :prep
                    :subcat {:1 {:cat :noun
                                 :sem obj}}
                    :sem {:obj obj
                          :pred :at}}})

   "attend" {:synsem {:cat :verb
                      :sem {:pred :attend}
                      :subcat {:1 {:cat :noun}
                               :2 {:cat :noun}}}}

   "avoid" {:synsem {:cat :verb
                     :sem {:pred :avoid}
                     :subcat {:1 {:cat :noun}
                              :2 {:cat :noun}}}}

   "backpack" {:synsem {:cat :noun
                        :sem {:pred :backpack}}}

   "bag" {:synsem {:cat :noun
                   :sem {:pred :bag}}}

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
                  {:sense 1
                   :synsem {:subcat {:1 {:cat :noun}
                                     :2 '()}
                            :sem {:pred :be}}})

           ;; be + propernoun, e.g. "My name is John"
           (let [gender (atom :top)
                 number (atom :top)
                 subj-agr (atom {:person :3rd
                                 :gender gender
                                 :number number})
                 infl (atom :top)
                 the-real-subj (atom :top)
                 the-obj (atom {:number number
                                :gender gender})] ;; prevents e.g. "Her name is John"
             (unify common
                    {:sense 2 ;; used for debugging
                     ;; TODO: remove (in)transtivize (false|true): should not
                     ;; need these explicit compiler directives; compilation
                     ;; should be able to manage on its own
                     :intransitivize false
                     :transitivize false
                     :synsem {:agr subj-agr
                              :sem {:aspect :progressive
                                    :pred :be-called
                                    :reflexive true
                                    :subj the-real-subj
                                    :obj the-real-subj
                                    :iobj the-obj}
                              :subcat {:1 {:cat :noun
                                           :case :nom ;; TODO: this should be a general lexical post-processing step -
                                           ;; call it subject-is-nominative or similar.
                                           :agr subj-agr
                                           :sem {:pred :name  ;; "My name" in "My name is John"
                                                 :subj the-real-subj}
                                           }
                                       :2 {:cat :noun
                                           :agr subj-agr
                                           :sem the-obj
                                           :pronoun false
                                           :propernoun true ;; "John" in "My name is John"
                                           }
                                       :3 '()
                                       } ;; subcat {
                              } ;; synsem {
                     } ;; end of map
                    ))

           (let [pred (atom :top)
                 obj (atom :top)]
             (unify common
                    {:sense 3
                     :synsem {:cat :verb
                              :sem {:obj obj
                                    :reflexive false
                                    :pred pred
                                    :shared-with-obj true}
                              :subcat {:1 {:cat :noun}
                                       :2 {:cat :prep
                                           :reflexive false
                                           :sem {:obj obj
                                                 :pred pred}}
                                       :3 '()}}}))
           (let [pred (atom :top)
                 obj (atom :top)
                 agr (atom :top)]
             (unify common
                    {:sense 4
                     :synsem {:cat :verb
                              :sem {:subj obj ;; reflexive: subj=obj
                                    :obj obj
                                    :reflexive true
                                    :pred pred
                                    :shared-with-obj true}
                              :subcat {:1 {:cat :noun
                                           :agr agr}
                                       :2 {:cat :prep
                                           :reflexive true
                                           :agr agr
                                           :sem {:obj obj
                                                 :pred pred}}
                                       :3 '()}}}))])

   "be able" (let [common {:english {:imperfect {:1sing "was able"
                                                 :2sing "were able"
                                                 :3sing "was able"
                                                 :1plur "were able"
                                                 :2plur "were able"
                                                 :3plur "were able"}

                                     ;; TODO: improve this. Currently sounds pretty awkward:
                                     ;; "he was being able"
                                     :participle "being able"

                                     :present {:1sing "am able"
                                               :2sing "are able"
                                               :3sing "is able"
                                               :1plur "are able"
                                               :2plur "are able"
                                               :3plur "are able"}
                                     :past {:1sing "was able"
                                            :2sing "were able"
                                            :3sing "was able"
                                            :1plur "were able"
                                            :2plur "were able"
                                            :3plur "were able"}}}]
               [(unify common
                       {:modal-with :infinitive
                        :synsem {:cat :verb
                                 :sem {:pred :be-able-to}}})
                (unify common
                       {:synsem {:cat :verb
                                 :sem {:pred :be-able-to}
                                 :subcat {:1 {:cat :top}
                                          :2 '()}}})])

   "be born" {:synsem {:cat :verb
                       :sem {:pred :be-born}
                       :subcat {:2 '()}}
              :english {:future "be born"
                        :participle "being born"
                        :conditional "be born"
                        :imperfect {:1sing "was being born"
                                    :2sing "were being born"
                                    :3sing "was being born"
                                    :1plur "were being born"
                                    :2plur "were being born"
                                    :3plur "were being born"}
                        :past {:english "was born"
                               :2sing "were born"
                               :2plur "were born"
                               :3plur "were born"}
                        :present {:1sing "am born"
                                  :2sing "are born"
                                  :3sing "is born"
                                  :1plur "are born"
                                  :2plur "are born"
                                  :3plur "are born"}}}

   "be missed" {:synsem {:cat :verb
                         :sem {:pred :mancare}
                         :subcat {:2 '()}}
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

   "beach" {:synsem {:cat :noun
                     :sem {:place true
                           :pred :beach
                           :artifact false}}}

   "become" [{:synsem {:cat :verb
                       :sem {:pred :become}
                       :subcat {:2 {:cat :adjective}}}
              :english {:past "became"}}
             {:synsem {:cat :verb
                       :sem {:pred :become}
                       :subcat {:2 '()}}
              :english {:past "became"}}]

   "begin" [{:synsem {:cat :verb
                      :sem {:pred :begin}
                      :subcat {:2 '()}}
             :english {:past "began"
                       :participle "beginning"}}
            {:modal-with :infinitive
             :synsem {:cat :verb
                      :sem {:pred :begin}}
             :english {:past "began"
                       :participle "beginning"}}]

   "believe" (let [common {:synsem {:cat :verb
                                    :subcat {:1 {:cat :noun
                                                 :sem {:human true}}}}}]
               [(unify common
                       {:synsem {:sem {:pred :believe}
                                 :subcat {:2 '()}}}) ;; intransitive
                (unify common
                       {:synsem {:sem {:pred :believe}
                                 :subcat {:2 {:cat :comp
                                              :comp-type :that
                                              :subcat '()}
                                          :3 '()}}})])
   "bicycle" {:synsem {:cat :noun
                       :sem {:pred :bicycle
                             :artifact true
                             :consumable false
                             :place false}}}

   "bird" {:synsem {:cat :noun
                    :sem {:pred :bird}}}
   "black"
   {:synsem {:cat :adjective
             :sem {:mod {:pred :black}
                   :comparative false
                   :physical-object true
                   :human false}}}

   "boil" {:synsem {:cat :verb
                :sem {:pred :boil}}}
   "book"
   {:synsem {:cat :noun
             :sem {:artifact true
                   :pred :book
                   :legible true
                   :speakable false
                   :mass false
                   :place false
                   :consumable false}}}

   "break" {:english {:past "broke"
                       :past-participle "broken"}
             :synsem {:cat :verb
                      :sem {:pred :break}}}
   "bread"
   ;; inherently singular.
   {:synsem {:agr {:number :sing}
             :cat :noun
             :sem {:pred :bread
                   :edible true
                   :artifact true}
             :subcat {:1 {:cat :det
                          :def :def}}}}

   "bring" {:synsem {:cat :verb
                     :sem {:pred :bring}}
          :english {:past "brought"}}

   "burn" {:synsem {:cat :verb
                    :sem {:pred :burn}}
           :english {:past "burnt"}}

   "buy" {:synsem {:cat :verb
                   :sem {:pred :comprare}}
          :english {:past "bought"
                    :present {:3sing "buys"}}}

   "call" {:synsem {:cat :verb
                    :sem {:pred :call}}}
   "can"
   {:english {:participle "being able to"
              :past "could"
              :imperfect {:1sing "used to be able to"
                          :2sing "used to be able to"
                          :3sing "used to be able to"
                          :1plur "used to be able to"
                          :2plur "used to be able to"
                          :3plur "used to be able to"}
              :present {:3sing "can"}
              :future "be able to"
              :conditional "be able to"}
    :modal-with :root
    :synsem {:cat :verb
             :sem {:pred :be-able-to}}}

   "car" {:synsem {:cat :noun
                   :sem {:pred :car}}}

   "carry" {:synsem {:cat :verb
                     :sem {:pred :carry}}
            :english {:past "carried"}}

   "cat" {:synsem {:cat :noun
                   :sem {:pred :cat}}}

   "chair" {:synsem {:cat :noun
                     :sem {:pred :chair}}}

   "change" {:synsem {:cat :verb
                      :sem {:pred :change}}} ;; TODO: add reflexive sense

   "change clothes" {:synsem {:cat :verb
                              :sem {:pred :change-clothes
                                    :reflexive true}
                              :subcat {:2 '()}}
                     :english {:present {:3sing "changes clothes"}
                               :participle "changing clothes"
                               :past "changed clothes"}}

   "chat" (let [common {:synsem {:cat :verb
                                 :sem {:pred :chat}}
                        :english {:participle "chatting"
                                  :past "chatted"}}]
            [(unify common ;; intransitive TODO: remove: intransitivization should happen automatically in lexical compilation.
                    {:synsem {:subcat {:2 '()}}})
             (unify common ;; transitive "chatted with <human>"
                    {:share-sem :obj
                     :synsem {:subcat {:2 {:cat :prep
                                           :sem {:obj {:human true}
                                                 :pred :with}
                                           :subcat '()}}}})])

   "charge" {:synsem {:cat :verb
                      :sem {:pred :charge}}}

   "check" {:synsem {:cat :verb
                     :sem {:pred :check}}}

   "city" {:synsem {:cat :noun
                    :sem {:pred :city
                          :city true}}}

   "clean" {:synsem {:cat :verb
                     :sem {:pred :clean}}}

   "close" {:synsem {:cat :verb
                     :sem {:pred :close}}}

   "coffee" {:synsem {:cat :noun
                      :sem {:pred :coffee}}}

   "comb" (let [common {:synsem {:cat :verb}}]
            ;; 1. reflexive sense of "comb"
            [(let [subject-semantics (atom :top)
                   subject-agr (atom :top)]
               {:unify [common]
                :synsem {:sem {:pred :comb-oneself
                               :subj subject-semantics
                               :obj subject-semantics}
                         :subcat {:1 {:agr subject-agr
                                      :sem subject-semantics}
                                  :2 {:agr subject-agr
                                      :pronoun true
                                      :reflexive true
                                      :sem subject-semantics}}}})])

   "come" {:synsem {:cat :verb
                    :sem {:pred :come}
                    :subcat {:2 '()}}
           :english {:past "came"}}

   "comment" {:synsem {:cat :verb
                       :sem {:pred :comment}}}

   "confess" {:synsem {:cat :verb
                       :sem {:pred :confess}
                       :subcat {:2 {:cat :prep
                                    :sem {:pred :to}}}}}

   "complain" {:synsem {:cat :verb
                        :sem {:pred :complain}}}

   "consent" {:synsem {:cat :verb
                       :sem {:pred :consent}
                       :subcat {:2 {:cat :prep
                                    :sem {:pred :to}}}}}
   "conserve" {:synsem {:cat :verb
                        :sem {:pred :conserve}}}

   "consider" {:synsem {:cat :verb
                        :sem {:pred :consider}}}

   "continue" {:synsem {:cat :verb
                      :sem {:pred :continue}}}

   "convert" {:synsem {:cat :verb
                       :sem {:pred :convert}}}

   "correspond" {:share-sem :obj
                 :synsem {:cat :verb
                          :sem {:pred :correspond}
                          :subcat {:2 {:cat :prep
                                       :sem {:pred :with}}}}}
   "count" {:synsem {:cat :verb
                      :sem {:pred :count}}}

   "create" {:synsem {:cat :verb
                      :sem {:pred :create}}}

   "cry" {:synsem {:cat :verb
                   :sem {:pred :cry}
                   :subcat {:2 '()}}}

   "cut" {:english {:past "cut"
                    :participle "cutting"}
          :synsem {:cat :verb
                   :sem {:pred :cut}}}

    "damage" {:synsem {:cat :verb
                   :sem {:pred :damage}}}


   "dance" [{:synsem {:cat :verb
                      :sem {:pred :dance}
                      :subcat {:2 {:cat :prep
                                   :sem {:pred :with}}}}}
            {:synsem {:cat :verb
                      :sem {:pred :dance}
                      :subcat {:2 '()}}}]

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
                    :sem {:pred :cenare}
                    :subcat {:2 '()}}}

   "displace" {:synsem {:cat :verb
                :sem {:pred :displace}}}

   "divide" {:synsem {:cat :verb
                      :sem {:pred :divide}}}

   "draw" {:synsem {:cat :verb
                      :sem {:pred :draw}}
                      :english {:past "drew"}}


   "drink" {:synsem {:cat :verb
                     :sem {:pred :drink}}
            :english {:past "drank"}}

   "drive" {:synsem {:cat :verb
                     :sem {:pred :drive}}
            :english {:past "drove"}}

   "disappoint" {:synsem {:cat :verb
                          :sem {:pred :deludere}}}

   "do" {:synsem {:cat :verb
                  :sem {:pred :do}}
         :english {:past "did"
                   :present {:3sing "does"}}}

   "download" {:synsem {:cat :verb
                        :sem {:pred :scaricare}}}

   "dog" {:synsem {:cat :noun
                   :sem {:pred :dog}}}

   "earn"  {:synsem {:cat :verb
                     :sem {:pred :earn}}}

   "eat" {:english {:past "ate"}
          :synsem {:cat :verb
                   :sem {:pred :eat}}}

   "eat dinner" {:synsem {:cat :verb
                          :sem {:pred :have-dinner}
                          :subcat {:2 '()}}
                 :english {:present {:3sing "eats dinner"}
                           :participle "eating dinner"
                           :past "ate dinner"}}

   "embrace" {:synsem {:cat :verb
                       :sem {:pred :abbracciare}}}

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

   "espresso" {:synsem {:cat :noun
                        :sem {:pred :espresso}}}

   "exist" {:synsem {:cat :verb
                     :sem {:pred :exist}
                     :subcat {:2 '()}}}

   "exit" {:synsem {:cat :verb
                     :sem {:pred :exit}}}

   "explain" {:synsem {:cat :verb
                     :sem {:pred :explain}}}


   "express" {:synsem {:cat :verb
                       :sem {:pred :express}}}

   "faint" {:synsem {:cat :verb
                     :sem {:pred :faint}
                     :subcat {:2 '()}}}

   "fall" {:transitivize false
           :synsem {:cat :verb
                    :sem {:pred :fall}}
           :english {:past "fell"}}

    "fall asleep"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :fall-asleep
                     :reflexive true
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "falling asleep"
                :present {:3sing "falls asleep"}
                :past "fell asleep"}})

   "father" {:synsem {:cat :noun
                      :agr {:gender :masc}
                      :sem {:human true
                            :pred :father
                            :child false}}}

   "fill" {:synsem {:cat :verb
                    :sem {:pred :fill}}}

   "find" {:synsem {:cat :verb
                     :sem {:pred :break }}
                     :english {:past "found"}}


   "finish" {:synsem {:cat :verb
                      :sem {:pred :finish}}}

   "first" {:synsem {:cat :adjective
                     :sem {:mod {:pred :first}
                           :comparative false}}}

   "fit" {:english {:past "fit"
                    :participle "fitting"}
          :synsem {:cat :verb
                   :sem {:pred :fit}}}


    "fold" {:synsem {:cat :verb
                    :sem {:pred :fold}}}

   "follow" {:synsem {:cat :verb
                      :sem {:pred :follow}}}

   "for" {:synsem {:cat :prep
                   :subcat {:1 {:cat :noun}}
                   :sem {:pred :for}}}

   "forget" {:synsem {:cat :verb
                      :sem {:pred :forget}}
             :english {:past "forgot"
                       :participle "forgetting"}}

   "form" {:synsem {:cat :verb
                    :sem {:pred :form}}}

   "furnish"  {:synsem {:cat :verb
                        :sem {:pred :furnish}}}

   "game" {:synsem {:cat :noun
                    :sem {:pred :game}}}

   "get angry"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :get-angry
                     :reflexive true
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting angry"
                :present {:3sing "gets angry"}
                :past "got angry"}})
   "get bored"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :get-bored
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting bored"
                :present {:3sing "gets bored"}
                :past "got bored"}})

   "get dressed"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :get-dressed
                     :reflexive true
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting dressed"
                :present {:3sing "gets dressed"}
                :past "got dressed"}})


    "get married"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :get-married
                     :reflexive true
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting married"
                :present {:3sing "gets married"}
                :past "got married"}})

    "get off"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :get-off
                     :subj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting off"
                :present {:3sing "gets off"}
                :past "got off"}})
   "get on"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :get-on
                     :subj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting on"
                :present {:3sing "gets on"}
                :past "got on"}})
   "get ready"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :get-ready
                     :reflexive true
                     :subj subject-semantics
                     :obj subject-semantics}
               :subcat {:1 {:sem subject-semantics}
                        :2 '()}}
      :english {:participle "getting ready"
                :present {:3sing "gets ready"}
                :past "got ready"}})
   "get up"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :get-up
                     :reflexive true
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

   "Gianluca and Giovanni"
   {:synsem {:agr {:number :plur
                   :person :3rd
                   :gender :masc}
             :sem {:pred :gianluca-e-giovanni
                   :human true}
             :propernoun true}}

   "Gianluca and Luisa"
   {:synsem {:agr {:number :plur
                   :person :3rd
                   :gender :masc}
             :sem {:pred :gianluca-e-luisa
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
                    :pred :go}
             :subcat {:2 {:cat :prep}}}
    :english {:past "went"}}

   "go around"
   {:synsem {:cat :verb
             :sem {:activity true
                   :discrete false
                   :pred :go-around}
             :subcat {:2 '()}}
    :english {:past "went around"
              :participle "going around"
              :present {:3sing "goes around"}}}

   "go downstairs"
   {:synsem {:cat :verb
             :sem {:activity true
                   :discrete false
                   :pred :go-downstairs}
             :subcat {:2 '()}}
    :english {:past "went downstairs"
              :participle "going downstairs"
              :present {:3sing "goes downstairs"}}}

   "go out"
   {:synsem {:cat :verb
             :sem {:activity true
                   :discrete false
                   :pred :go-out}
             :subcat {:2 '()}}
    :english {:past "went out"
              :participle "going out"
              :present {:3sing "goes out"}}}

   "go upstairs"
   {:synsem {:cat :verb
             :sem {:activity true
                   :discrete false
                   :pred :go-upstairs}
             :subcat {:2 '()}}
    :english {:past "went upstairs"
              :participle "going upstairs"
              :present {:3sing "goes upstairs"}}}

   "grab"  {:synsem {:cat :verb
                     :sem {:pred :prendere}}
            :english {:participle "grabbing"
                      :past "grabbed"}}

   "guess" {:synsem {:cat :verb
                     :sem {:pred :guess}}}

   ;; TODO: add auxiliary sense of "have"
   "have" [;; possessive
           {:synsem {:cat :verb
                     :subcat {:2 {:cat :noun}}
                     :sem {:activity false
                           :discrete false
                           :pred :have}}
            :english {:present {:3sing "has"}
                      :past "had"}}

           ;; modal: "have to + VP"
           ;; TODO: if used without infinitive VP, conjugates as "have" but should be "have to".
           {:synsem {:cat :verb
                     :sem {:pred :have-to}}
            :english {:present {:3sing "has"}
                      :past "had"}
            :modal-with :infinitive}]

   "have dinner" {:synsem {:cat :verb
                           :sem {:pred :have-dinner}
                           :subcat {:2 '()}}
                  :english {:present {:3sing "has dinner"}
                            :past "had dinner"
                            :participle "having dinner"}}
   "have fun"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :have-fun
                     :reflexive true}
               :subcat {:1 :top
                        :2 '()}}
      :english {:participle "having fun"
                :present {:3sing "has fun"}
                :past "had fun"}})

   "have lunch" {:synsem {:cat :verb
                          :sem {:pred :have-lunch}
                          :subcat {:2 '()}}
                 :english {:present {:3sing "has lunch"}
                           :past "had lunch"
                           :participle "having lunch"}}

   "he" {:synsem {:cat :noun
                  :pronoun true
                  :case :nom
                  :agr {:person :3rd
                        :gender :masc
                        :number :sing}
                  :sem {:human true
                        :pred :lui}
                  :subcat '()}}

   "hear" {:synsem {:cat :verb
                    :sem {:pred :hear}}
           :english {:past "heard"}}

   "help"
   {:synsem {:cat :verb
             :sem {:pred :aiutare
                   :activity true
                   :obj {:human true}}
             :subcat {:1 {:cat :noun}
                      :2 {:cat :noun
                          :sem {:human true}}}}}
   "her"
   [{:synsem {:cat :det
              :agr {:number :sing}
              :sem {:of {:pred :lei}}
              :def :possessive}}
    {:synsem {:cat :det
              :agr {:number :plur}
              :sem {:of {:pred :lei}}
              :def :possessive}}
    {:synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive false
              :agr {:person :3rd
                    :gender :fem
                    :number :sing}
              :sem {:pred :lei
                    :human true}
              :subcat '()}}]

   "herself" {:synsem {:cat :noun
                       :pronoun true
                       :case :acc
                       :reflexive true
                       :agr {:person :3rd
                             :gender :fem
                             :number :sing}
                       :sem {:human true}
                       :subcat '()}}
   "him" {:synsem {:cat :noun
                   :pronoun true
                   :case :acc
                   :reflexive false
                   :agr {:person :3rd
                         :gender :masc
                         :number :sing}
                   :sem {:human true
                         :pred :lui}
                   :subcat '()}}

   "himself" {:synsem {:cat :noun
                       :pronoun true
                       :case :acc
                       :reflexive true
                       :agr {:person :3rd
                             :gender :masc
                             :number :sing}
                       :sem {:human true}
                       :subcat '()}}

   "his" [{:synsem {:cat :det
                    :agr {:number :sing}
                    :sem {:of {:pred :lui}}
                    :def :possessive}}
          {:synsem {:cat :det
                    :agr {:number :plur}
                    :sem {:of {:pred :lui}}
                    :def :possessive}}]

   "hit" {:english {:past {:english "hit"
                          :note "past tense"}
                   :participle "hitting"}
          :synsem {:cat :verb
                   :sem {:pred :hit}}}

   "hold"
   {:synsem {:cat :verb
             :sem {:pred :hold}}
    :english {:past "held"}}

   "hope" (let [common {:synsem {:cat :verb
                                 :subcat {:1 {:cat :noun
                                              :sem {:human true}}}}}]
            [(unify common
                    {:synsem {:sem {:pred :hope}
                              :subcat {:2 '()}}}) ;; intransitive
             (unify common
                    {:synsem {:sem {:pred :hope}
                              :subcat {:2 {:cat :comp
                                           :comp-type :that
                                           :subcat '()}
                                       :3 '()}}})])
   "house" {:synsem {:cat :noun
                     :sem {:pred :house}}}
   "hug"
   {:synsem {:cat :verb
             :sem {:pred :hug}}
    :english {:past "hugged"
              :participle "hugging"}}

   "hurt" (let [common {:english {:past "hurt"}
                        :synsem {:cat :verb}}]
            ;; 1. reflexive sense of "hurt"
            [(let [subject-semantics (atom :top)
                   subject-agr (atom :top)]
               (unify common
                       {:synsem {:sem {:pred :hurt-oneself
                                       :subj subject-semantics
                                       :obj subject-semantics}
                                 :subcat {:1 {:agr subject-agr
                                              :sem subject-semantics}
                                         :2 {:agr subject-agr
                                             :pronoun true
                                             :reflexive true
                                             :sem subject-semantics}}}}))

             ;; 2. transitive sense of "hurt"
             (unify common
                     {:synsem {:sem {:pred :hurt
                                     ;; TODO: consider making lexicon post-processing rule:
                                     ;; if not(reflexive=true) => reflexive=false
                                     :reflexive false
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

   "Juan and me"
   [{:synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive false
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

   "if"   {:synsem {:cat :comp
                    :comp-type :if
                    :subcat {:1 {:cat :verb
                                 :subcat '()}
                             :2 '()}}}

   "imagine" {:synsem {:cat :verb
                       :sem {:pred :imagine}}}

   "import" {:synsem {:cat :verb
                      :sem {:pred :import}}}

   "improve" {:synsem {:cat :verb
                       :sem {:pred :improve}}}

   "in front of" {:synsem {:cat :prep
                           :sem {:pred :in-front-of
                                 :obj {:furniture true}}}}

   "increase" {:synsem {:cat :verb
                        :sem {:pred :increase}}}

   "insist" {:synsem {:cat :verb
                      :sem {:pred :insist}}}

   "insure" {:synsem {:cat :verb
                      :sem {:pred :insure}}}

   "intelligent" {:synsem {:cat :adjective
                           :sem {:mod {:pred :intelligent}
                                 :human true
                                 :comparative false}
                           :subcat {:1 {:cat :det}
                                    :2 '()}}}
   "interrupt" {:synsem {:cat :verb
                :sem {:pred :interrupt}}}
   "it"
   [{:english {:note "♂"}
     :synsem {:cat :noun
              :pronoun true
              :case :top ;; we could just omit this kv, but we explicitly
              ;; set it to :top to show how it's different than other
              ;; pronouns: like 'you' the nominative and accusative
              ;; are the same (compare versus 'I'/'me', 'she'/'her', etc)
              :agr {:person :3rd
                    :number :sing}
              :reflexive false
              :sem {:pred :lui
                    :gender :masc
                    :human false}
              :subcat '()}}

    {:english {:note "♀"}
     :synsem {:cat :noun
              :pronoun true
              :case :top ;; see above comment
              :agr {:person :3rd
                    :number :sing}
              :reflexive false
              :sem {:pred :lei
                    :gender :fem
                    :human false}
              :subcat '()}}]
   "itself"
   [{:synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :3rd
                    :gender :masc
                    :number :sing}
              :sem {:human false}
              :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :3rd
                    :gender :fem
                    :number :sing}
              :sem {:human false}
              :subcat '()}}]
   "keep"
   [{:synsem {:cat :verb
              :sem {:pred :keep}}
     :english {:past "kept"}}
    {:synsem {:cat :verb
              :sem {:pred :keep-safe}}
     :english {:note "something safe"
               :past "kept"}}]

   "key" {:synsem {:cat :noun
                   :sem {:pred :key
                         :artifact true
                         :legible false
                         :speakable false}}}

   "kill" {:synsem {:cat :verb
                    :sem {:pred :kill}}}

   "learn" {:synsem {:cat :verb
                     :sem {:pred :learn}}}

  "know" [{:english {:past "knew"
                    :past-participle "known"}
            :synsem {:cat :verb
                     :sem {:pred :know-s}}}
           {:english {:past "knew"
                     :past-participle "known"}
                     :synsem {:cat :verb
                     :sem {:pred :know-c}}}]


   "leave" [{:english {:past "left"}
             :synsem {:cat :verb
                      :sem {:pred :leave-behind}}}

            {:english {:note "on a trip"
                       :past "left"}
             :synsem {:cat :verb
                      :sem {:pred :leave}}}]

  "lend" {:synsem {:cat :verb
                   :sem {:pred :lend}}
          :english {:past "lent"}}

   "lie" {:synsem {:cat :verb
                   :sem {:pred :lie}
                   :subcat {:2 {:cat :prep
                                :sem {:pred :to}}}}}

   "lift" {:synsem {:cat :verb
                    :sem {:pred :lift}}}

   "light" {:synsem {:cat :verb
                     :sem {:pred :light}}}

   "listen to" {:synsem {:cat :verb
                         :sem {:pred :listen-to}}
                :english {:participle "listening to"
                          :past "listened to"
                          :present {:3sing "listens to"}}}

   "live" [{:synsem {:cat :verb
                     :sem {:pred :live-somewhere}}}
           {:synsem {:cat :verb
                     :sem {:pred :live}}}]

    "look at" {:synsem {:cat :verb
                         :sem {:pred :look-at}}
                :english {:participle "looking at"
                          :past "looked at"
                          :present {:3sing "looks at"}}}

   ;; TODO: combine all "look X" into "look" with different subcategorizations
   "look for" {:synsem {:cat :verb
                        :sem {:pred :look-for}}
               :english {:participle "looking for"
                         :past "looked for"
                         :present {:3sing "looks for"}}}

   "look up" {:synsem {:cat :verb
                       :sem {:pred :look-up}}
              :english {:participle "looking up"
                        :past "looked up"
                        :present {:3sing "looks up"}}}

   "lose" {:english {:participle "losing"
                     :past "lost"}
           :synsem {:cat :verb
                    :sem {:pred :lose}}}

   "love" {:synsem {:cat :verb
                    :sem {:pred :amare}}}

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

  "Luisa and I (♂)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:gender :masc
                   :person :1st
                   :number :plur}
              :sem {:human true
                    :pred :luisa-and-i}
             :subcat '()}}

   "Luisa and I (♀)"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:gender :fem
                   :person :1st
                   :number :plur}
             :sem {:human true
                    :pred :luisa-and-i}
             :subcat '()}}

   "make" {:synsem {:cat :verb
                    :sem {:pred :make}}
           :english {:past "made"}}

   "man" {:english {:plur "men"}
          :synsem {:agr {:gender :masc}
                   :cat :noun
                   :sem {:human true
                         :pred :man
                         :child false}}}

   "manage" {:synsem {:cat :verb
                :sem {:pred :manage}}}
   "Matteo"
   {:synsem {:agr {:number :sing
                   :person :3rd
                   :gender :masc}
             :sem {:pred :matteo
                   :human true}
             :propernoun true}}

   "may" {:english {:past "might"
                    :participle "able to"
                    :present {:1sing "may"
                              :2sing "may"
                              :3sing "may"
                              :1plur "may"
                              :2plur "may"
                              :3plur "may"}
                    :future "be able to"
                    :conditional "be able to"}
          :modal-with :root
          :synsem {:cat :verb
                   :sem {:pred :may}}}

   "me" {:synsem {:cat :noun
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

   "meeting" {:synsem {:cat :noun
                       :sem {:pred :meeting
                             :event true}}}

   "mother" {:synsem {:agr {:gender :fem}
                      :cat :noun
                      :sem {:human true
                            :pred :madre
                            :child false}}}

   "move" {:synsem {:cat :verb
                    :sem {:pred :move}}}

   "multiply" {:synsem {:cat :verb
                        :sem {:pred :multiply}}}

   ;; TODO: should not need to provide an irregular plural form
   ;; [:sem :mass]=true should be sufficient.
   "music" {:synsem {:agr {:number :sing}
                     :cat :noun
                     :sem {:pred :music
                           :animate false
                           :physical-object false
                           :place false
                           :mass true}}}
   "my"
   (map #(unify %
                {:synsem {:cat :det
                          :sem {:of {:pred :I}}
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
   (let [gender (atom :top)
         number (atom :top)
         agr (atom {:gender gender
                    :number number})
         of (atom {:gender gender
                   :number number})]
     {:synsem {:agr agr
               :cat :noun
               :sem {:pred :name
                     :mod '()
                     :subj of}
               :subcat {:1 {:agr agr
                            :cat :det
                            :def :possessive
                            :sem {:of of}}}}})

   "need" {:synsem {:cat :verb
                :sem {:pred :need}}}

   "new" {:synsem {:cat :adjective
                   :sem {:mod {:pred :new}
                         :physical-object true}}}

   "note" [{:synsem {:cat :verb
                     :sem {:pred :note}}}
           {:synsem {:cat :noun
                     :sem {:pred :note}}}]

   "observe" {:synsem {:cat :verb
                :sem {:pred :observe}}}

   "obtain" {:synsem {:cat :verb
                :sem {:pred :obtain}}}

   "off" {:synsem {:cat :prep
                   :sem {:pred :off}
                   :subcat '()}}

   "up" {:synsem {:cat :prep
                   :sem {:pred :up}
                   :subcat '()}}
   "down" {:synsem {:cat :prep
                   :sem {:pred :down}
                   :subcat '()}}

   "old" {:synsem {:cat :adjective
                   :sem {:mod {:pred :old}
                         :physical-object true}}}

   "on" {:synsem {:cat :prep
                  :sem {:pred :on}
                  :subcat '()}}

   "organize" {:synsem {:cat :verb
                :sem {:pred :organize}}}
   "our"
   (map #(unify %
                {:synsem {:cat :det
                          :agr {:gender :masc}
                          :sem {:of {:pred :noi}}
                          :def :possessive}})
        [{:synsem {:agr {:number :sing}}}
         {:synsem {:agr {:number :plur}}}])

   "ourselves"
   [{:synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :1st
                    :number :plur
                    :gender :fem}
              :sem {:human true}
              ;; TODO: remove: should be derived from
              ;; :pronoun=true.
              :subcat '()}}

    {:synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :1st
                    :number :plur
                    :gender :masc}
              :sem {:human true}
              ;; TODO: remove: should be derived from
              ;; :pronoun=true.
              :subcat '()}}]

   "paint"  {:synsem {:cat :verb
                      :sem {:pred :paint}}}

   "participate"  {:synsem {:cat :verb
                            :sem {:pred :participate}
                            :subcat {:2 '()}}}

   "party" [{:synsem {:cat :noun
                      :sem {:pred :party
                            :place true}}}
            {:synsem {:cat :noun
                      :sem {:pred :party
                            :event true}}}]

   "pay" {:synsem {:cat :verb
                    :sem {:pred :pay}}
                          :english {:past "paid"}}


   "pizza"
   {:synsem {:cat :noun
             :sem {:pred :pizza}}}

   ;; TODO: 3sing present exception used below to avoid "playies" is not an exception: it's a rule: y->ys.
   ;; the exceptional case is when "ys" is not used (e.g. "tries").
   "play" [{:comment "We are talking about playing games or sports."
            :english {:note "⚽"}
            :synsem {:cat :verb
                     :sem {:pred :giocare}}}

           {:comment "We are talking about playing music or sounds."
            :english {:note "🎼"}
            :synsem {:cat :verb
                     :sem {:pred :suonare}}}]

   "praise" {:synsem {:cat :verb
                      :sem {:pred :praise}}}

   ;; TODO: all reflexive verbs should have this subject-object agreement like this one does:
   "prepare" (let [subject-semantics (atom :top)
                   subject-agreement (atom :top)]
               {:synsem {:cat :verb
                         :sem {:pred :get-ready
                               :subj subject-semantics
                               :obj subject-semantics}
                         :subcat {:1 {:sem subject-semantics
                                      :agr subject-agreement
                                      }
                                  :2 {:pronoun true
                                      :reflexive true
                                      :agr subject-agreement
                                      :sem subject-semantics}}}})
   "preserve" {:synsem {:cat :verb
                        :sem {:pred :preserve}}}

   "print"  {:synsem {:cat :verb
                      :sem {:pred :stampare}}}

   "professor" {:synsem {:agr :top
                         :cat :noun
                         :sem {:human true
                               :pred :professor}}}

   "pupil" {:synsem {:agr :top
                     :cat :noun
                     :sem {:human true
                           ;; example of a synonym, where we use the convention of making the
                           ;; :pred (i.e. :student) the more common case (c.f. "student")
                           :pred :student}}}
   "put"
   (let [common {:english {:participle "putting"
                           :past "put"
                           :present {:3sing "put"}
                           :note "past tense"}}]
     [{:unify [common]
       :phrasal-verb true
       :synsem {:cat :verb
                :sem {:pred :insult}
                :subcat {:2 {:cat :prep
                             :sem {:pred :down}}}}}
      {:unify [common]
       :phrasal-verb true
       :synsem {:cat :verb
                :sem {:pred :delay}
                :subcat {:2 {:cat :prep
                             :sem {:pred :off}}}}}
      {:unify [common]
       :phrasal-verb true
       :synsem {:cat :verb
                :sem {:pred :put-on}
                :subcat {:2 {:cat :prep
                             :sem {:pred :on}}}}}
      {:unify [common]
       :phrasal-verb true
       :synsem {:cat :verb
                :sem {:pred :tolerate}
                :subcat {:2 {:cat :prep
                             :sem {:pred :up-with}}}}}])
   "put on"
   {:english {:participle "putting on"
              :past "put on"
              :present {:3sing "puts on"}
              :note "past tense"}
    :phrasal-verb true
    :intransitivize false
    :synsem {:cat :verb
             :sem {:pred :put-on}}}

   "radio" {:synsem {:cat :noun
                     :sem {:pred :radio}}}

   "read" ;; if this was a phonetic dictionary, there would be two
   ;; entries for each pronounciation - one pronounced as
   ;; like the word "reed" and one pronounced like the word "red".
   {:english {:past {:english "read"
                     :note "past tense"}}
    :synsem {:cat :verb
             :sem {:pred :read
                   :discrete false}}}

   "receive"  {:synsem {:cat :verb
                        :sem {:pred :receive}}}

   "reciprocate" {:synsem {:cat :verb
                           :sem {:pred :reciprocate}}}

   "recognize" {:synsem {:cat :verb
                :sem {:pred :recognize}}}

   "recount" {:synsem {:cat :verb
                :sem {:pred :recount}}}

   "recover" {:synsem {:cat :verb
                :sem {:pred :recover}}}
   "red"
   {:synsem {:cat :adjective
             :sem {:mod {:pred :rosso}
                   :comparative false
                   :physical-object true
                   :human false}}}

   "remain" [{:synsem {:cat :verb
                       :subcat {:2 {:cat :prep
                                    :sem {:pred :with}}}
                       :sem {:pred :remain}}} ;; for other than italian

             ;; these two below are for Italian.
             {:english {:note {:it "ri"}}
              :synsem {:cat :verb
                       :sem {:pred :remain1}
                       :subcat {:2 {:cat :prep
                                    :sem {:pred :with}}}}}
             {:english {:note {:it "re"}}
              :synsem {:cat :verb
                       :sem {:pred :remain2}
                       :subcat {:2 {:cat :prep
                                    :sem {:pred :with}}}}}]

   "remember"  {:synsem {:cat :verb
                         :sem {:pred :ricordare}}}

    "repeat"  {:synsem {:cat :verb
                        :sem {:pred :repeat}}}

    "repair" {:synsem {:cat :verb
                        :sem {:pred :repair}}}

    "reserve" {:synsem {:cat :verb
                :sem {:pred :reserve}}}

   "respond"  {:share-sem :obj
               :synsem {:cat :verb
                        :sem {:pred :answer}
                        :subcat {:2 {:cat :prep
                                     :sem {:pred :to}}}}}
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
                   :sem {:pred :run}
                   :subcat {:2 '()}}}

   "s" [(let [of (atom :top)]
          {:synsem {:agr {:number :sing}
                    :cat :det
                    :def :genitive
                    :subcat {:1 {:cat :noun
                                 :pronoun false
                                 :sem of
                                 :subcat '()}}
                    :sem {:pred :of
                          :of of}}})
        (let [of (atom :top)]
          {:synsem {:agr {:number :plur}
                    :cat :det
                    :def :genitive
                    :sem {:pred :of
                          :of of}
                    :subcat {:1 {:cat :noun
                                 :pronoun false
                                 :sem of
                                 :subcat '()}}}})]

   "salad" {:synsem {:cat :noun
                     :sem {:pred :salad}}}

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

   "second" {:synsem {:cat :adjective
                      :sem {:mod {:pred :second}
                            :comparative false}}}

   "see"  [{:synsem {:cat :verb
                     :sem {:pred :see
                           :reflexive false}
                     :subcat {:1 {:cat :noun
                                  :sem {:animate true}}
                              :2 {:cat :noun
                                  :reflexive false
                                  :sem {:physical-object true}}}}
            :english {:past "saw"
                      :past-participle "seen"}}
           (let [obj (atom {:animate true
                            :physical-object true})
                 agr (atom :top)]
             {:synsem {:cat :verb
                       :sem {:pred :see
                             :reflexive true}
                       :subcat {:1 {:cat :noun
                                    :agr agr
                                    :sem obj}
                                :2 {:cat :noun
                                    :pronoun true
                                    :agr agr
                                    :sem obj
                                    :reflexive true}}}
              :english {:past "saw"
                        :past-participle "seen"}})]

   "sell" {:synsem {:cat :verb
                    :sem {:pred :vendere}}
           :english {:past "sold"}}

   "send" {:synsem {:cat :verb
                    :sem {:pred :send}}
           :english {:past "sent"}}

   "set" {:synsem {:cat :verb
                   :sem {:pred :set}}
          :english {:past {:english "set"
                           :participle "setting"
                           :note "past tense"}}}

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

   "shoe" {:synsem {:cat :noun
                    :sem {:pred :shoe}}}
   "short"
   [{:synsem {:cat :adjective
              :sem {:mod {:pred :short}
                    :comparative false
                    :physical-object true}
              :subcat {:1 {:cat :det}
                       :2 '()}}}
    {:synsem {:cat :adjective
              :sem {:mod {:pred :short}
                    :comparative false
                    :event true}
              :subcat {:1 {:cat :det}
                       :2 '()}}}]

   "show" {:synsem {:cat :verb
                    :sem {:pred :show
                          :past-participle "shown"}}}

   "sigh" [{:synsem {:cat :verb
                     :sem {:pred :sigh}
                     :subcat {:2 {:sem {:pred :sigh}}}}}
           {:synsem {:cat :noun
                     :sem {:pred :sigh}}}]

   "sign" {:synsem {:cat :verb
                    :sem {:pred :sign}}}


   "sing" {:synsem {:cat :verb
                    :sem {:pred :sing}}
           :english {:past "sang"}}

   "sit down" {:english {:past "sat down"
                         :participle "sitting down"
                         :past-participle "sat down"
                         :present {:3sing "sits down"}}
               :synsem {:cat :verb
                        :sem {:pred :sit-down
                              :reflexive true}
                        :subcat {:2 '()}}}

   "sleep" {:synsem {:cat :verb
                     :sem {:pred :sleep}
                     :subcat {:2 '()}}
            :english {:past "slept"}}
   "small"
   {:synsem {:cat :adjective
             :sem {:mod {:pred :small}
                   :comparative false}
             :subcat {:1 {:cat :det}
                      :2 '()}}}

   "snap" {:synsem {:cat :verb
                    :sem {:pred :snap-pictures}
                    :subcat {:2 {:sem {:pred :picture}}}}
           :english {:past "snapped"
                     :participle "snapping"
                     :note "pictures"}}
   "some"
   [{:synsem {:cat :det
              :def :partitivo
              :sem {:pred :some-of
                    :of {:pred nil}}
              :agr {:number :plur}}}
    {:synsem {:cat :det
              :def :indef
              :sem {:pred :some-one-of
                    :of {:pred nil}}
              :agr {:number :sing}}}]

   "smoke" {:synsem {:cat :verb
             :sem {:pred :smoke}}}

   "speak"
   {:english {:past "spoke"
              :past-participle "spoken"}
    :synsem {:cat :verb
             :sem {:pred :speak}}}

    "spell" {:synsem {:cat :verb
             :sem {:pred :spell}}}


    "spend" {:synsem {:cat :verb
                     :sem {:pred :spend}}
            :english {:past "spent"}}


    "start" {:synsem {:cat :verb
                     :sem {:pred :start}}}

   "stay" {:synsem {:cat :verb
                    :sem {:pred :stay}
                    :subcat {:2 {:cat :prep
                                 :sem {:pred :at}}}}}

   "stain" {:synsem {:cat :verb
                     :sem {:pred :stain}}}

   "steal" {:synsem {:cat :verb
                     :sem {:pred :steal}}
            :english {:past "stole"}}

   "stop" (let [common {:english {:participle "stopping"
                                  :past "stopped"}
                        :synsem {:cat :verb}}]
            ;; 1. reflexive sense of "stop"
            [(let [subject-semantics (atom :top)
                   subject-agr (atom :top)]
               {:unify [common]
                :synsem {:sem {:pred :stop-oneself
                               :subj subject-semantics
                               :obj subject-semantics}
                         :subcat {:1 {:agr subject-agr
                                      :sem subject-semantics}
                                  :2 {:agr subject-agr
                                      :pronoun true
                                      :reflexive true
                                      :sem subject-semantics}}}})])

   "strike" {:english {:past "struck"}
             :synsem {:cat :verb
                      :sem {:pred :strike}}}

   "student" {:synsem {:agr :top
                       :cat :noun
                       :sem {:human true
                             :pred :student}}}

   "study"  {:synsem {:cat :verb
                      :sem {:pred :study}}
             :english {:past "studied"}}

   "stupid" {:synsem {:cat :adjective
                      :sem {:mod {:pred :stupid}
                            :human true
                            :comparative false}
                      :subcat {:1 {:cat :det}
                               :2 '()}}}
   "supply" {:synsem {:cat :verb
                :sem {:pred :supply}}}

   "support" {:synsem {:cat :verb
                       :sem {:pred :support}}}

   "swim" {:synsem {:cat :verb
                        :sem {:pred :break }}
           :english {:past "swam"
                     :participle "swimming"}}

   "table" {:synsem {:cat :noun
                     :sem {:pred :table}}}

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
             :sem {:pred :talk}
             :subcat {:2 {:cat :prep
                          :sem {:pred :to}}}}}
   "tall"
   {:synsem {:cat :adjective
             :sem {:mod {:pred :tall}
                   :human true
                   :comparative false}
             :subcat {:1 {:cat :det}
                      :2 '()}}}

   "teach"  {:synsem {:cat :verb
                      :sem {:pred :teach}}
                            :english {:past "taught"}}

   "telephone" {:synsem {:cat :verb
                         :sem {:pred :telefonare}}}

   "tell" {:english {:past "told"}
           :synsem {:cat :verb
                    :sem {:pred :tell}}}

   "that" [;; "that" as in "she thinks that .."
           {:synsem {:cat :comp
                     :comp-type :that
                     :subcat {:1 {:cat :verb
                                  :subcat '()}
                              :2 '()}}}
           ;; "that" as in "that woman"
           {:synsem {:cat :det
                     :agr {:number :sing}
                     :sem {:pred :demonstrative
                           :of :none}
                     :def :def}}]

   "the" (map #(unify %
                      {:synsem {:cat :det
                                :sem {:pred :definite
                                      :of {:pred nil}}
                                :def :def}})
              [{:synsem {:agr {:number :sing}}}
               {:synsem {:agr {:number :plur}}}])

   "their"
   (map #(unify %
                {:synsem {:cat :det
                          :sem {:of {:pred :loro}}
                          :def :possessive}})
        [{:synsem {:agr {:number :sing}}}
         {:synsem {:agr {:number :plur}}}])

   "themselves"
   [{:synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :3rd
                    :number :plur
                    :gender :fem}
              :subcat '()}}

    {:synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :3rd
                    :number :plur
                    :gender :masc}
              :subcat '()}}
    ]

   "they"
   [;; human
    {:english {:note "♂"}
     :synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :masc
                    :number :plur}
              :sem {:gender :masc
                    :human true
                    :pred :loro}
              :subcat '()}}
    ;; non-human
    {:english {:note "♂"}
     :synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :masc
                    :number :plur}
              :sem {:gender :masc
                    :human false
                    :pred :loro}
              :subcat '()}}
    ;; human
    {:english {:note "♀"}
     :synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :fem
                    :number :plur}
              :sem {:gender :fem
                    :human true
                   :pred :loro}
              :subcat '()}}
    ;; non-human
    {:english {:note "♀"}
     :synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :fem
                    :number :plur}
              :sem {:gender :fem
                    :human false
                    :pred :loro}
              :subcat '()}}]

   "think" (let [common {:synsem {:cat :verb
                                  :subcat {:1 {:cat :noun
                                               :sem {:human true}}}}
                         :english {:past "thought"}}]
             [(unify common
                     {:synsem {:sem {:pred :think}
                               :subcat {:2 '()}}}) ;; intransitive

              (unify common
                     {:synsem {:sem {:pred :think}
                               :subcat {:2 {:cat :comp
                                            :comp-type :that
                                            :subcat '()}
                                        :3 '()}}})])
   "those" {:synsem {:cat :det
                     :agr {:number :plur}
                     :sem {:pred :demonstrative
                           :of :none}
                     :def :def}}

   "throw" {:english {:past "threw"}
            :synsem {:cat :verb
                     :sem {:pred :throw}}}

   "throw out"
   {:synsem {:cat :verb
             :sem {:pred :throw-out}}
    :english {:past "threw out"
              :present {:3sing "throws out"}
              :participle "throwing out"}}

   "to" {:synsem {:cat :prep
                  :subcat {:1 {:cat :noun}}
                  :sem {:pred :to}}}

   "transfer" {:english {:past "transferred"
                         :participle "transferring"}
               :synsem {:cat :verb
                        :sem {:pred :transfer}}}

   "try" [{:modal-with :infinitive
           :synsem {:cat :verb
                    :sem {:pred :try}}}
          {:synsem {:cat :verb
                    :sem {:pred :try}
                    :subcat {:2 '()}}}]

   "turn"
   [{:phrasal-verb true
     :synsem {:cat :verb
              :sem {:pred :turn-down}
              :subcat {:2 {:cat :prep
                           :sem {:pred :down}}}}}
    {:phrasal-verb true
     :synsem {:cat :verb
              :sem {:pred :turn-off}
              :subcat {:2 {:cat :prep
                           :sem {:pred :off}}}}}
    {:phrasal-verb true
     :synsem {:cat :verb
              :sem {:pred :turn-on}
              :subcat {:2 {:cat :prep
                           :sem {:pred :on}}}}}
    {:phrasal-verb true
     :synsem {:cat :verb
              :sem {:pred :turn-up}
              :subcat {:2 {:cat :prep
                           :sem {:pred :up}}}}}]
   "turn off"
   {:synsem {:cat :verb
             :sem {:pred :turn-off
                   :reflexive false}
             :subcat {:2 {:cat :noun}
                      :3 '()}}
    :english {:past "turned off"
              :present {:3sing "turns off"}
              :participle "turning off"}}
   "turn on"
   {:synsem {:cat :verb
             :sem {:pred :turn-on
                   :reflexive false}
             :subcat {:2 {:cat :noun
                          :pronoun false}
                      :3 '()}}
    :english {:past "turned on"
               :present {:3sing "turns on"}
              :participle "turning on"}}

    "understand" (let [common {:english {:past "understood"}}]
                  [(unify
                   common
                   {:synsem {:cat :verb
                             :sem {:pred :understand}}})
                  (unify
                   common
                   {:synsem {:cat :verb
                             :sem {:pred :understand-deeply}}
                    :english {:note "deeply"}})

                  (unify
                   common
                   {:synsem {:cat :verb
                             :sem {:pred :understand-simply}}
                    :english {:note "simply"}})])

   "upload"  {:synsem {:cat :verb
                       :sem {:pred :caricare}}}

   "use"  {:synsem {:cat :verb
                    :sem {:pred :usare}}}

   "wait"  {:synsem {:cat :verb
                     :sem {:pred :wait-for}
                     :subcat {:2 {:sem {:pred :for}}}}}

   "wake up"
   (let [subject-semantics (atom :top)]
     {:synsem {:cat :verb
               :sem {:pred :wake-up
                     :reflexive true}
               :subcat {:2 '()}}
      :english {:participle "waking up"
                :present {:3sing "wakes up"}
                :past "woke up"}})

   "walk" {:synsem {:cat :verb
                :sem {:pred :walk}}}

   "want" [{:modal-with :infinitive
            :synsem {:cat :verb
                     :sem {:pred :want}}}

           {:synsem {:cat :verb
                     :sem {:pred :want}}}]

   "warm" {:synsem {:cat :verb
                :sem {:pred :warm}}}

   "warn" {:synsem {:cat :verb
                     :sem {:pred :warn}}}

   "wash" [;; 1. non-reflexive
           {:synsem {:cat :verb
                     :sem {:pred :wash}}}
           ;; 2. reflexive
           (let [subject-semantics (atom :top)]
             {:synsem {:cat :verb
                       :sem {:pred :wash-oneself
                             :reflexive true
                             :subj subject-semantics
                             :obj subject-semantics}
                       :subcat {:1 {:sem subject-semantics}
                                :2 {:pronoun true
                                    :reflexive true
                                    :sem subject-semantics}}}})]
   "waste" {:synsem {:cat :verb
                :sem {:pred :waste}}}

   "watch" {:synsem {:cat :verb
                     :sem {:pred :watch}}}

   "water" {:synsem {:cat :noun
                     :sem {:pred :water}}}
   "we"
   [{:english {:note "♀"}
     :synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :1st
                    :gender :fem
                    :number :plur}
              :sem {:human true
                    :gender :fem
                    :pred :noi}
              :subcat '()}}
    {:english {:note "♂"}
     :synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :gender :masc
                   :number :plur}
             :sem {:human true
                   :gender :masc
                   :pred :noi}
             :subcat '()}}]

   "wear"  {:english {:past "wore"}
            :synsem {:cat :verb
                     :sem {:pred :wear}}}
   "whether"
   {:synsem {:cat :comp
             :comp-type :if
             :subcat {:1 {:cat :verb
                          :subcat '()}
                      :2 '()}}}

   "win"  {:synsem {:cat :verb
                    :sem {:pred :win}}
           :english {:past "won"
                     :participle "winning"}}

   "wine" {:synsem {:cat :noun
                    :sem {:pred :vino}}}

   "with" {:synsem {:cat :prep
                    :subcat {:1 {:cat :noun}}
                    :sem {:pred :with}}}

   "woman" {:english {:plur "women"}
            :synsem {:agr {:gender :fem}
                     :cat :noun
                     :sem {:human true
                           :pred :woman
                           :child false}}}

   "wonder" {:synsem {:cat :verb
                      :sem {:pred :wonder}
                      :subcat {:1 {:cat :noun}
                               :2 {:cat :comp
                                   :comp-type :if
                                   :subcat '()}}}}
   "word" {:synsem {:cat :noun
                    :sem {:pred :word}}}

   "work" [{:synsem {:cat :verb
                     :sem {:pred :work-human}}
            :english {:note "human"}}

           {:english {:note "nonliving or machines"} ;; TODO: add support in UI for :note.
            :synsem {:cat :verb
                     :sem {:pred :work-nonhuman}}}]

    "wound" {:synsem {:cat :verb
                     :sem {:pred :wound}}}


   "worry" {:english {:past "worried"
                       :participle "worrying"
                       :past-participle "worried"}
             :synsem {:cat :verb
                      :sem {:pred :worry}}}

   "write"  {:english {:past "wrote"
                       :past-participle "written"}
             :synsem {:cat :verb
                      :sem {:pred :write}}}

   "yell" {:synsem {:cat :verb
                    :sem {:pred :yell}
                    :subcat {:2 {:cat :prep
                                 :sem {:pred :at}}}}}
   "yellow"
   {:synsem {:cat :adjective
             :sem {:mod {:pred :yellow}
                   :comparative false
                   :physical-object true
                   :human false}}}

   "you"
   [{:english {:note "♂"}
     :target :it ;; Italian makes gender distinction for agreement with verbs and adjectives..
     :synsem {:cat :noun
              :pronoun true
              :reflexive false
              :case :top ;; see comment in "it" about :case.
              :agr {:person :2nd
                    :gender :masc
                    :number :sing}
              :sem {:human true
                    :pred :tu}
              :subcat '()}}
    {:english {:note "♀"}
     :target :it ;; Italian makes gender distinction for agreement with verbs and adjectives..
     :synsem {:cat :noun
              :pronoun true
              :case :top ;; see comment in "it" about :case.
              :reflexive false
              :agr {:person :2nd
                    :gender :fem
                    :number :sing}
              :sem {:human true
                    :pred :tu}
              :subcat '()}}

    {:english {:note "♀"}
     :target :es ;; ..but Spanish does not.
     :synsem {:cat :noun
              :pronoun true
              :case :top ;; see comment in "it" about :case.
              :reflexive false
              :agr {:person :2nd
                    :gender :fem
                    :number :sing}
              :sem {:human true
                    :pred :tu}
              :subcat '()}}]

   "you all"
   [{:english {:note "♂"}
     :synsem {:cat :noun
              :pronoun true
              :reflexive false
              :case :top ;; see comment in "it" about :case.
              :agr {:person :2nd
                    :gender :masc
                    :number :plur}
              :sem {:human true
                    :reflexive false
                    :pred :voi}
              :subcat '()}}
    {:english {:note "♀"}
     :synsem {:cat :noun
             :pronoun true
             :reflexive false
             :case :top ;; see comment in "it" about :case.
             :agr {:person :2nd
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :reflexive false
                   :pred :voi}
             :subcat '()}}]

   "your"
   (map #(unify %
                {:synsem {:cat :det
                          :sem {:of {:pred :tu}}
                          :def :possessive}})
        [{:synsem {:agr {:number :sing}}}
         {:synsem {:agr {:number :plur}}}])

   "yourself"
   [{:synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :2nd
                    :number :sing
                    :gender :fem}
              :sem {:human true
                   :pred :tu}
              :subcat '()}}

    {:synsem {:cat :noun
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
   [{:synsem {:cat :noun
              :pronoun true
              :case :acc
              :reflexive true
              :agr {:person :2nd
                    :number :plur
                    :gender :fem}
              :sem {:human true
                    :pred :voi}
              :subcat '()}}

    {:synsem {:cat :noun
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


(declare lexicon)

;; TODO: allow a filter of lexemes
(defn- deliver-lexicon []
  (-> (compile-lex lexicon-source
                   morph/exception-generator
                   morph/phonize)

      apply-unify-key

      ;; <category-independent rules>

      (default
       (let [cat (atom :top)]
         {:english {:cat cat}
          :synsem {:cat cat}}))

      ;; </category-independent rules>

      ;; <noun default rules>

      ;; make :propernoun and :pronoun available to morphological rules
      ;; to prevent e.g. (they -> *theys) or (ourselves -> *ourselvess)
      (default
       (let [pronoun (atom :top)
                        propernoun (atom :top)]
         {:english {:pronoun pronoun
                    :propernoun propernoun}
          :synsem {:cat :noun
                   :pronoun pronoun
                   :propernoun propernoun}}))

      ;; pronouns have semantic number and gender.
      (default
       (let [gender (atom :top)
             number (atom :top)]
         {:synsem {:cat :noun
                              :pronoun true
                   :agr {:gender gender
                         :number number}
                   :sem {:gender gender
                         :number number}}}))

      ;; propernouns have semantic number and gender.
      (default
       (let [gender (atom :top)
             number (atom :top)]
         {:synsem {:cat :noun
                   :propernoun true
                              :agr {:gender gender
                                    :number number}
                   :sem {:gender gender
                         :number number}}}))

      ;; nouns have number-agreement morphology: 'the dog sleeps' vs 'the dogs sleep'
      ;; english.morphology needs to see the :cat=noun as well, so share that within :english.
      (default
       (let [agr (atom :top)]
                    {:english {:agr agr}
                     :synsem {:cat :noun
                              :agr agr}}))
      ;; A pronoun is either reflexive or not reflexive, but
      ;; a non-pronoun is never reflexive.
      (default
       {:synsem {:cat :noun
                 :pronoun false
                 :reflexive false}})

      ;; </noun default rules>

      ;; <verb default rules>
      ;; add a second argument to every verb, unless it's explicitly disallowed with {:2 '()}.
      (default
       {:synsem {:cat :verb
                 :subcat {:2 {:cat :top}}}})

      ;; prevent :modal-with :infinitive matching unless it's already set
      (default
       {:modal-with false
        :synsem {:cat :verb}})

      (default
       (let [modal-subject (atom {:cat :noun})]
         {:modal-with :infinitive
          :synsem {:cat :verb
                   :subcat {:1 modal-subject
                            :2 {:cat :verb
                                :infl :infinitive
                                :subcat {:1 modal-subject
                                         :2 '()}}}}}))
      (default
       (let [modal-subject (atom {:cat :noun})]
         {:modal-with :root
          :synsem {:cat :verb
                   :subcat {:1 modal-subject
                            :2 {:cat :verb
                                :infl :root
                                :subcat {:1 modal-subject
                                         :2 '()}}}}}))

      ;; prevent :shared-semantics :obj unless it's already set
      (default
       {:share-sem false
        :synsem {:cat :verb}})

      ;; semantic object of lexical verb is the same as the object of verb's prepositional phrase.
      (default
       (let [obj (atom :top)]
         {:share-sem :obj
          :synsem {:cat :verb
                   :sem {:obj obj}
                   :subcat {:2 {:cat :prep
                                :sem {:obj obj}}}}}))

      ;; add :sem :obj if necessary, so that intransitivize is triggered.
      (default {:modal-with false
                :synsem {:cat :verb
                         :subcat {:2 {:cat :noun}}
                         :sem {:obj {:pred :top}}}})

      (new-entries ;; remove the second argument and semantic object to make verbs intransitive.
       {:synsem {:cat :verb
                 :aux false
                 :sem {:obj {:top :top}
                       :shared-with-obj false
                       :reflexive false}
                 ;; likely to be :noun or :prep but could be others
                 :subcat {:2 {:cat :top}
                          :3 '()}}}
       (fn [lexeme]
         (unify
          (dissoc-paths lexeme [[:synsem :sem :obj]
                                [:synsem :subcat :2]])
          {:applied {:1 true}
           :synsem {:subcat {:2 '()}}})))

      (default ;; intransitive verbs' :obj is :unspec.
       {:modal-with false
        :applied {:2 true}
        :synsem {:cat :verb
                 :subcat {:1 {:top :top}
                          :2 '()}
                 :sem {:reflexive false
                       :shared-with-obj false
                       :obj :unspec}}})

      ;; subject-verb agreement
      (default (let [infl (atom :top)
                     agr (atom :top)]
                 {:english {:agr agr
                            :infl infl}
                  :synsem {:infl infl
                           :cat :verb
                           :subcat {:1 {:agr agr}}}}))

      (verb-pred-defaults encyc/verb-pred-defaults)

      ;; if a verb has an object,
      ;; and the object is {:cat :noun},
      ;; then the object is {:synsem {:case :acc}}.
      (default {:synsem {:cat :verb
                         :subcat {:2 {:cat :noun
                                      :case :acc}}}})

      ;; aux default: false
      (default {:synsem {:cat :verb
                         :aux false}})

      ;; phrasal-verbs: false
      (default {:synsem {:cat :verb}
                :phrasal-verb false})

      (default
       (let [subject (atom :top)]
         {:synsem {:cat :verb
                   :sem {:subj subject}
                   :subcat {:1 {:sem subject}}}}))

      (default
       (let [object (atom :top)]
         {:phrasal-verb false
          :synsem {:cat :verb
                   :sem {:obj object
                         :iobj nil}
                   :subcat {:2 {:sem object}}}}))

      (default
       (let [object (atom :top)]
         {:phrasal-verb true
          :synsem {:cat :verb
                   :sem {:obj object}
                   :subcat {:3 {:cat :noun
                                :pronoun false
                                :subcat '()
                                :sem object}}}}))
      ;; reflexive=false
      (default {:synsem {:cat :verb
                         :sem {:reflexive false}}})

      (default
       (let [subject-agr (atom :top)]
         {:synsem {:sem {:reflexive true}
                   :cat :verb
                   :subcat {:1 {:agr :subject-agr}
                            :2 {:agr :subject-agr}}}}))
      (default
       {:synsem {:cat :verb
                 :subcat {:2 {:subcat '()}}}})

      ;; </verb default rules>

      ;; <prep default rules>
      (default
       (let [obj-sem (atom :top)]
         {:synsem {:cat :prep
                   :subcat {:1 {:cat :noun
                                :case :acc
                                :subcat '()
                                :sem obj-sem}
                            :2 '()}
                   :sem {:obj obj-sem}}}))

      ;; </prep default rules>

))

(def lexicon-promise (promise))
(def lexicon
  (future
    (if (realized? lexicon-promise)
      @lexicon-promise
      @(deliver lexicon-promise (deliver-lexicon)))))
