(ns babel.francais.lexicon
  (:require
   [babel.lexiconfn :refer (unify)]
   [babel.francais.pos :refer :all]))

(def lexicon-source 
  {
   "abandoner" {:synsem {:cat :verb
                         :sem {:pred :abandon}}}

   "accepter" {:synsem {:cat :verb
                        :sem {:pred :accept}}}

   "accompagner" {:synsem {:cat :verb
                           :sem {:pred :accompany}}}

   "acheter" {:synsem {:cat :verb
                       :sem {:subj {:human true}
                             :pred :comprare}}}

   "aider" {:synsem {:cat :verb
                     :sem {:pred :aiutare}}}
   
   "aller" {:français {:essere true
                       :future-stem "ir"
                       :present {:1sing "vais"
                                 :2sing "vas"
                                 :3sing "va"
                                 :1plur "allons"
                                 :2plur "allez"
                                 :3plur "vont"}}
            :synsem {:cat :verb
                     :essere true
                     :sem {:subj {:animate true}
                           :pred :andare}}}

   "anoncier" {:synsem {:cat :verb
                        :sem {:pred :announce}}}

   "appeler" {:synsem {:cat :verb
                       :sem {:pred :call}}}

   "apporter" [{:synsem {:cat :verb
                         :sem {:pred :take}}}
               {:synsem {:cat :verb
                         :sem {:pred :carry}}}]

   ;;  CONJUGATES LIKE TENIR
   "apprendre" {:synsem {:cat :verb
                         :sem {:pred :imparare}}}
   
   "assurer" [{:synsem {:cat :verb
                        :sem {:pred :assure}}}
              {:synsem {:cat :verb
                        :sem {:pred :insure}}}]

   "attendre" {:synsem {:cat :verb
                         :sem {:pred :wait}}}
                       
   "augmenter" {:synsem {:cat :verb
                         :sem {:pred :increase}}}                   
   "avoir"
   {:synsem {:essere false
             :sem {:pred :avere}
             :cat :verb}
    :français {:futuro-stem "aur"
               :drop-e true
               :imperfect-stem "av"
               :present {:1sing "ai"
                         :2sing "as"
                         :3sing "a"
                         :1plur "avons"
                         :2plur "avez"
                         :3plur "ont"}}}

   "baisser" {:synsem {:cat :verb
                      :sem {:pred :lower}}}

   "boire" {:français {:passato "bu"
                       :boot-stem1 "boiv"
                       :boot-stem2 "buv"}
            :synsem {:cat :verb
                     :sem {:pred :drink}}}

   "changer" {:synsem {:cat :verb
                      :sem {:pred :cambiare}}}
   
   "chanter" {:synsem {:cat :verb
                       :sem {:pred :sing}}}
   
   "commencer" {:synsem {:cat :verb
                         :sem {:pred :begin}}}

   "commenter" {:synsem {:cat :verb
                         :sem {:pred :comment}}}
   
   "comprendre" {:synsem {:cat :verb
                          :sem {:pred :understand}}}

   "conserver" [{:synsem {:cat :verb
                          :sem {:pred :conserve}}}
                {:synsem {:cat :verb
                          :sem {:pred :preserve}}}]
   
   "considérer" {:synsem {:cat :verb
                         :sem {:pred :consider}}}
    
   "couper" {:synsem {:cat :verb
                      :sem {:pred :cut}}}
  
   "courir" {:synsem {:cat :verb
                      :sem {:pred :run}}}

   "créer" {:synsem {:cat :verb
                     :sem {:pred :create}}}
   
   "decider" {:synsem {:cat :verb
                       :sem {:pred :decide}}}

   "developer" {:synsem {:cat :verb
                        :sem {:pred :develop}}}

   "devoir" {:synsem {:cat :verb
                     :sem {:pred :have-to}}}

   "diviser" {:synsem {:cat :verb
                      :sem {:pred :divide}}}

   "ecouter" {:synsem {:cat :verb
                       :sem {:pred :listen}}}                  
   
   "effacer" {:synsem {:cat :verb
                       :sem {:pred :erase}}}
   
   "croire" {:synsem {:cat :verb
                      :sem {:pred :believe}}}  
   
   "décider" {:synsem {:cat :verb
                       :sem {:pred :decide}}}
   
   "désirer" {:synsem {:cat :verb
                       :sem {:pred :desire}}}
   
   "donner" {:synsem {:cat :verb
                      :sem {:pred :give}}}
   
   "dormir" {:synsem {:cat :verb
                      :sem {:pred :sleep}}}

   "echapper" {:synsem {:cat :verb
                        :sem {:pred :escape}}}

   "écouter" {:synsem {:cat :verb
                      :sem {:pred :listen-to}}}
   "elle"
   [{:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :number :sing
                   :gender :fem}
              :sem {:human true
                    :pred :lei}
              :subcat '()}}
    {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :number :sing
                   :gender :fem}
              :sem {:human false
                    :pred :lei}
             :subcat '()}}]
   "elles"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :plur
                    :gender :fem}
              :sem {:human true
                    :gender :fem
                    :pred :loro}
              :subcat '()}}
    {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :number :plur
                   :gender :fem}
             :sem {:human false
                   :gender :fem
                   :pred :loro}
             :subcat '()}}]

   "enseigner" [{:synsem {:cat :verb
                         :sem {:pred :show}}}
               {:synsem {:cat :verb
                         :sem {:pred :teach}}}]

   "entrer" {:synsem {:cat :verb
                     :sem {:pred :enter}}}

   "envoyer" {:synsem {:cat :verb
                       :sem {:pred :send}}}

   "essayer" {:synsem {:cat :verb
                      :sem {:pred :try}}}

   "être"
   (let [common
         {:synsem {:cat :verb
                   :essere false}
          :français {:futuro-stem "ser"
                     :infinitive "être"
                     :present {:1sing "suis"
                               :2sing "es"
                               :3sing "est"
                               :1plur "sommes"
                               :2plur "êtes"
                               :3plur "sont"}
                     :passato "été"
                     :imperfect {:1sing "étais"
                                 :2sing "étais"
                                 :3sing "était"
                                 :1plur "étions"
                                 :2plur "étiez"
                                 :3plur "étaient"}
                     :futuro {:1sing "serai"
                        :2sing "seras"
                              :3sing "sera"
                              :1plur "serons"
                              :2plur "serez"
                              :3plur "seront"}}}]
     [(unify common {:synsem {:sem {:pred :be}}})
      (unify common verb-aux)])

     
   "étudier" {:synsem {:cat :verb
                       :sem {:pred :study}}}
   
   "éviter" {:synsem {:cat :verb
                      :sem {:pred :avoid}}}
  
   "exister" {:synsem {:cat :verb
                       :sem {:pred :exist}}}
   
   "exprimer" {:synsem {:cat :verb
                        :sem {:pred :express}}}
   
   "expulser" {:synsem {:cat :verb
                        :sem {:pred :throw-out}}}

   "former" {:synsem {:cat :verb
                     :sem {:pred :form}}}
   
   "gagner" [{:synsem {:cat :verb
                      :sem {:pred :earn}}}
            {:synsem {:cat :verb
                      :sem {:pred :win}}}]

   "gérer" {:synsem {:cat :verb
                    :sem {:pred :manage}}}
   "il"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing
                    :gender :masc}
              :sem {:human true
                    :pred :lui}
              :subcat
              '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing
                    :gender :masc}
              :sem {:human false
                    :pred :lui}
              :subcat
              '()}}]

    "ils"
    [{:synsem {:cat :noun
               :pronoun true
               :case :nom
               :agr {:person :3rd
                     :number :plur
                     :gender :masc}
               :sem {:human true
                     :gender :masc
                     :pred :loro}
               :subcat '()}}
     {:synsem {:cat :noun
               :pronoun true
               :case :nom
               :agr {:person :3rd
                     :number :plur
                     :gender :masc}
               :sem {:human false
                     :gender :masc
                     :pred :loro}
               :subcat '()}}]

  "imaginer" {:synsem {:cat :verb
                       :sem {:pred :imagine}}}
  "importer" {:synsem {:cat :verb
                       :sem {:pred :import}}}

  "insister" {:synsem {:cat :verb
                       :sem {:pred :insist}}}

;;  "interessér" {:synsem {:cat :verb
;;                         :sem {:pred :interest??}}}

  "je" {:synsem {:cat :noun
                 :pronoun true
                 :case :nom
                 :agr {:person :1st
                       :number :sing}
                 :sem {:human true
                       :pred :I}
                 :subcat '()}}
  
  "laisser" {:synsem {:cat :verb
                      :sem {:pred :leave-behind}}}

  "manger"
  {:synsem {:cat :verb
            :sem {:pred :mangiare}}}
  
  "manquer" {:synsem {:cat :verb
                      :sem {:pred :to-be-missing}}}
  
  "marcher" [{:synsem {:cat :verb
                       :sem {:pred :walk}}}
             {:synsem {:cat :verb
                       :sem {:subj {:human false}
                              :pred :work-nonhuman}}}]

  "mettre" {:synsem {:cat :verb
                     :sem {:pred :set}}}
  "nous"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :number :plur}
              :sem {:human true
                    :pred :noi}
             :subcat '()}}

   "observer" {:synsem {:cat :verb
                       :sem {:pred :observe}}}

   "oublier" {:synsem {:cat :verb
                      :sem {:pred :forget}}}
   "parler"
   [{:synsem {:cat :verb
              :sem {:pred :speak}}}
    {:synsem {:cat :verb
              :sem {:pred :talk}}}]

   "partager" {:synsem {:cat :verb
                       :sem {:pred :share}}}

   "participer" {:synsem {:cat :verb
                         :sem {:pred :participate}}}

   "peindre" {:français {:boot-stem1 "pein"
                         :boot-stem2 "peign"
                         :passé "peint"
                         :futuro-stem "paindr"
                         :imperfect "peign"}
              :synsem {:cat :verb
                       :sem {:pred :paint}}}
;  "profiter (de)" {:synsem {:cat :verb
;                            :sem {:pred :take-advantage-of}}}


  "regarder" [{:synsem {:cat :verb
                        :sem {:pred :look}}}
              {:synsem {:cat :verb
                        :sem {:pred :watch}}}]
  
  "remarquer" {:synsem {:cat :verb
                        :sem {:pred :note}}}

  "répondre" {:synsem {:cat :verb
                       :sem {:pred :answer}}}

;  "s'amuser" (let [subject-semantics (ref {:human true})
;                    subject-agr (ref :top)]
;                {:synsem {:cat :verb
;                          :essere true
;                          :sem {:pred :have-fun
;                                :reflexive true
;                                :subj subject-semantics
;                                :obj subject-semantics}
;                          :subcat {:1 {:agr subject-agr
;                                       :sem subject-semantics}
;                                   :2 {:agr subject-agr
;                                       :pronoun true
;                                       :reflexive true
;                                       :sem subject-semantics}}}})
  "soulever" {:synsem {:cat :verb
                       :sem {:pred :lift}}}

  "soutenir" {:synsem {:cat :verb
                       :sem {:pred :support}}}

  "terminer" {:synsem {:cat :verb
                       :sem {:pred :finish}}}

  "touer" {:synsem {:cat :verb
                    :sem {:pred :kill}}}
  "tu"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :number :sing}
              :sem {:human true
                    :pred :tu}
             :subcat '()}}
   "vous"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :number :plur}
              :sem {:human true
                    :pred :voi}
             :subcat '()}}  
   })






