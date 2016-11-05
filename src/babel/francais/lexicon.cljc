(ns babel.francais.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.francais.morphology :refer [exception-generator phonize]]
   [babel.francais.pos :refer [gender-pronoun-agreement intransitivize
                               transitivize verb-aux]]
   [babel.lexiconfn :refer [compile-lex if-then map-function-on-map-vals]]
   [babel.pos :as pos :refer [pronoun-acc]]
   [dag_unify.core :refer [get-in unify]]))

(def lexicon-source
  {
   "abandoner" {:synsem {:cat :verb
                         :sem {:pred :abandon}}}

   "abîmeer" {:synsem {:cat :verb
                         :sem {:pred :damage}}}

   "accepter" {:synsem {:cat :verb
                        :sem {:pred :accept}}}

   "accompagner" {:synsem {:cat :verb
                           :sem {:pred :accompany}}}

   "acheter" {:synsem {:cat :verb
                       :sem {:subj {:human true}
                             :pred :comprare}}}

   "aider" {:synsem {:cat :verb
                     :sem {:pred :help}}}

   "aimer" {:synsem {:cat :verb
                     :sem {:pred :amare
                           :subj {:human true}}}}
   "ajouter" {:synsem {:cat :verb
                     :sem {:pred :add}}}

   "aller" {:français {:future-stem "ir"
                       :present {:1sing "vais"
                                 :2sing "vas"
                                 :3sing "va"
                                 :1plur "allons"
                                 :2plur "allez"
                                 :3plur "vont"}}
            :synsem {:cat :verb
                     :essere true
                     :sem {:subj {:animate true}
                           :pred :go}}}

   "annoncer" {:synsem {:cat :verb
                       :sem {:pred :announce}}}

   "appeler" {:synsem {:cat :verb
                       :boot-stem "appell"
                       :future-stem "appell"
                       :sem {:pred :call}}}

   "apporter" [{:synsem {:cat :verb
                         :sem {:pred :take}}}
               {:synsem {:cat :verb
                         :sem {:pred :carry}}}]

   ;;  CONJUGATES LIKE TENIR
   "apprendre" {:synsem {:cat :verb
                         :past-participle "appris"
                         :sem {:pred :learn}}}

   "rester" {:synsem {:cat :verb
                       :essere true
                       :sem {:pred :remain}}}

   "assurer" [{:synsem {:cat :verb
                        :sem {:pred :assure}}}
              {:synsem {:cat :verb
                        :sem {:pred :insure}}}]

   "attendre" {:synsem {:cat :verb
                        :sem {:pred :wait-for}}}

   "augmenter" {:synsem {:cat :verb
                         :sem {:pred :increase}}}
   "avoir"
   (let [common
         {:synsem {:essere false
                   :cat :verb}
          :français {:future-stem "aur"
                     :drop-e true
                     :past-participle "eu"
                     :imperfect-stem "av"
                     :present {:1sing "ai"
                               :2sing "as"
                               :3sing "a"
                               :1plur "avons"
                               :2plur "avez"
                               :3plur "ont"}}}]
     [(unify common {:synsem {:sem {:pred :have
                                    :subj {:human true}}}})
      (unify common verb-aux
             {:synsem {:subcat {:2 {:essere false}}}})])

   "baisser" {:synsem {:cat :verb
                      :sem {:pred :lower}}}

   "boire" {:français {:past-participle "bu"
                       :boot-stem1 "boiv"
                       ;; TODO: boot-stem2 is currently unsupported by babel.francais.morphology:
                       :boot-stem2 "buv"
                       :present {:1sing "bois"
                                 :2sing "boit"
                                 :3sing "boit"}}
            :synsem {:cat :verb
                     :sem {:pred :drink}}}

   "changer" {:synsem {:cat :verb
                      :sem {:pred :change}}}

   "chanter" {:synsem {:cat :verb
                       :sem {:pred :sing}}}

   "commencer" {:synsem {:cat :verb
                         :sem {:pred :begin}}}

   "commenter" {:synsem {:cat :verb
                         :sem {:pred :comment}}}

   "comprendre" {:synsem {:cat :verb
                          :past-participle "compris"
                          :sem {:pred :understand}}}

   "conserver" [{:synsem {:cat :verb
                          :sem {:pred :conserve}}}
                {:synsem {:cat :verb
                          :sem {:pred :preserve}}}]

   "considérer" {:synsem {:cat :verb
                         :sem {:pred :consider}}}

   "couper" {:synsem {:cat :verb
                      :sem {:pred :cut}}}

   ;; TODO: with 'courir', *everything* is an exception: this can probably be done better via
   ;; morphology (francais/morphology/verbs.clj). Surely there are some regularities that we are missing.
   "courir" {:français {:past-participle "couru"
                        :present {:1sing "cours"
                                  :2sing "cours"
                                  :3sing "court"
                                  :1plur "courons"
                                  :2plur "courez"
                                  :3plur "courent"}
                        :imperfect {:1sing "courais"
                                    :2sing "courais"
                                    :3sing "courait"
                                    :1plur "courions"
                                    :2plur "couriez"
                                    :3plur "couraient"}
                        :future {:1sing "courrai"
                                 :2sing "courras"
                                 :3sing "courra"
                                 :1plur "courrons"
                                 :2plur "courrez"
                                 :3plur "courront"}
                        :conditional  {:1sing "courrais"
                                       :2sing "courrais"
                                       :3sing "courrait"
                                       :1plur "courrions"
                                       :2plur "courriez"
                                       :3plur "courraient"}}
             :synsem {:cat :verb
                      :sem {:pred :run}}}

   "créer" {:synsem {:cat :verb
                     :sem {:pred :create}}}

   "croire" {:synsem {:cat :verb
                      :sem {:pred :believe}}}

   "danser" {:synsem {:cat :verb
                       :sem {:pred :dance}}}

   "decider" {:synsem {:cat :verb
                       :sem {:pred :decide}}}

   "demander" {:synsem {:cat :verb
                       :sem {:pred :ask-for}}}


   "developer" {:synsem {:cat :verb
                        :sem {:pred :develop}}}
   "devoir"
   (let [common
         {:synsem {:essere false
                   :cat :verb}
          :français {:future-stem "devr"
                     :drop-e true
                     :past-participle "dû"
                     :imperfect-stem "dev"
                     :present {:1sing "dois"
                               :2sing "dois"
                               :3sing "doit"
                               :1plur "devons"
                               :2plur "devez"
                               :3plur "doivent"}}}]
     (unify common {:synsem {:sem {:pred :have-to
                                   :subj {:human true}}}}))



   "désirer" {:synsem {:cat :verb
                       :sem {:pred :desire}}}

   "devenir"
   (let [common
         {:synsem {:essere true
                   :cat :verb}
          :français {:future-stem "deviendr"
                     :drop-e true
                     :past-participle "devenu"
                     :imperfect-stem "deven"
                     :present {:1sing "devienss"
                               :2sing "deviens"
                               :3sing "devient"
                               :1plur "devenons"
                               :2plur "devenez"
                               :3plur "deviennent"}}}]
     (unify common {:synsem {:sem {:pred :become
                                   :subj {:human true}}}}))


   "dire"
   (let [shared-part-of-dire
         {:synsem {:cat :verb}
          :français {:français "dire"
                     :past-participle "dit"
                     :future-stem "dir"
                     :imperfect-stem "dis"
                     :present {:1sing "dis"
                               :2sing "dis"
                               :3sing "dit"
                               :1plur "disons"
                               :2plur "dites"
                               :3plur "disent"}}}]
     [(unify shared-part-of-dire
             {:synsem {:sem {:pred :say}}})
      (unify shared-part-of-dire
             {:synsem {:sem {:pred :tell}}})])

   "diviser" {:synsem {:cat :verb
                      :sem {:pred :divide}}}

   "donner" {:synsem {:cat :verb
                      :sem {:pred :give}}}

   "dormir" {:synsem {:cat :verb
                      :sem {:pred :sleep}}
             :français {:past-participle "dit"
                        :present {:1sing "dors"
                                  :2sing "dors"
                                  :3sing "dort"
                                  :1plur "dormons"
                                  :2plur "dormez"
                                  :3plur "dorment"}}}

   "echapper" {:synsem {:cat :verb
                        :sem {:pred :escape}}}

   "écouter" {:synsem {:cat :verb
                       :sem {:pred :listen-to}}}

   "effacer" {:synsem {:cat :verb
                       :sem {:pred :erase}}}
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
   {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :plur
                    :gender :fem}
              :sem {:gender :fem
                    :pred :loro}
              :subcat '()}}

   "enseigner" [{:synsem {:cat :verb
                         :sem {:pred :show}}}
               {:synsem {:cat :verb
                         :sem {:pred :teach}}}]

   "entrer" {:synsem {:cat :verb
                      :essere true
                      :sem {:pred :enter}}}

   "envoyer" {:synsem {:cat :verb
                       :sem {:pred :send}}}

   "essayer" {:synsem {:cat :verb
                      :sem {:pred :try}}}

    "éteindre" {:synsem {:cat :verb
                         :sem {:pred :turn-off}
                         :essere false}
                :français {:future-stem "éteind"
                           :infinitive "éteindre"
                           :present {:1sing "éteins"
                                     :2sing "éreins"
                                     :3sing "éteint"
                                     :1plur "éteignons"
                                     :2plur "éteignez"
                                     :3plur "éteignent"}
                          :past-participle "étreint"
                          :imperfect-stem "éteign"}}

    "être"
   (let [common
         {:synsem {:cat :verb
                   :essere false}
          :français {:future-stem "ser"
                     :infinitive "être"
                     :present {:1sing "suis"
                               :2sing "es"
                               :3sing "est"
                               :1plur "sommes"
                               :2plur "êtes"
                               :3plur "sont"}
                     :past-participle "été"
                     :imperfect {:1sing "étais"
                                 :2sing "étais"
                                 :3sing "était"
                                 :1plur "étions"
                                 :2plur "étiez"
                                 :3plur "étaient"}
                     :future {:1sing "serai"
                              :2sing "seras"
                              :3sing "sera"
                              :1plur "serons"
                              :2plur "serez"
                              :3plur "seront"}}}]
     [(unify common {:synsem {:sem {:pred :be}}})
      (unify common verb-aux
             {:synsem {:subcat {:2 {:essere true}}}})])
   ;; ^^ in auxiliary form, "être" only allows essere=true verbs.
   ;; compare with "avoir", which only allows essere=false verbs.

   "étreindre" {:synsem {:cat :verb
                         :sem {:pred :hug}
                         :essere false}
                :français {:future-stem "étreind"
                           :infinitive "étreindre"
                           :present {:1sing "étreins"
                                     :2sing "étreins"
                                     :3sing "étreint"
                                     :1plur "étreignons"
                                     :2plur "étreignez"
                                     :3plur "étreignent"}
                          :past-participle "étreint"
                          :imperfect-stem "étreign"}}

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

   "faire"
      (let [shared-part-of-faire
            {:synsem {:cat :verb}
             :français {:français "faire"
                        :drop-e false
                        :past-participle "fait"
                        :future-stem "fer"
                        :present {:1sing "fais"
                                  :2sing "fais"
                                  :3sing "fait"
                                  :1plur "faisons"
                                  :2plur "faites"
                                  :3plur "font"}}}]

        [(unify shared-part-of-faire
             {:synsem {:sem {:pred :do}}})
         (unify shared-part-of-faire
                {:synsem {:sem {:pred :make}}})])

   "former" {:synsem {:cat :verb
                     :sem {:pred :form}}}


   "former" {:synsem {:cat :verb
                     :sem {:pred :form}}}

   "gagner" [{:synsem {:cat :verb
                       :sem {:pred :earn
                             :subj {:human true}}}}
             {:synsem {:cat :verb
                       :sem {:pred :win
                             :subj {:human true}}}}]

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
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :plur
                    :gender :masc}
              :sem {:gender :masc
                    :pred :loro}
               :subcat '()}}

    "imaginer" {:synsem {:cat :verb
                       :sem {:pred :imagine}}}
  "importer" {:synsem {:cat :verb
                       :sem {:pred :import}}}

  "insister" {:synsem {:cat :verb
                       :sem {:pred :insist}}}

;;  "interessér" {:synsem {:cat :verb
;;                         :sem {:pred :interest??}}}

  "je"
  (let [common {:synsem {:cat :noun
                         :pronoun true
                         :case :nom
                         :agr {:person :1st
                               :number :sing}
                         :sem {:human true
                               :pred :I}
                         :subcat '()}}]
    [(unify gender-pronoun-agreement
            common
            {:synsem {:agr {:gender :fem}}})
     (unify gender-pronoun-agreement
            common
            {:synsem {:agr {:gender :masc}}})])

   "Jean" {:synsem {:sem {:pred :Jean
                          :human true}
                    :propernoun true
                    :agr {:number :sing
                          :person :3rd
                          :gender :masc}}}

   "la" {:synsem {:cat :noun
                  :pronoun true
                  :reflexive true
                  :case pronoun-acc
                  :agr {:person :3rd
                        :gender :fem
                        :number :sing}
                  :subcat '()}}

   "laisser" {:synsem {:cat :verb
                      :sem {:pred :leave-behind}}}

   "le" {:synsem {:cat :noun
                  :pronoun true
                  :reflexive true
                  :case pronoun-acc
                  :agr {:person :3rd
                        :gender :masc
                        :number :sing}
                  :subcat '()}}

   "mal" {:synsem {:cat :adverb
                   :sem {:pred :bad}}}

  "manger"
  {:synsem {:cat :verb
            :sem {:pred :eat}}}

  "manquer" {:synsem {:cat :verb
                      :sem {:pred :to-be-missing}}}

  "marcher" [{:synsem {:cat :verb
                       :sem {:pred :walk}}}
             {:synsem {:cat :verb
                       :sem {:subj {:human false}
                              :pred :work-nonhuman}}}]

   "me" {:synsem {:cat :noun
                  :pronoun true
                  :reflexive true
                  :case pronoun-acc
                  :agr {:person :1st
                        :number :sing}
                  :subcat '()}}

  "mettre" {:synsem {:cat :verb
                     :passato "mis"
                     :present {:1sing "mets"
                               :2sing "mets"
                               :3sing "met"
                               :1plur "mettons"
                               :2plur "mettez"
                               :3plur "mettent"}

                     :sem {:pred :put}}}

  "naître" {:synsem {:cat :verb
                     :future-stem "naîtr"
                     :imperfect-stem "naiss"
                     :passato "né"
                     :present {:1sing "nais"
                               :2sing "nais"
                               :3sing "naît"
                               :1plur "naissons"
                               :2plur "naissez"
                               :3plur "naissent"}

                     :sem {:pred :be-born}}}

  "nous"
  (let [common {:synsem {:case :top
                         :agr {:person :1st
                               :number :plur}
                         :sem {:human true
                               :pred :noi}}}]
    [(unify gender-pronoun-agreement
            common
            {:synsem {:agr {:gender :fem}}})
     (unify gender-pronoun-agreement
            common
            {:synsem {:agr {:gender :masc}}})])

   "observer" {:synsem {:cat :verb
                       :sem {:pred :observe}}}

   "oublier" {:synsem {:cat :verb
                      :sem {:pred :forget}}}
   "parler"
   [{:synsem {:cat :verb
              :sem {:pred :speak
                    :subj {:human true}}}}
    {:synsem {:cat :verb
              :sem {:pred :talk
                    :subj {:human true}}}}]

   "partager" {:synsem {:cat :verb
                       :sem {:pred :share}}}

   "participer" {:synsem {:cat :verb
                         :sem {:pred :participate}}}

   "peindre" {:français {:boot-stem1 "pein"
                         ;; TODO: boot-stem2 is currently unsupported by babel.francais.morphology:
                         :boot-stem2 "peign"
                         :passé "peint"
                         :future-stem "paindr"
                         :imperfect "peign"}
              :synsem {:cat :verb
                       :sem {:pred :paint}}}

   "profiter (de)" {:synsem {:cat :verb
                             :sem {:pred :take-advantage-of}}}

  "prendre" [{:synsem {:cat :verb
                       :past-participle "pris"
                       :sem {:pred :grab}}}
             {:synsem {:cat :verb
                       :past-participle "pris"
                       :sem {:pred :take}}}]
  "pouvoir"
   (let [shared-part-of-pouvoir
         {:synsem {:essere false
                   :cat :verb}
          :français {:future-stem "pourr"
                     :drop-e true
                     :past-participle "pu"
                     :imperfect-stem "pouv"
                     :present {:1sing "peux"
                               :2sing "peux"
                               :3sing "peut"
                               :1plur "pouvons"
                               :2plur "pouvez"
                               :3plur "peuvent"}}}]
     [(unify shared-part-of-pouvoir
             {:synsem {:sem {:pred :can}}})
      (unify shared-part-of-pouvoir
             {:synsem {:sem {:pred :may}}})
      (unify shared-part-of-pouvoir
             {:synsem {:sem {:pred :be-able-to}}})])

  "regarder" [{:synsem {:cat :verb
                        :sem {:pred :look}}}
              {:synsem {:cat :verb
                        :sem {:pred :watch}}}]

  "remarquer" {:synsem {:cat :verb
                        :sem {:pred :note}}}

  "répondre" {:synsem {:cat :verb
                       :sem {:pred :answer}}}

   "se" {:synsem {:cat :noun
                  :pronoun true
                  :reflexive true
                  :case pronoun-acc
                  :agr {:person :3rd
                        :number :plur}
                  :subcat '()}}

   "s'amuser" (let [subject-semantics (atom {:human true})
                    subject-agr (atom :top)]
                {:synsem {:cat :verb
                          :essere true
                          :sem {:pred :have-fun
                                :reflexive true
                                :subj subject-semantics
                                :obj subject-semantics}
                          :subcat {:1 {:agr subject-agr
                                       :sem subject-semantics}
                                   :2 {:agr subject-agr
                                       :pronoun true
                                       :reflexive true
                                       :sem subject-semantics}}}})
   "s'appeler"
   (let [subject-semantics (atom :top)
         called-semantics (atom :top)
         subject-gender (atom :top)
         subject-person (atom :top)
         subject-number (atom :top)]
     {:synsem {:cat :verb
               :sem {:aspect :progressive
                     :pred :be-called
                     :reflexive true
                     :tense :present
                     :subj subject-semantics
                     :obj called-semantics}
               :subcat {:1 {:propernoun false
                            :agr {:number subject-number
                                  :person subject-person
                                  :gender subject-gender}}
                        :2 {:agr {:number subject-number
                                  :person subject-person
                                  :gender subject-gender}
                            :pronoun true
                            :reflexive true}
                        :3 {:agr {:number subject-number
                                  :gender subject-gender}
                            :propernoun true
                            :sem called-semantics
                            :cat :noun
                            :subcat '()}}}})

   "se blesser" (let [subject-semantics (atom {:human true})
                      subject-agr (atom :top)]
                  {
                   :synsem {:cat :verb
                            :essere true
                            :sem {:pred :hurt-oneself
                                  :reflexive true
                                  :subj subject-semantics
                                  :obj subject-semantics}
                            :subcat {:1 {:agr subject-agr
                                         :sem subject-semantics}
                                     :2 {:agr subject-agr
                                         :pronoun true
                                         :reflexive true
                                         :sem subject-semantics}}}})

   "se changer" (let [subject-semantics (atom {:human true})
                      subject-agr (atom :top)]
                  {
                   :synsem {:cat :verb
                            :essere true
                            :sem {:pred :change-clothes
                                  :reflexive true
                                  :subj subject-semantics
                                  :obj subject-semantics}
                            :subcat {:1 {:agr subject-agr
                                         :sem subject-semantics}
                                     :2 {:agr subject-agr
                                         :pronoun true
                                         :reflexive true
                                         :sem subject-semantics}}}})

   ;; not supported in grammar yet.
   ;;                                     :3 {:cat :adverb
   ;;                                         :sem {:pred :bad}}}}})

  "s'endormir" (let [subject-semantics (atom {:human true})
                    subject-agr (atom :top)]
                {:synsem {:cat :verb
                          :essere true
                          :sem {:pred :fall-asleep
                                :reflexive true
                                :subj subject-semantics
                                :obj subject-semantics}
                          :subcat {:1 {:agr subject-agr
                                       :sem subject-semantics}
                                   :2 {:agr subject-agr
                                       :pronoun true
                                       :reflexive true
                                       :sem subject-semantics}}}})

  "s'énerver" (let [subject-semantics (atom {:human true})
                    subject-agr (atom :top)]
                {:synsem {:cat :verb
                          :essere true
                          :sem {:pred :get-angry
                                :reflexive true
                                :subj subject-semantics
                                :obj subject-semantics}
                          :subcat {:1 {:agr subject-agr
                                       :sem subject-semantics}
                                   :2 {:agr subject-agr
                                       :pronoun true
                                       :reflexive true
                                       :sem subject-semantics}}}})

  "s'ennuyer" (let [subject-semantics (atom {:human true})
                    subject-agr (atom :top)]
                {:synsem {:cat :verb
                          :essere true
                          :français {:future-stem "ennui"
                                     :boot-stem "ennui"}
                          :sem {:pred :get-bored
                                :reflexive true
                                :subj subject-semantics
                                :obj subject-semantics}
                          :subcat {:1 {:agr subject-agr
                                       :sem subject-semantics}
                                   :2 {:agr subject-agr
                                       :pronoun true
                                       :reflexive true
                                       :sem subject-semantics}}}})

 "s'enrager" (let [subject-semantics (atom {:human true})
                    subject-agr (atom :top)]
                {:synsem {:cat :verb
                          :essere true
                          :sem {:pred :get-angry
                                :reflexive true
                                :subj subject-semantics
                                :obj subject-semantics}
                          :subcat {:1 {:agr subject-agr
                                       :sem subject-semantics}
                                   :2 {:agr subject-agr
                                       :pronoun true
                                       :reflexive true
                                       :sem subject-semantics}}}})

  "se lever" (let [subject-semantics (atom {:human true})
                    subject-agr (atom :top)]
                {:synsem {:cat :verb
                          :essere true
                          :sem {:pred :get-up
                                :reflexive true
                                :subj subject-semantics
                                :obj subject-semantics}
                          :subcat {:1 {:agr subject-agr
                                       :sem subject-semantics}
                                   :2 {:agr subject-agr
                                       :pronoun true
                                       :reflexive true
                                       :sem subject-semantics}}}})
  "soulever" {:synsem {:cat :verb
                       :sem {:pred :lift}}}

  "soutenir" {:synsem {:cat :verb
                       :sem {:pred :support}}}

   "te" {:synsem {:cat :noun
                  :pronoun true
                  :reflexive true
                  :case pronoun-acc
                  :agr {:person :2nd
                        :number :sing}
                  :subcat '()}}

   "terminer" {:synsem {:cat :verb
                       :sem {:pred :finish}}}

  "touer" {:synsem {:cat :verb
                    :sem {:pred :kill}}}

  "trouver" {:synsem {:cat :verb
                    :sem {:pred :find}}}


  "tu"
  (let [common {:synsem {:cat :noun
                         :case :nom
                         :agr {:person :2nd
                               :number :sing}
                         :sem {:human true
                               :pred :tu}}}]
    [(unify gender-pronoun-agreement
            common
            {:synsem {:agr {:gender :fem}}})
     (unify gender-pronoun-agreement
            common
            {:synsem {:agr {:gender :masc}}})])

  "venir"
   (let [common
         {:synsem {:essere true
                   :cat :verb}
          :français {:future-stem "viendr"
                     :drop-e true
                     :past-participle "venu"
                     :imperfect-stem "ven"
                     :present {:1sing "vienss"
                               :2sing "viens"
                               :3sing "vient"
                               :1plur "venons"
                               :2plur "venez"
                               :3plur "viennent"}}}]
     (unify common {:synsem {:sem {:pred :come
                                   :subj {:human true}}}}))

  "vous"
  (let [common {:synsem {:case :top
                         :agr {:person :2nd
                               :number :plur}
                         :sem {:human true
                               :pred :voi}}}]
    [(unify gender-pronoun-agreement
            common
            {:synsem {:agr {:gender :fem}}})
     (unify gender-pronoun-agreement
            common
            {:synsem {:agr {:gender :masc}}})])
   })

(def lexicon
  (-> (compile-lex lexicon-source exception-generator phonize)

      ;; make an intransitive version of every verb which has a path [:sem :obj].
      intransitivize

      ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
      transitivize

      (if-then {:synsem {:cat :verb
                         :subcat {:3 '()}}}
               {:synsem {:subcat {:3 '()}}})

      ;; if object is not specified, then set to :unspec.
      ;; this prevents translations that may have actual objects - e.g. would allow translations like:
      ;; "Je mange" => "I eat the bread" whereas a better translation is "I eat".
      (if-then {:synsem {:cat :verb
                         :aux false
                         :sem {:obj :unspec
                               :reflexive false
                               }}}
               {:synsem {:sem {:obj :unspec}}})

      ;; TODO: use lexiconfn/if-then here, like espanol/lexicon does.
      ;; default: essere=false
      (map-function-on-map-vals
       (fn [k vals]
         (map (fn [val]
                ;; if: 1. the val's :cat is :verb
                ;;     2. it is not true that essere=true (either essere=false or essere is not defined)
                ;; then: essere=false
                (cond (and (= (get-in val [:synsem :cat])
                              :verb)
                           (not (= true (get-in val [:synsem :essere] false))))
                      (unify val {:synsem {:essere false}})

                      true ;; otherwise, leave the verb alone
                      val))
              vals)))

      ;; Cleanup functions can go here. Number them for ease of reading.
      ;; 1. this filters out any verbs without an inflection:
      ;; infinitive verbs should have inflection ':infinitive',
      ;; rather than not having any inflection.
      (map-function-on-map-vals
       (fn [k vals]
         (filter #(or (not (= :verb (get-in % [:synsem :cat])))
                      (not (= :none (get-in % [:synsem :infl] :none))))
                 vals)))))
