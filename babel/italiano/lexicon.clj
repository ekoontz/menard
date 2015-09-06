(ns babel.italiano.lexicon
  (:require
   [babel.lexiconfn :refer (unify)]
   [babel.italiano.pos :refer :all]))

(def lexicon-source
  {"Antonia"
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
   [(let [location (ref {:place true})]
      {:synsem {:cat :prep
                :sem {:pred :a
                      :obj location
                      :comparative false}
                :subcat {:1 {:cat :noun
                             :subcat '()
                             :sem location}
                         :2 '()}}})

       {:synsem {:cat :prep
             :sem {:pred :in}
             :subcat {:1 {:cat :noun
                          :sem {:city true}}}}}

       ;; e.g. "a ridere": tu hai fatto bene a ridere (you did well to laugh)"
       (let [complement-semantics (ref {:pred :a
                                        :mod {:pred :a}})]
         {:synsem {:cat :prep
                   :sem complement-semantics
                   :subcat {:1 {:cat :verb
                                :sem complement-semantics
                                :subcat {:1 :top
                                         :2 '()}}
                            :2 '()}}})]

   "abbandonare" {:synsem {:cat :verb
                           :sem {:pred :abandon}}}

   "abbassare"  {:synsem {:cat :verb
                          :sem {:pred :lower}}}
   "abbracciare"
    {:synsem {:cat :verb
              :essere false
              :sem {:pred :abbracciare
                    :activity false
                    :discrete false
                    :subj {:human true}
                    :obj {:animate true}}}}

   "accettare" {:synsem {:cat :verb
                          :sem {:pred :accept}}}

   "accompagnare" {:synsem {:cat :verb
                            :sem {:pred :accompany}}}

   "acqua"
   (unify (:agreement noun)
          (:drinkable noun)
          (:feminine noun)
          {:synsem {:sem {:artifact false
                          :animate false
                          :pred :acqua}}})

   "affolato"
   [;; comparative
    (let [is-place (ref {:place true}) ;; only places can be crowded.
          than-this (ref {:pred :di
                          :mod is-place})]
      (unify adjective
              comparative
              {:synsem {:sem {:pred :affolato
                              :arg1 is-place
                              :arg2 is-place}
                        :subcat {:1 {:cat :noun
                                     :sem is-place}
                                 :2 {:cat :prep
                                     :sem than-this}}}}))
    ;; non-comparative
    (unify adjective
            {:synsem {:cat :adjective
                      :sem {:pred :affolato
                            :comparative false
                            :place true}}})

    ] ;; only places can be crowded.

   "aggiungere" {:synsem {:cat :verb
                          :sem {:pred :add}}}
   "aiutare"
    {:synsem {:essere false
              :sem {:pred :aiutare
                    :activity true
                    :obj {:human true}}}}
   "alto"
   [;; non-comparative:
    (let [subject-sem (ref {:human true}) ;; only humans can be tall.
          subject-agr (ref :top)] 
      (unify adjective
             non-comparative-adjective
             {:synsem {:cat :adjective
                       :sem {:pred :alto
                             :comparative false
                             :arg1 subject-sem
                             :human true}
                       :subcat {:1 {:cat :noun
                                    :agr subject-agr
                                    :sem subject-sem}
                                :2 '()}}}))
    ;; comparative:
    (let [complement-complement-sem (ref {:human true}) ;; only humans can be tall.
          complement-sem (ref {:pred :di
                               :mod complement-complement-sem})
          subject-sem (ref {:human true})] ;; only humans can be tall.
      (unify adjective
             comparative
             {:synsem {:sem {:pred :alto
                             :arg1 subject-sem
                             :arg2 complement-complement-sem}
                       :subcat {:1 {:cat :noun
                                    :sem subject-sem}
                                :2 {:cat :prep
                                    :sem complement-sem}}}}))]

   "alzarsi" (let [subject-semantics (ref {:animate true})
                   subject-agr (ref :top)]
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
   "amare"
    {:synsem {:cat :verb
              :essere false
              :sem {:pred :amare
                    :activity false
                    :discrete false
                    :subj {:human true}}}}
   "amico"
   (unify agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem {:pred :amico
                          :human true
                          :child false}}})
   "ammirare" {:synsem {:cat :verb
                        :sem {:pred :admire}}}
   "andare"
    {:italiano {:italiano "andare"
                :essere true
                :drop-e true
                :futuro-stem "andr"
                :present {:1sing "vado"
                          :2sing "vai"
                          :3sing "va"
                          :1plur "andiamo"
                          :2plur "andate"
                          :3plur "vanno"}}
     :synsem {:cat :verb
              :essere true
              :sem {:subj {:animate true}
                    :pred :andare}}}
    
;   (map (fn [each]
;          (unify
;           each
;           ;; common part of all andare lexemes:
;           {:italiano {:italiano "andare"
;                       :essere true
;                       :drop-e true
;                       :present {:1sing "vado"
;                                 :2sing "vai"
;                                 :3sing "va"
;                                 :1plur "andiamo"
;                                 :2plur "andate"
;                                 :3plur "vanno"}}
;            :synsem {:essere true
;                     :sem {:subj {:animate true}
;                           :activity false ;; because "I was going when (something happened) .." sounds weird.
;                           :pred :andare
;                           :discrete false
;                           :motion false}}}))
;        (list
;          ;; "andare"-intransitive
;          

;           {:synsem {:sem {:location '()}}})))

;          (unify
;           transitive
;           ;; "andare" that takes a prepositional phrase
;           (let [place-sem (ref {:place true
;                                 :pred :a})]
;             {:synsem {:sem {:location place-sem}
;                       :subcat {:2 {:sem place-sem
;                                    :subcat '()
;                                    :cat :prep}}}
;              :note "andare-pp"}))))


   "annunciare"  {:synsem {:cat :verb
                           :sem {:pred :announce}}}

   "appoggiare"  {:synsem {:cat :verb
                           :sem {:pred :support}}}

   "apprendere"  {:synsem {:cat :verb
                           :sem {:pred :imparare}}}

   "approfittare"  {:synsem {:cat :verb
                             :sem {:pred :take-advantage-of}}}
   
   "approvare" {:synsem {:cat :verb
                         :sem {:pred :approve}}}

   "ascoltare"  {:synsem {:cat :verb
                          :sem {:pred :listen-to}}}

   "aspettare"  {:synsem {:cat :verb
                          :sem {:pred :wait-for}}}

   "assicurare" [ {:synsem {:cat :verb
                            :sem {:pred :assure}}}
                  {:synsem {:cat :verb
                            :sem {:pred :insure}}}]

   "aumentare"   {:synsem {:cat :verb
                           :sem {:pred :increase}}}
   "avere"
   (let [avere-common {:synsem {:essere false
                                :cat :verb
                                :reflexive false}
                       :italiano {:italiano "avere"
                                  :futuro-stem "avr"
                                  :drop-e true
                                  :present {:1sing "ho"
                                            :2sing "hai"
                                            :3sing "ha"
                                            :1plur "abbiamo"
                                            :2plur "avete"
                                            :3plur "hanno"}}}]
     [(unify ;; 1. "avere": to possess something buyable
       transitive
       avere-common
       {:note "avere (possess)"
        :synsem {:sem {:pred :avere
                       :activity false
                       :discrete false
                       :subj {:human true}
                       :obj {:buyable true}}}})


      ;; 2. avere: unspecified object
      (unify
       avere-common
       verb-subjective
       intransitive-unspecified-obj
       {:note "avere (possess): unspecified object"
        :synsem {:sem {:pred :avere
                       :activity false
                       :discrete false
                       :subj {:human true}}}})

      ;; 3. "avere" that takes a transitive verb: e.g. "io l'ho vista (i saw her)"
      (let [agr-of-obj-of-main-verb (ref :top)]
        (unify
         verb-aux
         verb-subjective
         avere-common
        {:note "avere(aux): takes trans"
         :synsem {:infl :present
                  :subcat {:2 {:agr agr-of-obj-of-main-verb
                               :reflexive false
                               :subcat {:2 {:agr agr-of-obj-of-main-verb
                                            :pronoun true}}
                               :essere false}}}}))

      ;; 4. "avere" that takes an intransitive verb or a transitive verb within a VP
      ;;    with the object (e.g. "io ho dormito (i slept)" or "io ho [mangiato la pizza] (i ate the pizza)"
      ;; "avere": auxiliary-verb: takes 2 args:
      ;; 1. subject that is the same as the subject of 2.
      ;; 2. an intransitive verb.
      (let [agr-of-subj-of-main-verb (ref :top)]
        (unify
         verb-aux
         verb-subjective
         avere-common
         {:note "avere(aux): takes intrans"
          :synsem {:infl :present
                   :subcat {:1 {:agr agr-of-subj-of-main-verb}
                            :2 {:essere false
                                :agr agr-of-subj-of-main-verb
                                :subcat {:1 {:agr agr-of-subj-of-main-verb}
                                         :2 '()}}}}}))])


   "bello"
   [;; non-comparative
    (unify adjective
           {:synsem {:sem {:pred :bello
                           :comparative false
                           }}}) ;; for now, no restrictions on what can be beautiful.
    
    (let [complement-complement-sem (ref :top) ;; for now no restrictions
          complement-sem (ref {:pred :di
                               :mod complement-complement-sem})
          subject-sem (ref :top)] ;; subject can be anything.
      (unify adjective
              comparative
              {:synsem {:sem {:pred :bello
                              :arg1 subject-sem
                              :arg2 complement-complement-sem}
                        :subcat {:1 {:cat :noun
                                    :sem subject-sem}
                                 :2 {:cat :prep
                                     :sem complement-sem}}}}))]

   "bene"
   {:synsem {:cat :adverb
             :sem {:pred :bene}}}

   "bere"
    {:italiano {:passato "bevuto"
                :futuro-stem "berr"
                :imperfetto {:1sing "bevevo"
                             :2sing "bevevi"
                             :3sing "beveva"
                             :1plur "bevevamo"
                             :2plur "bevevate"
                             :3plur "bevevano"}
                :present {:1sing "bevo"
                          :2sing "bevi"
                          :3sing "beve"
                          :1plur "beviamo"
                          :2plur "bevete"
                          :3plur "bevono"}}
     :synsem {:cat :verb
              :essere false
              :sem {:pred :bere
                    :subj {:animate true}
                    :obj {:drinkable true}}}}

   "bianco"
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :bianco
                          :comparative false
                          :physical-object true
                          :human false}}
           :italiano {:masc {:plur "bianchi"}
                      :fem {:plur "bianche"}
                      :cat :adjective}})

   "birra"
   (unify agreement-noun
           drinkable-noun
           feminine-noun
           {:synsem {:sem {:pred :birra
                           :artifact true}}})


   "braccio"
   (unify agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem {:pred :braccio
                          :part-of-human-body true}}
           ;; adding "bracci" as irregular because
           ;; current morphology.clj would otherwise return
           ;; "braccii".
           ;; TODO: might not be an exception so much
           ;; as a ortho-pholological rule "io" -plur-> "ia"
           :italiano {:plur "bracci"}})

   "brutto"
   ;; non-comparative
   ;; TODO: add comparative
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :brutto
                          :comparative false
                          }} ;; for now, no restrictions on what can be ugly.
           :italiano {:cat :adjective}})

   "bucato"
   {:synsem {:cat :noun
             :agr {:gender :masc
                   :number :sing}
             :sem {:animate false
                   :drinkable false
                   :edible false
                   :legible false
                   :mass false
                   :pred :bucato
                   :speakable false}
             :subcat {:1 {:def :def}}}}

   "calzoni"
   ;; inherently plural
   (unify agreement-noun
           common-noun
           countable-noun
           masculine-noun
           {:synsem {:sem {:pred :calzoni
                           :artifact true
                           :speakable false
                           :legible false
                           :consumable false
                           :clothing true}}})

   "cambiare" {:synsem {:cat :verb
                        :sem {:pred :cambiare}}}
   
   "cancellare" {:synsem {:cat :verb
                          :sem {:pred :cancellare}}}

   "cantare" {:synsem {:cat :verb
                       :sem {:pred :cantare}}}

   "caricare" {:italiano {:futuro-stem "carich"}
               :synsem {:cat :verb
                        :sem {:pred :caricare}}}

   "cenare" {:synsem {:cat :verb
                      :essere false
                      :sem {:subj {:human true}
                            :pred :cenare}}}

   "camicia"
    (unify agreement-noun
           common-noun
           countable-noun
           feminine-noun
           {:synsem {:sem {:pred :camicia
                           :artifact true
                           :speakable false
                           ;; (although an exception would be tshirts with writing on them):
                           :legible false
                           :consumable false
                           :clothing true}}})

    "camminare"
    {:synsem {:sem {:pred :run}
              :cat :verb}}

   "cane"
   (unify agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem {:animal true
                          :human false
                          :pet true
                          :pred :cane}}})


   "capire" {:italiano {:boot-stem1 "capisc"
                        :boot-stem2 "cap"}
             :synsem {:cat :verb
                      :sem {:pred :understand}}}

   "casa"
    (unify agreement-noun
           common-noun
           countable-noun
           feminine-noun
           {:synsem {:sem {:pred :casa
                           :activity false ;; should not need this probably: should be inferrable from  :place==true or perhaps :artifact==true.
                           :buyable true
                           :artifact true
                           :place true}}})

    "cattivo"
     (unify adjective
            {:synsem {:cat :adjective
                      :sem {:pred :cattivo
                            :comparative false
                            :human true;; TODO:should not need this because child => human.
                            :child true}}
             :italiano {:cat :adjective}})

      ;; working on: "mi sono comprato un nuovo cellulare"
     "cellulare"
     (unify agreement-noun
             masculine-noun
             common-noun
             countable-noun
             {:synsem {:cat :noun
                       :sem {:pred :cellulare
                             :artifact true
                             :consumable false
                             :writable false
                             :place false
                             :speakable false}}})

     "chiacchierare" {:synsem {:cat :verb
                               :sem {:pred :chat}}}
      
     "chiedere" {:synsem {:cat :verb
                          :sem {:subj {:human true}
                                :pred :chiedere}}
                 :italiano {:passato "chiesto"}}

     "chiunque"
     {:synsem {:cat :fail ; :noun ;; disabling until more constraints are put on usage of it (TODO).
               :pronoun true
               :case :nom
               :agr {:person :3rd
                     :number :sing}
               :sem {:human true
                      :pred :chiunque
                     :elective-existential true}
               :subcat '()}}

      "ci"
      {:synsem {:cat :noun
                :pronoun true
                :case pronoun-acc
                :agr {:person :1st
                      :number :plur}
                :sem {:human true 
                      :pred :noi}
                :subcat '()}
       :italiano {:initial true
                  :cat :noun
                  :case pronoun-acc}}

      "cipolla"
      (unify agreement-noun
             common-noun
             feminine-noun
             {:synsem {:sem {:pred :cipolla
                             :edible true
                             :animate false
                             :artifact false}}})
      
      "cercare" {:synsem {:cat :verb
                          :essere false
                          :sem {:activity true
                                :discrete false
                                :pred :cercare
                                :subj {:animate true}}}}

      "città"
      (unify agreement-noun
             common-noun
             countable-noun
             feminine-noun
             {:synsem {:sem {:pred :città
                             :buyable false  ;; can't buy a city (unless you're a billionaire like Mike Bloomberg)
                             :artifact true ;;
                             :city true}
                       :subcat {:1 {:cat :det
                                    :def :def}}}})

      "cominciare"
      [{:synsem {:essere false
                          :sem {:activity true
                                :discrete false
                                :pred :begin
                                :subj {:animate true}}}}
       {:synsem {:essere false
                 :sem {:activity true
                       :discrete false
                       :pred :start
                       :subj {:animate true}}}}]
      
      "compito"
      (unify agreement-noun
             common-noun
             countable-noun
             masculine-noun
             {:synsem {:sem {:pred :compito
                             :legible true
                             :speakable false
                             :buyable false
                             :artifact true}}})


      "commentare"  {:synsem {:cat :verb
                              :sem {:pred :comment}}}

      "comprare" {:synsem {:cat :verb
                           :essere false
                           :sem {:pred :comprare
                                 :subj {:human true}
                                 :obj {:buyable true}}}}
      
      "condividere"  {:synsem {:cat :verb
                               :sem {:pred :share}}}

      "conservare" [{:synsem {:cat :verb
                              :sem {:pred :conserve}}}
                    {:synsem {:cat :verb
                              :sem {:pred :preserve}}}]

      
      "considerare"  {:synsem {:cat :verb
                               :sem {:pred :consider}}}

      "contento"
      [(let [complement-complement-sem (ref {:human true})
             complement-sem (ref {:pred :di
                                  :mod complement-complement-sem})
             subject-sem (ref {:place true})]
         (unify adjective
                comparative
                {:synsem {:sem {:pred :contento
                                :arg1 subject-sem
                                :arg2 complement-complement-sem}
                        :subcat {:1 {:cat :noun
                                     :sem subject-sem}
                                 :2 {:cat :prep
                                     :sem complement-sem}}}})

         ;; non-comparative
         (unify adjective
                {:synsem {:cat :adjective
                          :sem {:pred :contento
                                :comparative false
                                :human true}}}))]

      "contraccambiare" {:synsem {:sem {:pred :reciprocate}
                                  :cat :verb}}

      "correre"  {:italiano {:passato "corso"}
                  :synsem {:cat :verb
                           :sem {:pred :run}}}

      "corrispondere"  {:synsem {:cat :verb
                                 :sem {:pred :correspond}}}

      "corto"
      [(let [complement-complement-sem (ref {:human true}) ;; only humans can be short.
             complement-sem (ref {:pred :di
                                  :mod complement-complement-sem})
             subject-sem (ref {:human true})] ;; only humans can be short.
        (unify adjective
               comparative
               {:synsem {:sem {:pred :corto
                               :arg1 subject-sem
                               :arg2 complement-complement-sem}
                         :subcat {:1 {:cat :noun
                                      :sem subject-sem}
                                  :2 {:cat :prep
                                      :sem complement-sem}}}}))

       ;; non-comparative
       (unify adjective
              {:synsem {:cat :adjective
                        :sem {:pred :corto
                              :comparative false
                              :human true}}
               :italiano {:cat :adjective}})]

      "creare"  {:synsem {:cat :verb
                          :sem {:pred :create}}}


      "colpire" {:italiano {:boot-stem1 "colpisc"
                            :boot-stem2 "colp"}
                 :synsem {:sem {:pred :strike}
                          :cat :verb}}

      "controllare" {:synsem {:sem {:pred :check}
                              :cat :verb}}
                                    

      ;; TODO: account for "dare" being ditransitive.
      "dare"  {:synsem {:cat :verb
                        :sem {:pred :dare}}
               :italiano {:present {:2sing "dai"
                                    :3plur "danno"}
                          :futuro-stem "dar"}}

;; TODO: something wrong here with respect to passato: probably the :essere=false is causing a problem.
;        :synsem {:cat :verb
;                 :essere false
;                 :sem {:pred :dare
;                       :subj {:human true}
;                      :iobj {:animate true}
;                       :obj {:buyable true}}}})
      
      "decidere"  {:synsem {:cat :verb
                            :essere false
                            :sem {:pred :decide}}
                   :italiano {:passato "deciso"}}

      "dei"
      {:synsem {:cat :det
                :def :partitivo
                :number :plur
                :gender :masc}}

      "delle"
      {:synsem {:cat :det
                :def :partitivo
                :number :plur
                :gender :fem}}

               
      "deludere"
       {:italiano {:passato "deluso"}
        :synsem {:essere false
                 :sem {:deliberate false
                       :discrete true
                       :activity false
                       :pred :deludere
                       :subj {:human true}
                       :obj {:human true}}}}

      "desiderare"  {:synsem {:cat :verb
                              :sem {:pred :desire}}}
      
      "di"
      {:synsem {:cat :prep
                :sem {:pred :di
                      :comparative true}
                :subcat {:1 {:cat :noun
                             :subcat '()
                             :def {:not :partitivo} ;; avoid alliteration like "di delle ragazze (of some women)"
                             :case :disj ;; pronouns must be disjunctive (me/te/lui/lei...etc)
                           ;; non-pronouns will unify with this constraint.
                             }
                         :2 '()}}
       :italiano {:initial true}}

      "di la"
      {:synsem {:cat :det
                :def :partitivo
                :number :sing
                :mass true
                :gender :fem}}

      "di le"
      {:synsem {:cat :det
                :def :partitivo
                :number :plur
                :gender :fem}}

      "di il"
      {:synsem {:cat :det
                :def :partitivo
                :number :sing
                :mass true
                :gender :masc}}

      "difficile"
      ;; non-comparative
      ;; TODO: add comparative
      (unify adjective
             {:synsem {:cat :adjective
                       :sem {:pred :difficile
                             :comparative false
                             ;; "difficile" has a very specific list of things it can modify:
                             :drinkable false
                             :human false
                             :animate false
                             :buyable false
                             :legible true
;                             :activity true ;; TODO: cannot declare this as an activity because of some semantic implicature..
                             :artifact true
                             :physical-object true
                             :edible false}}
              :italiano {:cat :adjective}})

      "dipingere" {:synsem {:cat :verb
                            :sem {:pred :dipingere}}
                   :italiano {:passato "dipinto"}}

      "divertirsi" (let [subject-semantics (ref {:human true})
                         subject-agr (ref :top)]
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
      "domani"
      (unify sentential-adverb
             {:synsem {:cat :sent-modifier
                       :sem {:pred :domani}
                       :subcat {:1 {:infl :futuro
                                    :sem {:tense :futuro}
                                    :subcat '()}}}
              :italiano "domani"})

      "donna"
      (unify agreement-noun
              common-noun
              countable-noun
              feminine-noun
              {:synsem {:sem {:human true
                              :pred :donna
                              :child false}}})
      
      "dopodomani"
      (unify sentential-adverb
              {:synsem {:cat :sent-modifier
                        :sem {:pred :dopodomani}
                        :subcat {:1 {:infl :futuro
                                     :sem {:tense :future}
                                     :subcat '()}}}})

      "dormire"
      {:synsem {:cat :verb
                :essere false
                :sem {:subj {:animate true}
                      :discrete false
                      :pred :dormire}}}

      "dovere" {:synsem {:cat :verb
                         :sem {:pred :have-to}}
                :italiano {:futuro-stem "dovr"
                           :drop-e true
                           :present {:1sing "devo"
                                     :2sing "devi"
                                     :3sing "deve"
                                     :1plur "dobbiamo"
                                     :2plur "dovete"
                                     :3plur "devono"}}}


      "entrare" {:synsem {:cat :verb
                          :essere true
                          :sem {:pred :enter}}}

      "esistere" {:synsem {:cat :verb
                           :essere true
                           :sem {:pred :exist}}
                  :italiano {:passato "esistito"}}

      "esprimere" {:italiano {:passato "espresso"}
                   :synsem {:cat :verb
                            :sem {:pred :express}}}

      "essere"
      (let [essere-common 
            (let [infl (ref :top)
                  agr (ref :top)]
              {:synsem {:agr agr
                        :cat :verb
                        :essere true
                        :infl infl
                        :subcat {:1 {:agr agr}}}
               :italiano {:agr agr
                          :futuro-stem "sar"
                          :infinitive "essere"
                          :infl infl
                          :present {:1sing "sono"
                                    :2sing "sei"
                                    :3sing "è"
                                    :1plur "siamo"
                                    :2plur "siete"
                                    :3plur "sono"}
                          :passato "stato"
                          :imperfetto {:1sing "ero"
                                       :2sing "eri"
                                       :3sing "era"
                                       :1plur "eravamo"
                                       :2plur "eravate"
                                       :3plur "erano"}
                          :futuro {:1sing "sarò"
                                   :2sing "sarai"
                                   :3sing "sarà"
                                   :1plur "saremo"
                                   :2plur "sarete"
                                   :3plur "saranno"}}})]
        [;; essere: adjective
         ;; TODO: unify essere-adjective and essere-intensifier into one lexical entry.
         (let [gender (ref :top)
               number (ref :top)]
           (unify
            essere-common
            {:notes "essere-adjective"
             :synsem {:cat :verb
                      :sem {:pred :essere
                            :subj :top
                            :obj :top}
                      :subcat {:1 {:cat :noun
                                 :agr {:gender gender
                                       :number number}}
                               :2 {:cat :adjective
                                   :sem {:comparative false}
                                   :subcat {:1 :top
                                            :2 '()}
                                   :agr {:gender gender
                                         :number number}}}}}))

           (unify essere-common {:synsem {:sem {:pred :essere}}})

           ;; essere: copula ;; note that we don't enforce agreement the same here as we do in essere-adjective: TODO: try to make more consistent.
           (let [gender (ref :top)
                 number (ref :top)
                 human (ref :top)]
             (unify
              transitive
              essere-common
              {:notes "copula" ;; significant only for debugging.
               :synsem {:cat :verb
                        :subcat {:1 {:cat :noun
                                     :agr {:gender gender
                                           :number number}}
                                 :2 {:cat :noun
                                     :pronoun {:not true} ;; accusative pronouns cause unbounded depth-first searches on the subject side. (TODO: not sure if this problem is still present)
                                     :def {:not :demonstrativo}
                                     :agr {:gender gender
                                           :number number}}}
                        :sem {:pred :essere
                              :activity false
                              :discrete false
                              :subj {:human human}
                              :obj {:human human}}}}))

           ;; essere: intensifier
           ;; this is for e.g "essere più alto di quelle donne belle (to be taller than those beautiful women)"
           (let [gender (ref :top)
                 number (ref :top)
                 subject (ref {:agr {:gender gender
                                     :number number}
                               :cat :noun})
                 comp-pred (ref :top)
                 comp-sem (ref
                           {:activity false
                            :pred comp-pred
                            :discrete false})]
             (unify
              verb-subjective
              essere-common
              {:notes "essere-intensifer"
               :synsem {:cat :verb
                        :subcat {:1 subject
                                 :2 {:cat :intensifier
                                     :sem comp-sem
                                     :subcat {:1 subject
                                              :2 '()}}}
                        :sem {:pred comp-pred
                              :intensified true
                              :obj comp-sem}}}))

           (unify essere-common
                   verb-aux
                   verb-subjective
                   {:italiano {:notes "essere-aux"}})])
      
      "evitare"  {:synsem {:cat :verb
                           :sem {:pred :avoid}}}
      
      "finire"  {:italiano {:boot-stem1 "finisc"
                            :boot-stem2 "fin"}
                 :synsem {:cat :verb
                          :sem {:pred :finish}}}
      
      "formare"  {:synsem {:cat :verb
                           :sem {:pred :form}}}

      "fornire"  {:synsem {:cat :verb
                           :sem {:pred :furnish}}}
      
      "frequentare"  {:synsem {:cat :verb
                               :sem {:pred :frequentare}}}

      "funzionare" {:synsem {:cat :verb
                             :essere false
                             :sem {:pred :work-nonhuman
                                   :subj {:human false}}}}

      "gatto"
      (unify agreement-noun
             common-noun
             countable-noun
             masculine-noun
             {:synsem {:sem {:animal true
                             :human false
                             :pred :gatto
                             :pet true}}})

      "gestire" {:italiano {:boot-stem1 "gestisc"
                            :boot-stem2 "gest"}
                 :synsem {:sem {:pred :manage}
                          :cat :verb}}

      "giocare"  {:synsem {:cat :verb
                           :sem {;:obj {:games true}
                                 :subj {:human true}
                                 :pred :giocare}}
                  :italiano {:futuro-stem "giocher"}}

      "gridare" {:synsem {:cat :verb
                          :sem {:subj {:human true}
                                :pred :yell}}}

      "guadagnare"  {:synsem {:cat :verb
                              :sem {:pred :earn
                                    :subj {:human true}
                                    :obj {:human false}}}}

      "guidare"  {:synsem {:cat :verb
                           :sem {:pred :guidare}}}
      
      "i"
      (unify determiner
             {:synsem {:cat :det
                       :def :def
                       :gender :masc
                       :number :plur}})


      "il"
      (unify determiner
              {:synsem {:cat :det
                        :def :def
                        :gender :masc
                        :number :sing}})

      "imparare"  {:synsem {:cat :verb
                            :sem {:pred :imparare}}}

      "impegnare" {:synsem {:cat :verb
                            :sem {:pred :engage}}}

      "incontrare"  {:synsem {:cat :verb
                              :sem {:pred :incontrare}}}

      "indossare"  {:synsem {:cat :verb
                             :sem {:pred :wear}}}

      "insegnare"  {:synsem {:cat :verb
                             :sem {:pred :teach}}}

      "interrompere" {:italiano {:passato "interrotto"}
                      :synsem {:cat :verb
                               :sem {:pred :interrupt}}}
      
;; commenting out until discussed with Franco
;;      "" ;; empty pronominal subject e.g. "mangio" = "io mangio".
;;      [{:synsem {:agr {:person :1st
;;                       :number :sing}
;;                 :cat :noun
;;                 :case :nom
;;                 :pronoun true
;;                 :sem {:human true}
;;                 :subcat '()}}]

      "io"
      [{:synsem {:cat :noun
                 :pronoun true
                 :case :nom
                 :agr {:gender :fem
                       :person :1st
                       :number :sing}
                :sem {:human true
                      :pred :io}
                :subcat '()}}

       {:synsem {:cat :noun
                 :pronoun true
                 :case :nom
                 :agr {:gender :masc
                       :person :1st
                       :number :sing}
                 :sem {:human true
                       :pred :io}
                 :subcat '()}}]
      "la"
      ;; TODO: refactor commonalities
      ;; 1. pronoun: human
      [{:synsem {:cat :noun
                 :pronoun true
                 :case pronoun-acc
                 :reflexive false
                 :agr {:gender :fem
                       :person :3rd
                       :number :sing}
                 :sem {:human true
                       :pred :lei}
                 :subcat '()}
        :italiano {:initial true   ;; TODO: verify that 'la' above and this below are being unified correctly.
                   :cat :noun
                   :case pronoun-acc}}

       ;; 2. pronoun: non-human
       {:synsem {:cat :noun
                 :pronoun true
                 :case pronoun-acc
                 :reflexive false
                 :agr {:gender :fem
                       :person :3rd
                       :number :sing}
                 :sem {:human false
                       :place false ;; "they go to it (loro vanna a la)" sounds strange
                       :pred :lei}
                 :subcat '()}
        :italiano {:initial true
                   :cat :noun
                   :case pronoun-acc}}

       ;; 3. article
       {:synsem {:cat :det
                 :def :def
                 :gender :fem
                 :number :sing}}]
      
      "la loro"
      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :plur
                      :person :3rd}}}

      "la mia"
      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :sing
                      :person :1st}}}

      "la nostra"
      {:synsem {:cat :det
                :def :possessive
                :gender :fem
                :number :sing
                :sem {:number :plur
                      :person :1st}}}

   ;; TODO for below: add pronominal "la sua" (translated in English as "his" and "hers", depending on gender of the owner of the referent).
   ;; e.g. "I gatti sono i suoi. (The cats are hers) (if talking about a female owner) or (The cats are his) (if talking about a male owner).
   "la sua"
   {:synsem {:cat :det
             :def :possessive
             :gender :fem
             :number :sing
             :sem {:number :sing
                   :person :3rd}}}
   "la tua"
   [{:synsem {:cat :det
              :def :possessive
              :gender :fem
              :number :sing
              :sem {:number :sing
                    :person :2nd}}}

    {:synsem {:cat :det
              :def :possessive
              :gender :fem
              :number :sing
                :sem {:number :plur
                      :person :2nd}}}]

   "lavarsi" (let [subject-semantics (ref :top)
                   subject-agr (ref :top)]
              {:synsem {:cat :verb
                        :essere true
                        :sem {:pred :wash
                              :reflexive true
                              :subj subject-semantics
                              :obj subject-semantics}
                        :subcat {:1 {:agr subject-agr
                                     :sem subject-semantics}
                                 :2 {:agr subject-agr
                                     :pronoun true
                                     :reflexive true
                                     :sem subject-semantics}}}})

   "lavorare"  {:synsem {:cat :verb
                         :sem {:subj {:human true}
                               :pred :work-human}}}
   "le"
   {:synsem {:cat :det
             :def :def
             :gender :fem
             :number :plur}}
   "lei"
   (let [common {:synsem {:cat :noun
                          :pronoun true
                          :case :nom
                          :agr {:person :3rd
                                :gender :fem
                                :number :sing}
                          :sem {:pred :lei} ;; note: we don't specify human=true (english "it").
                          :subcat '()}}]
     [(unify common
             {:synsem {:sem {:human false}}})
      (unify common
             {:synsem {:sem {:human true}}})])
   "leggere"
   {:italiano {:passato "letto"}
    :synsem {:cat :verb
             :essere false
             :sem {:discrete false
                   :pred :leggere
                   :subj {:human true}
                   :obj {:legible true}}}}
   "libro"
   (unify agreement-noun
          common-noun
          countable-noun
          masculine-noun
          {:synsem {:sem {:pred :libro
                          :legible true
                          :speakable false
                          :mass false
                          :buyable true
                          :consumable false
                          :artifact true}}})

   "lo"
   [{:synsem {:cat :noun
              :pronoun true
              :case pronoun-acc
              :agr {:gender :masc
                    :person :3rd
                    :number :sing}
              :sem {:human true
                    :pred :lo}
              :subcat '()}}

    {:synsem {:cat :noun
              :pronoun true
              :case pronoun-acc
              :agr {:gender :masc
                    :person :3rd
                    :number :sing}
              :sem {:human false
                    :place false
                    :pred :lo}
              :subcat '()}
       :italiano {:initial true  ;; TODO: verify that 'lo' above and this below are being unified correctly.
                  :pronoun true
                  :cat :noun
                  :case pronoun-acc}}]

   "loro"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :fem
                    :number :plur}
              :sem {:human true
                    :pred :loro}
              :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :masc
                    :number :plur}
              :sem {:human true
                    :pred :loro}
              :subcat '()}}]

   "lui" (let [common {:synsem {:cat :noun
                                :pronoun true
                                :case :nom
                                :agr {:person :3rd
                                      :gender :masc
                                      :number :sing}
                                :sem {:pred :lui} ;; note: we don't specify human=true (english "it").
                                :subcat '()}}]
           [(unify common
                   {:synsem {:sem {:human false}}})
            (unify common
                   {:synsem {:sem {:human true}}})])

   "macchiare" {:synsem {:cat :verb
                         :sem {:pred :stain}}}
   "madre"
   (unify agreement-noun
          common-noun
          countable-noun
          feminine-noun
          {:synsem {:sem {:human true
                          :pred :madre
                          :child false}}})

   "mancare"  {:italiano {:futuro-stem "mancher"}
               :synsem {:cat :verb
                        :sem {:pred :mancare}}}
   
   "mandare"  {:synsem {:cat :verb
                        :sem {:pred :send}}}

   "mangiare"
    {:synsem {:cat :verb
              :essere false
              :sem {:pred :mangiare
                    :subj {:animate true}
                    :obj {:edible true}}}}

    "mentire"
    {:synsem {:cat :verb
              :sem {:pred :lie}}}

    "mi"
    [{:synsem {:cat :noun
               :pronoun true
               :case :acc
               :agr {:gender :fem
                     :person :1st
                     :number :sing}
               :sem {:human true
                     :pred :io}
               :subcat '()}}

     {:synsem {:cat :noun
               :pronoun true
               :case :acc
               :agr {:gender :masc
                     :person :1st
                     :number :sing}
               :sem {:human true
                     :pred :io}
               :subcat '()}}]

    "migliorare" {:synsem {:cat :verb
                           :sem {:pred :improve}}} ;; c.f. english: "ameliorate"
    
    "misurare" {:synsem {:cat :verb
                         :sem {:pred :measure}}}

    "mostrare" {:synsem {:cat :verb
                         :sem {:pred :show}}}
    
    "moltiplicare" {:synsem {:cat :verb
                             :sem {:pred :multiply}}}
    
    "muovere" {:synsem {:cat :verb
                        :sem {:pred :move}}}

   ;; non-comparative
   ;; TODO: add comparative
   "nero"
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :nero
                          :comparative false
                          :physical-object true
                          :human false}}})

   "noi"
   [{:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:gender :fem
                   :person :1st
                   :number :plur}
             :sem {:human true
                   :pred :noi}
             :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
             :agr {:gender :masc
                   :person :1st
                   :number :plur}
              :sem {:human true
                    :pred :noi}
              :subcat '()}}]

   "osservare" {:synsem {:cat :verb
                         :sem {:pred :observe}}}

   "ottenere" {:synsem {:cat :verb
                        :sem {:pred :obtain}}
              :italiano {:passato "ottenuto"
                         :present {:1sing "ottengo"
                                   :2sing "ottieni"
                                   :3sing "ottiene"
                                   :3plur "otttengono"}
                         :futuro-stem "otterr"}}

   "pane"
   ;; inherently singular.
   (unify agreement-noun
          common-noun
          masculine-noun
          {:synsem {:sem {:pred :pane
                          :edible true
                          :artifact true}
                    :subcat {:1 {:cat :det
                                 :number :sing
                                 :def :def}}}})

   "parlare"
   (let [common1
         {:synsem {:essere false
                   :sem {:pred :speak
                         :subj {:human true}}}}
         common2
         {:synsem {:essere false
                   :sem {:pred :talk
                         :subj {:human true}}}}]

     (list
      (unify common1 transitive
             {:synsem {:sem {:obj {:speakable true}}}})
      (unify common1 intransitive intransitive-unspecified-obj)

      (unify common2 intransitive intransitive-unspecified-obj)))

   "piangere" {:italiano {:passato "pianto"}
            :synsem {:cat :verb
                     :sem {:pred :cry}}}

   "piegare" {:synsem {:cat :verb
                      :sem {:pred :fold}}}

   "portare"  [{:synsem {:cat :verb
                         :sem {:pred :carry}}}
               {:synsem {:cat :verb
                         :sem {:pred :wear}}}]

   "prendere" {:synsem {:cat :verb :sem {:pred :take}}
               :italiano {:passato "preso"}}

   "prepararsi" (let [subject-semantics (ref {:human true})
                      subject-agr (ref :top)]
                  {:synsem {:cat :verb
                            :essere true
                            :sem {:pred :get-ready
                                  :reflexive true
                                  :subj subject-semantics
                                  :obj subject-semantics}
                            :subcat {:1 {:agr subject-agr
                                         :sem subject-semantics}
                                     :2 {:agr subject-agr
                                         :pronoun true
                                         :reflexive true
                                         :sem subject-semantics}}}})
   "prenotare" {:synsem {:cat :verb
                         :sem {:pred :reserve}}}
   "qualche"
   {:synsem {:cat :det
             :def :indef
             :mass false
             :number :sing}}

   "racontare" {:synsem {:cat :verb
                         :sem {:pred :recount}}}

   "recuperare" {:synsem {:cat :verb
                           :sem {:pred :recover}}}

   "restituire" {:italiano {:boot-stem1 "restituisc"
                            :boot-stem2 "restitu"}
                 :synsem {:cat :verb
                          :sem {:pred :giveback-return}}}

   "riconoscere" {:synsem {:cat :verb
                          :sem {:pred :recognize}}}

   "ricevere"  {:synsem {:cat :verb
                         :sem {:pred :ricevere}}}
   "ricordare"  {:synsem {:cat :verb
                          :sem {:pred :ricordare}}}

"rimanere" {:synsem {:cat :verb
                      :essere true
                      :sem {:pred :remain}}
             :italiano {:passato "rimasto"
                        :present {:1sing "rimango"
                                  :3plur "rimangono"}
                        :futuro-stem "rimarr"}}

   "riposare" {:synsem {:cat :verb
                        :sem {:pred :rest}}}

   "riscaldare" {:synsem {:cat :verb
                          :sem {:pred :warm}}}

   "rispondere" {:synsem {:cat :verb
                          :essere false
                          :sem {:pred :answer}}
                 :italiano {:passato "risposto"}}
      
   "ritornare" {:synsem {:cat :verb
                         :sem {:pred :ritornare}
                         :essere true}}

   "rompere" {:synsem {:cat :verb
                       :sem {:pred :break}}
              :italiano {:passato "rotto"}}

   "rubare" {:synsem {:cat :verb
                      :sem {:pred :steal}}}
      
   ;; non-comparative
   ;; TODO: add comparative
   "rosso"
   (unify adjective
          {:synsem {:cat :adjective
                    :sem {:pred :rosso
                          :comparative false
                          :physical-object true
                          :human false}}})

   "scappare"  {:synsem {:cat :verb 
                         :sem {:pred :escape}}}

   "scattare" {:synsem {:cat :verb
                        :sem {:pred :snap-pictures}}}

   "scaricare" {:synsem {:cat :verb 
                      :sem {:pred :scaricare}}}
   
   "scrivere"  {:synsem {:cat :verb 
                         :sem {:pred :scrivere}}
                :italiano {:passato "scritto"}}

   "sedersi" (let [subject-semantics (ref {:animate true})
                   subject-agr (ref :top)]
               {:synsem {:cat :verb
                         :essere true
                         :sem {:pred :sit-down}
                         :subcat {:1 {:agr subject-agr
                                      :sem subject-semantics}
                                  :2 {:agr subject-agr
                                      :pronoun true
                                      :reflexive true
                                      :sem subject-semantics}}}
                :italiano {:present {:1sing "siedo"
                                     :2sing "siedi"
                                     :3sing "siede"
                                     :1plur "sediamo"
                                     :2plur "sedete"
                                     :3plur "siedono"}}})
   "sgridare" {:synsem {:cat :verb
                        :sem {:subj {:human true}
                              :pred :scold}}}
   "si"
   [;; feminine singular
    (let [cat (ref :noun)]
      {:synsem {:cat cat
                :pronoun true
                :reflexive true
                :case pronoun-acc
                :agr {:person :3rd
                      :gender :fem
                      :number :sing}
                :sem {:pred :lei}
                :subcat '()}
       :italiano {:cat cat
                  :case pronoun-acc}})

    ;; masculine singular
    (let [cat (ref :noun)]
      {:synsem {:cat cat
                :pronoun true
                :reflexive true
                :case pronoun-acc
                :agr {:person :3rd
                      :gender :masc
                      :number :sing}
                :sem {:pred :lui}
                :subcat '()}
       :italiano {:cat cat
                  :case pronoun-acc}})

    ;; plural: unspecified gender
    (let [cat (ref :noun)]
      {:synsem {:cat cat
                :pronoun true
                :reflexive true
                :case pronoun-acc
                :agr {:person :3rd
                      :number :plur}
                :sem {:pred :loro}
                :subcat '()}
       :italiano {:cat cat
                  :case pronoun-acc}})]

   "sistemare" {:synsem {:cat :verb
                         :sem {:pred :organize}}}

   "sopportare" {:synsem {:cat :verb
                          :sem {:pred :endure}}}

   "sospirare" {:synsem {:cat :verb
                         :sem {:pred :sigh}}}

   "spostare" {:synsem {:cat :verb
                        :sem {:pred :displace}}}

   "sprecare" {:synsem {:cat :verb
                        :sem {:pred :waste}}}

   "stampare"  {:synsem {:cat :verb 
                         :sem {:pred :stampare}}}

   "studiare"  {:synsem {:cat :verb 
                      :sem {:pred :study}}}

   "suonare"  {:synsem {:cat :verb 
                      :sem {:subj {:human true}
                            :pred :suonare}}}
;                                            :obj {:music true}}}})

 "svenire" {:synsem {:cat :verb
                      :essere true
                      :sem {:pred :faint}}
             :italiano {:passato "svenuto"
                        :present {:1sing "svengo"
                                  :2sing "svieni"
                                  :3sing "sviene"
                                  :3plur "svengono"}
                        :futuro-stem "sverr"}}

   "svegliarsi" (let [subject-semantics (ref {:animate true})
                      subject-agr (ref :top)]
                  {:synsem {:cat :verb
                            :essere true
                            :sem {:pred :wake-up
                                  :reflexive true
                                  :subj subject-semantics
                                  :obj subject-semantics}
                            :subcat {:1 {:agr subject-agr
                                         :sem subject-semantics}
                                     :2 {:agr subject-agr
                                         :pronoun true
                                         :reflexive true
                                         :sem subject-semantics}}}})
   "sviluppare"  {:synsem {:cat :verb 
                      :sem {:pred :develop}}}

   "tagliare"  {:synsem {:cat :verb
                         :sem {:pred :cut}}}

   "telefonare"  {:synsem {:cat :verb 
                      :essere false
                           :sem {:pred :telefonare}}}

   "tenere"  {:synsem {:cat :verb 
                      :sem {:pred :tenere}}
              :italiano {:passato "tenuto"
                         :present {:1sing "tengo"
                                   :2sing "tieni"
                                   :3sing "tiene"
                                   :3plur "tengono"}
                         :futuro-stem "terr"}}
   "ti"
   {:synsem {:cat :noun
             :pronoun true
             :case pronoun-acc
             :agr {:person :2nd
                   :number :sing}
             :sem {:human true
                   :pred :tu}
             :subcat '()}
    :italiano {:initial true
               :cat :noun
               :case pronoun-acc}}
   "tirare" 
             {:synsem {:cat :verb 
                      :sem {:pred :throw}}}

   "tornare" 
   {:synsem {:cat :verb 
             :sem {:pred :tornare}
             :essere true}}
   
   "trasferire"
   {:synsem {:cat :verb
             :sem {:pred :transfer}}
    :italiano {:boot-stem1 "trasferisc"
               :boot-stem2 "trasfer"}}


   "tu"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :2nd
                    :gender :fem
                    :number :sing}
              :sem {:human true
                    :pred :tu}
              :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :2nd
                    :gender :masc
                    :number :sing}
              :sem {:human true
                    :pred :tu}
              :subcat '()}}]

   "un"
   [{:synsem {:cat :det
              :def :indef
              :mass false
              :gender :masc
              :number :sing}}]
   "una"
   [{:synsem {:cat :det
              :def :indef
              :mass false
              :gender :fem
              :number :sing}}]

   "usare"  {:synsem {:cat :verb
                      :sem {:pred :usare}}}

   "vedere" 
   {:synsem {:cat :verb
             :sem {:pred :vedere}}
    :italiano {:passato "visto"
               :futuro-stem "vedr"}}
   
   "vendere"  {:synsem {:cat :verb 
                        :sem {:pred :vendere
                              :subj {:human true}
                              :obj {:human false}}}}
   
   "venire" {:synsem {:cat :verb
                      :essere true
                      :sem {:pred :come}}
             :italiano {:passato "venuto"
                        :present {:1sing "vengo"
                                  :2sing "vieni"
                                  :3sing "viene"
                                  :3plur "vengono"}
                        :futuro-stem "verr"}}

   "vestirsi" (let [subject-semantics (ref {:human true})
                    subject-agr (ref :top)]
                {:synsem {:cat :verb
                          :essere true
                          :sem {:pred :get-dressed
                                :reflexive true
                                :subj subject-semantics
                                :obj subject-semantics}
                          :subcat {:1 {:agr subject-agr
                                       :sem subject-semantics}
                                   :2 {:agr subject-agr
                                      :pronoun true
                                       :reflexive true
                                       :sem subject-semantics}}}})


   "vi"
   {:synsem {:cat :noun
             :pronoun true
             :case pronoun-acc
             :agr {:person :2nd
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}
    :italiano {:initial true
               :cat :noun
               :case pronoun-acc}}
   
   "vincere"  {:synsem {:cat :verb
                        :sem {:pred :win
                              :subj {:human true}
                              :obj {:human false}}}}

   "vino"
   (unify drinkable-noun
          agreement-noun
          masculine-noun
          {:synsem {:sem {:pred :vino
                          :artifact true}}})

   "voi"
   [{:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}

    {:synsem {:cat cat-of-pronoun
              :pronoun true
              :case :nom
              :agr {:person :2nd
                    :gender :masc
                    :number :plur}
             :sem {:human true
                   :pred :voi}
              :subcat '()}}]})

