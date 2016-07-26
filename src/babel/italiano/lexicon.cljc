;; TODO: nouns do not need {:essere false}
(ns babel.italiano.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.encyclopedia :refer [sem-impl]]
   [babel.lexicon :refer [universals]]

   ;; TODO: use dag_unify/unifyc instead:
   ;; deprecate lexiconfn/unify.
   [babel.lexiconfn :refer [compile-lex if-then constrain-vals-if
                            filter-vals
                            map-function-on-map-vals
                            rewrite-keys unify]]

   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [babel.italiano.morphology :refer [exception-generator italian-specific-rules phonize]]
   [babel.italiano.pos :as it-pos]
   [dag_unify.core :refer [fail? get-in strip-refs]]))

(def analyze-lexemes false)

(def lexicon-source
  (let [adjective it-pos/adjective
        agreement-noun it-pos/agreement-noun 
        cat-of-pronoun it-pos/cat-of-pronoun
        common-noun it-pos/common-noun
        comparative it-pos/comparative
        countable-noun it-pos/countable-noun
        determiner it-pos/determiner
        intransitive it-pos/intransitive
        intransitive-unspecified-obj it-pos/intransitive-unspecified-obj
        masculine-noun it-pos/masculine-noun
        non-comparative-adjective it-pos/non-comparative-adjective
        preposition (let [shared (atom :top)]
                      {:synsem {:sem {:obj shared}
                                :subcat {:1 {:sem shared}}}})
        pronoun-acc it-pos/pronoun-acc
        pronoun-reflexive it-pos/pronoun-reflexive
        reflexive (let [subject-semantics (atom {:animate true})
                        subject-agr (atom :top)]
                    {:synsem {:cat :verb
                              :essere true
                              :sem {:subj subject-semantics
                                    :obj subject-semantics
                                    :reflexive true}
                              :subcat {:1 {:agr subject-agr
                                           :sem subject-semantics}
                                       :2 {:agr subject-agr
                                           :pronoun true
                                           :reflexive true
                                           :sem subject-semantics}}}})

        sentential-adverb it-pos/sentential-adverb
        transitive it-pos/transitive
        verb-aux it-pos/verb-aux
        verb-subjective it-pos/verb-subjective]
    
    {"Luisa"
   {:synsem {:sem {:pred :luisa
                   :human true}
             :agr {:number :sing
                   :person :3rd
                   :gender :fem}
             :propernoun true}}
   "Gianluca"
   {:synsem {:agr {:number :sing
                   :person :3rd
                   :gender :masc}
             :sem {:pred :gianluca
                   :human true}
             :propernoun true}}
   "a"
   ;; "a" as in "a casa"
   [
    {:unify [preposition]
     :synsem {:cat :prep
              :sem {:pred :a
                    :obj {:place true}
                    :comparative false}
              :subcat {:1 {:cat :noun
                           :case :acc
                           :subcat '()}
                       :2 '()}}}
     ;; "a" as in "a Roma"
    {:unify [preposition]
     :synsem {:cat :prep
              :sem {:pred :in
                    :obj {:city true}}
              :subcat {:1 {:cat :noun}
                       :2 '()}}}

    ;; "a" as in "a mezzogiorno"
    {:unify [preposition]
     :synsem {:cat :prep
              :sem {:pred :a
                    :obj {:time true}}
              :subcat {:1 {:cat :noun
                           :subcat '()}
                       :2 '()}}}
    
    ;; e.g. "a ridere": tu hai fatto bene a ridere (you did well to laugh)"
;    (let [complement-semantics (atom {:pred :a
;                                      :mod {:pred :a}})]
;      {:synsem {:cat :prep
;                :sem complement-semantics
;                :subcat {:1 {:cat :verb
;                             :sem complement-semantics
;                                :subcat {:1 :top
;                                         :2 '()}}
;                         :2 '()}}})
    ]
   
   "abbandonare" {:synsem {:cat :verb
                           :sem {:pred :abandon}}}

   "abbassare" {:synsem {:cat :verb
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
   {:synsem {:agr {:gender :fem}
             :cat :noun
             :sem {:artifact false
                   :animate false
                   :pred :acqua}}}

     "addormentarsi"
     {:unify [reflexive]
      :synsem {:sem {:pred :fall-asleep
                     :subj {:animate true}}}}

     "aereo" {:synsem {:agr {:gender :masc}
                     :cat :noun
                     :sem {:physical-object true
                           :pred :airplane
                           :place true}}}
   "affolato"
   [;; comparative
    (let [is-place {:synsem {:sem {:place true}}} ;; only places can be crowded.
          than-this {:pred :di
                     :mod is-place}]
      {:unify [adjective comparative is-place]
       :synsem {:sem {:pred :affolato
                      :arg1 is-place
                      :arg2 is-place}
                :subcat {:1 {:cat :noun
                             :sem is-place}
                         :2 {:cat :prep
                             :sem than-this}}}}
      ;; non-comparative
      {:unify [adjective is-place]
       :synsem {:cat :adjective
                :sem {:pred :affolato
                      :comparative false}}})]

   "aggiungere" {:synsem {:cat :verb
                          :sem {:pred :add}}}
   "aiutare"
   {:synsem {:essere false
             :cat :verb
             :sem {:pred :aiutare
                   :activity true
                   :obj {:human true}}}}

   "alla prossima"
   {:synsem {:cat :exclamation
             :sem {:pred :bye}}}

   "alto"
   [;; non-comparative:
    (let [subject-sem (atom {:human true}) ;; only humans can be tall.
          subject-agr (atom :top)] 
      {:unify [adjective non-comparative-adjective]
       :synsem {:cat :adjective
                :sem {:pred :alto
                      :comparative false
                      :arg1 subject-sem
                      :human true}
                :subcat {:1 {:cat :det
                             :agr subject-agr
                             :sem subject-sem}
                         :2 '()}}})
    ;; comparative:
    (let [complement-complement-sem (atom {:human true}) ;; only humans can be tall.
          complement-sem (atom {:pred :di
                               :mod complement-complement-sem})
          subject-sem (atom {:human true})] ;; only humans can be tall.
      {:unify [adjective comparative]
       :synsem {:sem {:pred :alto
                      :arg1 subject-sem
                      :arg2 complement-complement-sem}
                :subcat {:1 {:cat :noun
                             :sem subject-sem}
                         :2 {:cat :prep
                             :sem complement-sem}}}})]

     "alzarsi" {:unify [reflexive]
                :synsem {:sem {:pred :get-up
                               :subj {:animate true}}}}
   "amare"
    {:synsem {:cat :verb
              :essere false
              :sem {:pred :amare
                    :activity false
                    :discrete false
                    :subj {:human true}}}}
   "amica"
   {:synsem {:agr {:gender :fem}
             :cat :noun
             :sem {:pred :amico
                   :human true
                   :child false}}}
   "amico"
   {:synsem {:agr {:gender :masc}
             :cat :noun
             :sem {:pred :amico
                   :human true
                   :child false}}}
   
   "ammirare" {:synsem {:cat :verb
                        :sem {:pred :admire}}}
   "andare"
    {:italiano {:italiano "andare"
                :essere true
                :drop-e true
                :future-stem "andr"
                :present {:1sing "vado"
                          :2sing "vai"
                          :3sing "va"
                          :1plur "andiamo"
                          :2plur "andate"
                          :3plur "vanno"}}
     :synsem {:cat :verb
              :essere true
              :sem {:subj {:animate true}
                    :pred :go}}}
    
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
;           (let [place-sem (atom {:place true
;                                 :pred :a})]
;             {:synsem {:sem {:location place-sem}
;                       :subcat {:2 {:sem place-sem
;                                    :subcat '()
;                                    :cat :prep}}}
;              :note "andare-pp"}))))

   "anno" {:synsem {:agr {:gender :masc}
                    :cat :noun
                    :sem {:pred :year
                          :time true}}}

   "annunciare"  {:synsem {:cat :verb
                           :sem {:pred :announce}}}

   "appoggiare"  {:synsem {:cat :verb
                           :sem {:pred :support}}}

   "apprendere"  {:synsem {:cat :verb
                           :sem {:pred :learn}}}

   "approfittare"  {:synsem {:cat :verb
                             :sem {:pred :take-advantage-of}}}
   
   "approvare" {:synsem {:cat :verb
                         :sem {:pred :approve}}}

     "arrabbiarsi" {:unify [reflexive]
                    :synsem {:sem {:pred :get-angry
                                   :subj {:animate true}}}}
   
   "arrivare" {:synsem {:essere true 
                        :cat :verb
                        :sem {:pred :arrive}}}
   
   "ascoltare"  {:synsem {:cat :verb
                          :sem {:pred :listen-to}}}

   "aspettare"  {:synsem {:cat :verb
                          :sem {:pred :wait-for}}}

   "assicurare" [ {:synsem {:cat :verb
                            :sem {:pred :assure}}}
                  {:synsem {:cat :verb
                            :sem {:pred :insure}}}]

   "attoro" {:synsem {:agr {:gender :masc}
                      :cat :noun
                      :sem {:pred :actor}}}

   "aula" {:synsem {:agr {:gender :fem}
                    :cat :noun
                    :sem {:pred :classroom}}}
   
   "aumentare"   {:synsem {:cat :verb
                           :sem {:pred :increase}}}
   "avere"
   (let [avere-common {:synsem {:essere false
                                :cat :verb
                                :sem {:reflexive false}}
                       :italiano {:italiano "avere"
                                  :future-stem "avr"
                                  :drop-e true
                                  :present {:1sing "ho"
                                            :2sing "hai"
                                            :3sing "ha"
                                            :1plur "abbiamo"
                                            :2plur "avete"
                                            :3plur "hanno"}}}]

     ;; 1. "avere": to possess something buyable
     [{:unify [
               ;; TODO: transitive should have: {:synsem {:subcat {:3 '()}}}
               transitive       
               {:synsem {:subcat {:3 '()}}}
               avere-common]
       :note "avere (possess)"
       :synsem {:sem {:pred :have
                      :activity false
                      :discrete false
                      :subj {:human true}
                      :obj {:buyable true}}}}

      ;; 2. "avere": to be in a relation with: e.g. "I have two children"
      {:unify [
               ;; TODO: transitive should have: {:synsem {:subcat {:3 '()}}}
               transitive
               {:synsem {:subcat {:3 '()}}}
               avere-common]
       :note "avere (relation)"
       :synsem {:sem {:pred :have
                      :activity false
                      :discrete false
                      :subj {:human :top}
                      :obj {:human :top}}}}

      ;; 3. avere: unspecified object
      {:unify [avere-common verb-subjective intransitive-unspecified-obj]
       :note "avere (possess): unspecified object"
       :synsem {:sem {:pred :have
                      :activity false
                      :discrete false
                      :subj {:human true}}}}

      ;; 4. "avere" that takes a transitive verb: e.g. "io l'ho vista (i saw her)"
      (let [agr-of-obj-of-main-verb (atom :top)]
        {:unify [verb-aux verb-subjective avere-common]
         :note "avere(aux): takes trans"
         :synsem {:infl :present
                  :subcat {:2 {:agr agr-of-obj-of-main-verb
                               :reflexive false
                               :subcat {:2 {:agr agr-of-obj-of-main-verb
                                            :pronoun true}}
                               :essere false}}}})

      ;; 5. "avere" that takes an intransitive verb or a transitive verb within a VP
      ;;    with the object (e.g. "io ho dormito (i slept)" or "io ho [mangiato la pizza] (i ate the pizza)"
      ;; "avere": auxiliary-verb: takes 2 args:
      ;; 1. subject that is the same as the subject of 2.
      ;; 2. an intransitive verb.
      (let [agr-of-subj-of-main-verb (atom :top)]
        {:unify [verb-aux verb-subjective avere-common]
         :note "avere(aux): takes intrans"
         :synsem {:infl :present
                  :subcat {:1 {:agr agr-of-subj-of-main-verb}
                           :2 {:essere false
                               :reflexive false ;; in Italian, reflexive verbs are always essere=false.
                               ;; this additional qualification here (:reflexive false) is not necessary
                               ;; but is expected to reduce fails during generation.
                               :agr agr-of-subj-of-main-verb
                               :subcat {:1 {:agr agr-of-subj-of-main-verb}
                                        :2 '()}}}}})])
   "ballare" {:synsem {:cat :verb
                       :sem {:pred :dance}}}
   "bello"
   [;; non-comparative
    {:unify [adjective]
     :synsem {:sem {:pred :bello
                    :comparative false
                    }}} ;; for now, no restrictions on what can be beautiful.
    
    ;; comparative
    (let [complement-complement-sem (atom :top) ;; for now no restrictions
          complement-sem (atom {:pred :di
                               :mod complement-complement-sem})
          subject-sem (atom :top)] ;; subject can be anything.
      {:unify [adjective comparative]
       :synsem {:sem {:pred :bello
                      :arg1 subject-sem
                      :arg2 complement-complement-sem}
                :subcat {:1 {:cat :noun
                             :sem subject-sem}
                         :2 {:cat :prep
                             :sem complement-sem}}}})]
   "bene"
   {:synsem {:cat :adverb
             :sem {:pred :bene}}}

   "bere"
    {:italiano {:passato "bevuto"
                :future-stem "berr"
                :imperfect {:1sing "bevevo"
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
              :sem {:pred :drink
                    :reflexive false
                    :subj {:animate true}
                    :obj {:drinkable true}}}}
   "bianco"
     {:unify [adjective]
      :synsem {:cat :adjective
               :sem {:pred :bianco
                     :comparative false
                     :physical-object true
                     :human false}}
      :italiano {:masc {:plur "bianchi"}
                 :fem {:plur "bianche"}
                 :cat :adjective}}
   "birra"
   {:synsem {:agr {:gender :fem}
             :cat :noun
             :sem {:pred :birra
                   :artifact true
                   :drinkable true}}}
   "braccio"
   {:synsem {:agr {:gender :masc}
             :cat :noun
             :sem {:pred :braccio
                   :part-of-human-body true}}

    ;; adding "bracci" as irregular because
    ;; current morphology.clj would otherwise return
    ;; "braccii".
    ;; TODO: might not be an exception so much
    ;; as a ortho-pholological rule "io" -plur-> "ia"
    :italiano {:plur "bracci"}}

   "brutto"
   ;; non-comparative
   ;; TODO: add comparative
     {:unify [adjective]
      :synsem {:cat :adjective
               :sem {:pred :brutto
                     :comparative false
                     }} ;; for now, no restrictions on what can be ugly.
      :italiano {:cat :adjective}}
    
   "bucato"
   {:synsem {:cat :noun
             :agr {:gender :masc
                   :number :sing}
             :sem {:animate false
                   :drinkable false
                   :edible false
                   :legible false
                   :mass false
                   :pred :laundry
                   :speakable false}
             :subcat {:1 {:def :def}}}}

   "caffè" [{:synsem {:agr {:gender :masc
                            :number :sing}
                      :cat :noun
                      :sem {:pred :coffee}}}
            {:synsem {:agr {:gender :masc
                            :number :plur}
                      :cat :noun
                      :sem {:pred :coffee}}}]
   "calzoni"
   ;; inherently plural
   {:synsem {:cat :noun
             :agr {:gender :masc
                   :number :plur}
             :sem {:pred :calzoni
                   :clothing true}}}

   "cambiare" {:synsem {:cat :verb
                        :sem {:pred :change}}}
   
     "cambiarsi" {:unify [reflexive]
                  :synsem {:sem {:pred :change-clothes
                                 :subj {:human true}}}}
   
   "cancellare" {:synsem {:cat :verb
                          :sem {:pred :erase}}}

   "cantare" {:synsem {:cat :verb
                       :sem {:pred :sing
                             :subj {:human true}}}}
   "camicia"
   {:synsem {:agr {:gender :fem}
             :cat :noun
             :sem {:pred :camicia
                   ;; (although an exception would be tshirts with writing on them):
                   :legible false
                   :clothing true}}}
   "camminare"
   {:synsem {:sem {:pred :walk}
             :cat :verb}}
   "cane"
   {:synsem {:agr {:gender :masc}
             :cat :noun
             :sem {:pet true
                   :pred :cane}}}

   "capire" {:italiano {:boot-stem1 "capisc"}
             :synsem {:cat :verb
                      :sem {:pred :understand}}}

   "caricare" {:italiano {:future-stem "caricher"}
               :synsem {:cat :verb
                        :sem {:pred :caricare}}}
   "casa"
   [{:synsem {:agr {:gender :fem}
              :cat :noun
              :sem {:pred :casa
                    :city false
                    :place true}}}
   
    ;; in the sense of "a casa": no article needed.
    {:synsem {:case :acc
              :cat :noun
              :propernoun false
              :sem {:city false
                    :spec {:def :none} ;; "a casa", not "a mia casa", etc
                    :place true}
              :subcat '()}}
    ]

   "cattivo"
     {:unify [adjective]
      :synsem {:cat :adjective
               :sem {:pred :cattivo
                     :comparative false
                     :human true;; TODO:should not need this because child => human.
                     :child true}}
      :italiano {:cat :adjective}}

   ;; working on: "mi sono comprato un nuovo cellulare"
   "cellulare"
   {:synsem {:agr {:gender :masc}
             :cat :noun
             :sem {:pred :cellulare
                   :artifact true
                   :consumable false
                   :writable false
                   :place false
                   :speakable false}}}
  
   "cenare" {:synsem {:cat :verb
                      :essere false
                      :sem {:subj {:human true}
                            :pred :have-dinner}}}
     
   "chiacchierare" {:synsem {:cat :verb
                             :sem {:pred :chat}}}
      
   "chiamarsi" (let [subject-semantics (atom {:animate true})
                     called-semantics (atom :top)
                     subject-gender (atom :top)
                     subject-person (atom :top) ;; TODO: probably don't want this because it would not allow:
                     ;; "noi ci chiamiamo Gianluca e Gianni" (subject is 1st person, object is 3rd person)
                     subject-number (atom :top)
                     subject-agr (atom :top)]
                 {:synsem {:cat :verb
                           :essere true
                           :sem {:pred :be-called
                                 :reflexive true
                                 :subj subject-semantics
                                 :obj called-semantics}
                           :subcat {:1 {:agr {:number subject-number
                                              :person subject-person
                                              :gender subject-gender}
                                        :sem subject-semantics}
                                    :2 {:agr {:number subject-number
                                              :person subject-person
                                              :gender subject-gender}
                                        :pronoun true
                                        :reflexive true}
                                    ;; note that person is not shared.
                                    :3 {:agr {:number subject-number
                                              :gender subject-gender}
                                        :propernoun true
                                        :sem called-semantics
                                        :cat :noun
                                        :subcat '()}}}})

   "chiave" {:synsem {:agr {:gender :masc}
                      :cat :noun
                      :sem {:physical-object true
                            :pred :key
                            :place false}}}
   
   "chiedere" {:synsem {:cat :verb
                        :sem {:subj {:human true}
                              :pred :chiedere}}
               :italiano {:passato "chiesto"}}

   "chiudere" {:synsem {:cat :verb
                        :sem {:subj {:human true}
                              :pred :close}}
               :italiano {:passato "chiuso"}}

;     "chiunque"
;     {:synsem {:cat :fail ; :noun ;; disabling until more constraints are put on usage of it (TODO).
;               :pronoun true
;               :case :nom
;               :agr {:person :3rd
;                     :number :sing}
;               :sem {:human true
;                      :pred :chiunque
;                     :elective-existential true}
;               :subcat '()}}

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
   {:synsem {:agr {:gender :fem}
             :cat :noun
             :sem {:pred :cipolla
                   :edible true
                   :animate false
                   :artifact false}}}
      
   "cercare" {:synsem {:cat :verb
                       :essere false
                       :sem {:activity true
                             :discrete false
                             :pred :cercare
                             :subj {:animate true}}}}
   "città"
   {:synsem {:agr {:gender :fem}
             :cat :noun
             :sem {:city true
                   :pred :city}
             :subcat {:1 {:cat :det
                          :def :def}}}} ;; TODO: why must :def be specifically :def here?

   "cittadino" {:synsem {:cat :adjective
                         :sem {:pred :local}}}

     "colpire" (let [common {:italiano {:boot-stem1 "colpisc"}}]
                 [{:unify [common]
                   :synsem {:cat :verb
                            :sem {:pred :hit}}}
                  {:unify [common]
                   :synsem {:cat :verb
                            :sem {:pred :strike}}}])
   "cominciare"
   [{:synsem {:cat :verb
              :essere false
              :sem {:activity true
                    :discrete false
                    :pred :begin
                    :subj {:animate true}}}}
    {:synsem {:cat :verb
              :essere false
              :sem {:activity true
                    :discrete false
                    :pred :start
                    :subj {:animate true}}}}]

   "compito"
   {:synsem {:agr {:gender :masc}
             :cat :noun
             :sem {:pred :compito
                   :legible true
                   :speakable false
                   :buyable false
                   :artifact true}}}

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

   "contare" {:synsem {:sem {:pred :count}
                       :cat :verb}}
      
   "contento"
   [(let [complement-complement-sem (atom {:human true})
          complement-sem (atom {:pred :di
                                :mod complement-complement-sem})
          subject-sem (atom {:place true})]
      {:unify [adjective comparative]
       :synsem {:sem {:pred :contento
                      :arg1 subject-sem
                      :arg2 complement-complement-sem}
                :subcat {:1 {:cat :noun
                             :sem subject-sem}
                         :2 {:cat :prep
                             :sem complement-sem}}}}
    
      ;; non-comparative
      {:unify [adjective]
       :synsem {:cat :adjective
                :sem {:pred :contento
                      :comparative false
                      :human true}}})]
    
   "contraccambiare" {:synsem {:sem {:pred :reciprocate}
                               :cat :verb}}
   
   "correre"  {:italiano {:passato "corso"}
               :synsem {:cat :verb
                        :sem {:pred :run}}}

   "corrispondere"  {:synsem {:cat :verb
                              :sem {:pred :correspond}}}

   "corto"
   [(let [complement-complement-sem (atom {:human true}) ;; only humans can be short.
          complement-sem (atom {:pred :di
                                :mod complement-complement-sem})
          subject-sem (atom {:human true})] ;; only humans can be short.
      {:unify [adjective comparative]
       :synsem {:sem {:pred :corto
                      :arg1 subject-sem
                      :arg2 complement-complement-sem}
                :subcat {:1 {:cat :noun
                             :sem subject-sem}
                         :2 {:cat :prep
                             :sem complement-sem}}}})
    ;; non-comparative
    {:unify [adjective]
     :synsem {:cat :adjective
              :sem {:pred :corto
                    :comparative false
                    :human true}}
     :italiano {:cat :adjective}}]

   "creare"  {:synsem {:cat :verb
                       :sem {:pred :create}}}

   "controllare" {:synsem {:sem {:pred :check}
                           :cat :verb}}
                                    

   ;; TODO: account for "dare" being ditransitive.
   "dare"  {:synsem {:cat :verb
                     :sem {:pred :give}}
            :italiano {:present {:2sing "dai"
                                 :3plur "danno"}
                       :future-stem "dar"}}

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

   "del"
   {:synsem {:cat :det
             :def :partitivo
             :agr {:number :sing
                   :gender :masc}}}
   "dei"
   {:synsem {:cat :det
             :def :partitivo
             :agr {:number :plur
                   :gender :masc}}}
   "della"
   {:synsem {:cat :det
             :def :partitivo
             :agr {:number :sing
                   :gender :fem}}}
   "delle"
   {:synsem {:cat :det
             :def :partitivo
             :agr {:number :plur
                   :gender :fem}}}
   "deludere"
   {:italiano {:passato "deluso"}
    :synsem {:cat :Verb
             :essere false
             :sem {:deliberate false
                   :discrete true
                   :activity false
                   :pred :deludere
                   :subj {:human true}
                   :obj {:human true}}}}

   "desiderare"  {:synsem {:cat :verb
                           :sem {:pred :desire}}}
   
   "dettare" {:synsem {:cat :verb
                       :sem {:pred :dictate}}}
   
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
             :agr {:number :sing
                   :gender :fem}
             :mass true}}
   "di le"
   {:synsem {:cat :det
             :def :partitivo
             :agr {:number :plur
                   :gender :fem}}}
   "di il"
   {:synsem {:cat :det
             :def :partitivo
             :mass true
             :agr {:number :sing
                   :gender :masc}}}
   "difficile"
   ;; non-comparative
   ;; TODO: add comparative
     {:unify [adjective]
      :synsem {:cat :adjective
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
      :italiano {:cat :adjective
                 :fem {:sing "difficile"}}}

   "dipingere" {:synsem {:cat :verb
                         :sem {:pred :paint}}
                :italiano {:passato "dipinto"}}
   
   "dire" (let [shared-part-of-dire
                {:synsem {:cat :verb}
                 :italiano {:drop-e false
                            :passato "detto"
                            :future-stem "dir"
                            :present {:2plur "dite"}}}]
            [{:unify [shared-part-of-dire]
              :synsem {:sem {:pred :say}}}
             {:unify [shared-part-of-dire]
              :synsem {:sem {:pred :tell}}}])
      
   "diventare" {:synsem {:essere true 
                         :cat :verb
                         :sem {:pred :become}}}
      
     "divertirsi" {:unify [reflexive]
                   :synsem {:sem {:pred :have-fun
                                  :subj {:human true}}}}

   "domanda" {:synsem {:agr {:gender :fem}
                :cat :noun
                :sem {:pred :question}}}
   "domani"
     {:unify [sentential-adverb]
      :synsem {:cat :sent-modifier
               :sem {:pred :domani}
               :subcat {:1 {:infl :future
                            :sem {:tense :future}
                            :subcat '()}}}
      :italiano "domani"}

   "donna"
   {:synsem {:agr {:gender :fem}
             :cat :noun
             :sem {:human true
                   :pred :donna
                   :child false}}}
   "dopo"
   (let [time (atom {:time true})]
     {:synsem {:cat :prep
               :sem {:pred :after
                     :obj time}
               :subcat {:1 {:cat :noun
                            :subcat '()
                            :sem time}
                        :2 '()}}})
   "dopodomani"
     {:unify [sentential-adverb]
      :synsem {:cat :sent-modifier
               :sem {:pred :dopodomani}
               :subcat {:1 {:infl :future
                            :sem {:tense :future}
                            :subcat '()}}}}
   "dormire"
   {:synsem {:cat :verb
             :essere false
             :sem {:subj {:animate true}
                   :discrete false
                   :pred :sleep}}}

   "dovere" {:synsem {:cat :verb
                      :sem {:pred :have-to}}
             :italiano {:future-stem "dovr"
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

   "espresso" {:synsem {:agr {:gender :masc}
                :cat :noun
                :sem {:pred :espresso}}}

   "esprimere" {:italiano {:passato "espresso"}
                :synsem {:cat :verb
                         :sem {:pred :express}}}

   "essere"
   (let [essere-common 
         (let [infl (atom :top)
               agr (atom :top)]
           {:synsem {:agr agr
                     :cat :verb
                     :essere true
                     :infl infl
                     :subcat {:1 {:agr agr}}}
            :italiano {:agr agr
                       :future-stem "sar"
                       :infinitive "essere"
                       :infl infl
                       :present {:1sing "sono"
                                 :2sing "sei"
                                 :3sing "è"
                                 :1plur "siamo"
                                 :2plur "siete"
                                 :3plur "sono"}
                       :passato "stato"
                       :imperfect {:1sing "ero"
                                   :2sing "eri"
                                   :3sing "era"
                                   :1plur "eravamo"
                                   :2plur "eravate"
                                   :3plur "erano"}
                       :future {:1sing "sarò"
                                :2sing "sarai"
                                :3sing "sarà"
                                :1plur "saremo"
                                :2plur "sarete"
                                :3plur "saranno"}

                       :conditional {:1sing "sarei"
                                     :2sing "saresti"
                                     :3sing "sarebbe"
                                     :1plur "saremmo"
                                     :2plur "sareste"
                                     :3plur "sarebbero"}}})]
     [;; essere: adjective
      ;; TODO: unify essere-adjective, essere-prepositional-phrase
      ;; and essere-intensifier into one lexical entry.
      (let [gender (atom :top)
            number (atom :top)]
        {:unify [essere-common]
         :notes "essere-adjective"
         :synsem {:cat :verb
                  :sem {:pred :be
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
                                     :number number}}}}})

      ;; essere: prepositional phrase
      (let [gender (atom :top)
            number (atom :top)
            obj (atom {:pred :top
                       :place true})
            pred (atom :top)]
        {:unify [essere-common]
         :notes "essere-prepositional-phrase"
         :synsem {:cat :verb
                  :aux false
                  :sem {:pred pred
                        :obj obj}
                  ;; TODO: should not need agreement: should be covered by
                  ;; essere-common.
                  :subcat {:1 {:cat :noun
                               :agr {:gender gender
                                     :number number}}
                           :2 {:cat :prep
                               :subcat '()
                               :sem {:pred pred
                                     :obj obj}}
                           :3 '()}}})
         
      ;; TODO: do we need this? maybe remove?
      {:unify [essere-common]
       :notes "be"
       :synsem {:sem {:pred :be}}}

      ;; essere: copula ;; note that we don't enforce agreement the same here as we do in essere-adjective: TODO: try to make more consistent.
      (let [gender (atom :top)
            number (atom :top)
            human (atom :top)]
        {:unify [transitive essere-common]
         :notes "copula" ;; significant only for debugging.
         :synsem {:cat :verb
                  :subcat {:1 {:cat :noun
                               :agr {:gender gender
                                     :number number}}
                           :2 {:cat :noun
                               :pronoun {:not true} ;; accusative pronouns cause unbounded depth-first searches on the subject side. (TODO: not sure if this problem is still present)
                               :def {:not :demonstrativo}
                               :agr {:gender gender
                                     :number number}}}
                  :sem {:pred :be
                        :activity false
                        :discrete false
                        :subj {:human human}
                        :obj {:human human}}}})

      ;; essere: intensifier
      ;; this is for e.g "essere più alto di quelle donne belle (to be taller than those beautiful women)"
;           (let [gender (atom :top)
;                 number (atom :top)
;                 subject (atom {:agr {:gender gender
;                                     :number number}
;                               :cat :noun})
;                 comp-pred (atom :top)
;                 comp-sem (atom
;                           {:activity false
;                            :pred comp-pred
;                            :discrete false})]
;             (unify
;              verb-subjective
;              essere-common
;              {:notes "essere-intensifer"
;               :synsem {:cat :verb
;                        :subcat {:1 subject
;                                 :2 {:cat :intensifier
;                                     :sem comp-sem
;                                     :subcat {:1 subject
;                                              :2 '()}}}
;                        :sem {:pred comp-pred
;                              :intensified true
;                              :obj comp-sem}}}))

      {:unify [essere-common verb-aux verb-subjective]
       :notes "essere-aux"
       :italiano {:notes "essere-aux"}}])
      
   "evitare"  {:synsem {:cat :verb
                        :sem {:pred :avoid}}}
   "fare"
   (let [shared-part-of-fare
         {:synsem {:cat :verb}
          :italiano {:italiano "fare"
                     :drop-e false
                     :passato "fatto"
                     :future-stem "far"
                     :imperfect {:1sing "facevo"
                                 :2sing "facevi"
                                 :3sing "faceva"
                                 :1plur "facevamo"
                                 :2plur "facevate"
                                 :3plur "facevano"}
                     :present {:1sing "faccio"
                               :2sing "fai"
                               :3sing "fa"
                               :1plur "facciamo"
                               :2plur "fate"
                               :3plur "fanno"}}}]
     [{:unify [shared-part-of-fare]
       :synsem {:sem {:pred :do}}}
      {:unify [shared-part-of-fare]
       :synsem {:sem {:pred :make}}}
      (let [subject-semantics (atom {:human true})
            subject-agr (atom :top)]
        {:unify [shared-part-of-fare]
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
                               :sem subject-semantics}
                           :3 {:cat :adverb
                               :sem {:pred :bad}}}}})])
    
   "finestre" {:synsem {:agr {:gender :fem}
                        :cat :noun
                        :sem {:pred :window}}}
   
   "finire"  {:italiano {:boot-stem1 "finisc"
                         :boot-stem2 "fin"}
              :synsem {:cat :verb
                       :sem {:pred :finish}}}

   "formare"  {:synsem {:cat :verb
                        :sem {:pred :form}}}

   "fornire"  {:synsem {:cat :verb
                        :sem {:pred :furnish}}}
      
;; commenting out: no english equivalent      
;      "frequentare"  {:synsem {:cat :verb
;                               :sem {:pred :frequentare}}}

   "funzionare" {:synsem {:cat :verb
                          :essere false
                          :sem {:pred :work-nonhuman
                                :subj {:human false}}}}

   "gatto"  {:synsem {:agr {:gender :masc}
                      :cat :noun
                      :sem {:pred :cat
                            :pet true}}}

   "gestire" {:italiano {:boot-stem1 "gestisc"
                         :boot-stem2 "gest"}
              :synsem {:sem {:pred :manage}
                       :cat :verb}}
   "giallo"
   {:unify [adjective]
    :synsem {:cat :adjective
             :sem {:pred :yellow
                   :comparative false
                   :physical-object true
                   :human false}}}

   "giocare"  {:synsem {:cat :verb
                        :sem {;:obj {:games true}
                              :subj {:human true}
                              :pred :giocare}}
               :italiano {:future-stem "giocher"}}

   "Giovanni e io"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :masc
                    :person :1st
                    :number :plur}
              :sem {:human true
                    :pred :giovanni-and-i}
              :subcat '()}}]

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
   {:unify [determiner]
    :synsem {:cat :det
             :def :def
             :agr {:gender :masc
                   :number :plur}}}

   "idea" {:synsem {:agr {:gender :fem}
                :cat :noun
                :sem {:pred :idea}}}
   "il"
   {:unify [determiner]
    :synsem {:cat :det
             :def :def
             :agr {:gender :masc
                   :number :sing}}}

   "imparare"  {:synsem {:cat :verb
                         :sem {:pred :learn}}}

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
              :sem {:pred :I
                    :human true}
              :subcat '()}}

    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :masc
                    :person :1st
                    :number :sing}
              :sem {:pred :I
                    :human true}
              :subcat '()}}]

   "isola" {:synsem {:agr {:gender :fem}
                :cat :noun
                :sem {:pred :island}}}
   "la"
   ;; as with "lo", do we need both of these "lo" entries? try to get by with just one.
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
              :agr {:gender :fem
                    :number :sing}}}]
   "la loro"
   {:synsem {:cat :det
             :def :possessive
             :agr {:gender :fem
                   :number :sing}
             :sem {:number :plur
                   :person :3rd}}}
   "la mia"
   {:synsem {:cat :det
             :def :possessive
             :agr {:gender :fem
                   :number :sing}
             :sem {:number :sing
                   :person :1st}}}
   "la nostra"
   {:synsem {:cat :det
             :def :possessive
             :agr {:gender :fem
                   :number :sing}
             :sem {:number :plur
                   :person :1st}}}

   ;; TODO for below: add pronominal "la sua" (translated in English as "his" and "hers", depending on gender of the owner of the referent).
   ;; e.g. "I gatti sono i suoi. (The cats are hers) (if talking about a female owner) or (The cats are his) (if talking about a male owner).
   "la sua"
   {:synsem {:cat :det
             :def :possessive
             :agr {:gender :fem
                   :number :sing}
             :sem {:number :sing
                   :person :3rd}}}
   "la tua"
   [{:synsem {:cat :det
              :def :possessive
              :agr {:gender :fem
                    :number :sing}
              :sem {:number :sing
                    :person :2nd}}}

    {:synsem {:cat :det
              :def :possessive
              :agr {:gender :fem
                    :number :sing}
                :sem {:number :plur
                      :person :2nd}}}]

     "lavarsi" {:unify [reflexive]
                :synsem {:sem {:pred :wash
                               :subj {:human true}}}}

     "lavorare"  {:synsem {:cat :verb
                         :sem {:subj {:human true}
                               :pred :work-human}}}
   "le"
   {:synsem {:cat :det
             :def :def
             :agr {:gender :fem
                   :number :plur}}}
   "lei"
   (let [common {:synsem {:cat :noun
                          :pronoun true
                          :case :nom
                          :agr {:person :3rd
                                :gender :fem
                                :number :sing}
                          :sem {:pred :lei} ;; note: we don't specify human=true (english "it").
                          :subcat '()}}]
     [{:unify [common]
       :synsem {:sem {:human false}}}
      {:unify [common]
       :synsem {:sem {:human true}}}])
     
   "leggere"
   {:italiano {:passato "letto"}
    :synsem {:cat :verb
             :essere false
             :sem {:discrete false
                   :pred :read
                   :subj {:human true}
                   :obj {:legible true}}}}

   "lezione" {:synsem {:agr {:gender :fem}
                       :cat :noun
                       :sem {:pred :lesson}}}
   
   "libro" {:synsem {:agr {:gender :masc}
                     :cat :noun
                     :sem {:pred :libro}}}

   ;; as with "la", do we need both of these "lo" entries? try to get by with just one.
   "lo"
   [{:synsem {:cat :noun
              :pronoun true
              :case pronoun-acc
              :reflexive false
              :agr {:gender :masc
                    :person :3rd
                    :number :sing}
              :sem {:human true
                    :pred :lui}
              :subcat '()}}

    {:synsem {:cat :noun
              :pronoun true
              :case pronoun-acc
              :reflexive false
              :agr {:gender :masc
                    :person :3rd
                    :number :sing}
              :sem {:human false
                    :place false
                    :pred :lui}
              :subcat '()}
       :italiano {:initial true  ;; TODO: verify that 'lo' above and this below are being unified correctly.
                  :pronoun true
                  :cat :noun
                  :case pronoun-acc}}
    {:synsem {:cat :det
              :def :def
              :agr {:gender :masc
                    :number :sing}}}]
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
              :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :fem
                    :number :plur}
              :sem {:human false
                    :pred :loro}
              :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :3rd
                    :gender :masc
                    :number :plur}
              :sem {:human false
                    :pred :loro}
              :subcat '()}}
    ]

   "lui" (let [common {:synsem {:cat :noun
                                :pronoun true
                                :case :nom
                                :agr {:person :3rd
                                      :gender :masc
                                      :number :sing}
                                :sem {:pred :lui} ;; note: we don't specify human=true (english "it").
                                :subcat '()}}]
           [{:unify [common]
             :synsem {:sem {:human false}}}
            {:unify [common]
             :synsem {:sem {:human true}}}])

   "Luisa e io"
   [{:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:gender :fem
                   :person :1st
                   :number :plur}
             :sem {:human true
                   :pred :luisa-and-i}
             :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :masc
                    :person :1st
                    :number :plur}
              :sem {:human true
                    :pred :luisa-and-i}
              :subcat '()}}]
   
   "macchiare" {:synsem {:cat :verb
                         :sem {:pred :stain}}}
   "madre"
   {:synsem {:agr {:gender :fem}
             :cat :noun
             :sem {:human true
                   :pred :madre
                   :child false}}}

    "male" {:synsem {:cat :adverb
                     :sem {:pred :bad}}}

   "mancare"  {:italiano {:future-stem "mancher"}
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

   "mezzogiorno"
   {:unify [agreement-noun common-noun countable-noun masculine-noun]
    :synsem {:cat :noun
             :sem {:pred :noon
                   :time true}}}
   "mi"
   {:synsem {:cat :noun
               :pronoun true
               :case :acc
               :agr {:person :1st
                     :number :sing}
               :sem {:human true}
               :subcat '()}}

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

   "nascere" {:synsem {:cat :verb
                       :essere true
                       :sem {:pred :be-born}}
              :italiano {:passato "nato"}}
   
   "neonato"
   {:synsem {:agr {:gender :masc}
             :cat :noun
             :sem {:pred :newborn
                   :human true
                   :adult false}}}
   
   ;; non-comparative
   ;; TODO: add comparative
   "nero"
   {:unify [adjective]
    :synsem {:cat :adjective
             :sem {:pred :nero
                   :comparative false
                   :physical-object true
                   :human false}}}

   "noi"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :fem
                    :person :1st
                    :number :plur}
              :sem {:pred :noi
                    :human true}
              :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :masc
                    :person :1st
                    :number :plur}
              :sem {:pred :noi
                    :human true}
              :subcat '()}}]

   "osservare" {:synsem {:cat :verb
                         :sem {:pred :observe}}}
   
   "ostana" {:synsem {:cat :noun
                      :propernoun true
                      :sem {:pred :ostana
                            :city true}}}

   "ottenere" {:synsem {:cat :verb
                        :sem {:pred :obtain}}
               :italiano {:passato "ottenuto"
                          :present {:1sing "ottengo"
                                    :2sing "ottieni"
                                    :3sing "ottiene"
                                    :3plur "ottengono"}
                          :future-stem "otterr"}}
   "pane"
   ;; inherently singular.
   {:synsem {:agr {:gender :masc}
             :cat :noun
             :sem {:pred :bread}
             :subcat {:1 {:cat :det
                          :number :sing
                          :def :def}}}}
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
      {:unify [common1 transitive]
       :synsem {:sem {:obj {:speakable true}}}}
      {:unify [common1 intransitive intransitive-unspecified-obj]}
      
      {:unify [common2 intransitive intransitive-unspecified-obj]}))
   
   "partire" {:synsem {:essere true 
                       :cat :verb
                       :sem {:pred :leave}}}
   "pensare"
   {:synsem {:cat :verb
             :essere false
             :sem {:pred :think
                   :activity false
                   :discrete false
                   :subj {:human true}}}}

   "piangere" {:italiano {:passato "pianto"}
               :synsem {:cat :verb
                        :sem {:pred :cry}}}

   "piegare" {:synsem {:cat :verb
                       :sem {:pred :fold}}}

   "portare"  [{:synsem {:cat :verb
                         :sem {:pred :carry}}}
               {:synsem {:cat :verb
                         :sem {:pred :wear}}}]
   "potere" 
   (let [shared-part-of-potere
         {:synsem {:cat :verb}
          :italiano {:future-stem "potr"
                     :drop-e true
                     :present {:1sing "posso"
                               :2sing "puoi"
                               :3sing "può"
                               :1plur "possiamo"
                               :2plur "potete"
                               :3plur "possono"}}}]
     [{:unify [shared-part-of-potere]
       :synsem {:sem {:pred :can}}}
      {:unify [shared-part-of-potere]
       :synsem {:sem {:pred :may}}}
      {:unify [shared-part-of-potere]
       :synsem {:sem {:pred :be-able-to}}}])
                                      
   "prendere" {:synsem {:cat :verb :sem {:pred :grab}}
               :italiano {:passato "preso"}}

     "prepararsi" {:unify [reflexive]
                   :synsem {:sem {:pred :get-ready
                                  :subj {:human true}}}}

     "prenotare" {:synsem {:cat :verb
                         :sem {:pred :reserve}}}
   "qualche"
   {:synsem {:cat :det
             :def :indef
             :agr {:mass false
                   :number :sing}}}

   "raccontare" {:synsem {:cat :verb
                          :sem {:pred :recount}}}

   "recuperare" {:synsem {:cat :verb
                          :sem {:pred :recover}}}
   "ragazza"
   {:synsem {:cat :noun
             :agr {:gender :fem}
             :sem {:pred :girl
                   :human true}}}
   "ragazzo"
   {:synsem {:cat :noun
             :agr {:gender :masc}
             :sem {:pred :boy
                   :human true}}}
              
   "restare" {:synsem {:essere true 
                            :cat :verb
                            :sem {:pred :remain2}}}
   
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
                        :sem {:pred :remain1}}
               :italiano {:passato "rimasto"
                          :present {:1sing "rimango"
                                    :3plur "rimangono"}
                          :future-stem "rimarr"}}

   "riposare" {:synsem {:cat :verb
                        :sem {:pred :rest}}}

   "riscaldare" {:synsem {:cat :verb
                          :sem {:pred :warm}}}

   "rispondere" {:synsem {:cat :verb
                          :essere false
                          :sem {:pred :answer}}
                 :italiano {:passato "risposto"}}
   "ristorante"
   {:synsem {:agr {:gender :fem}
             :cat :noun
             :sem {:pred :restaurant
                   :city false
                   :place true}}}
      
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
   {:unify [adjective]
    :synsem {:cat :adjective
             :sem {:pred :rosso
                   :comparative false
                   :physical-object true
                   :human false}}}
   
   "salire" (let [common {:synsem {:cat :verb}
                                   :essere true
                                   :sem {:subj {:animate true}}
                          :italiano {:italiano "salire"
                                     :present {:1sing "salgo"
                                               :3plur "salgono"}}}]
              [{:unify [common]
                :synsem {:sem {:pred :get-on}}}
               {:unify [common]
                :synsem {:sem {:pred :go-upstairs}}}])
   
   "scappare"  {:synsem {:cat :verb 
                         :sem {:pred :escape}}}

   "scattare" {:synsem {:cat :verb
                        :sem {:pred :snap-pictures}}}

   "scaricare" {:synsem {:cat :verb 
                      :sem {:pred :scaricare}}}
   
   "scendere" [{:synsem {:cat :verb 
                         :essere true
                         :sem {:pred :get-off}}
                :italiano {:passato "sceso"}}                
               {:synsem {:cat :verb
                         :essere true
                         :sem {:pred :go-downstairs}}
                :italiano {:passato "sceso"}}]
   
   "scrivere"  {:synsem {:cat :verb 
                         :sem {:pred :scrivere}}
                :italiano {:passato "scritto"}}

     "sedersi" {:unify [reflexive]
                :synsem {:sem {:pred :sit-down
                               :subj {:human true}}}
                :italiano {:present {:1sing "siedo"
                                     :2sing "siedi"
                                     :3sing "siede"
                                     :1plur "sediamo"
                                     :2plur "sedete"
                                     :3plur "siedono"}}}
     
   "sgridare" {:synsem {:cat :verb
                        :sem {:subj {:human true}
                              :pred :scold}}}
     "si" [{:unify [pronoun-reflexive]

             ;; feminine singular
            :synsem {:agr {:person :3rd
                           :gender :fem
                           :number :sing}}}

           {:unify [pronoun-reflexive]
            ;; masculine singular
            :synsem {:agr {:person :3rd
                           :gender :masc
                           :number :sing}}}

           ;; plural: unspecified gender
           {:unify [pronoun-reflexive]
            :synsem {:agr {:person :3rd
                           :number :plur}}}]
   
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
                       
   "stare"  {:synsem {:cat :verb
                      :essere true
                        :sem {:pred :stay}}
               :italiano {:present {:2sing "stai"
                                    :3plur "stanno"}
                          :future-stem "star"}}                    

   "studiare"  {:synsem {:cat :verb 
                      :sem {:pred :study}}}

   "studente"  {:synsem {:agr {:gender :masc}
                         :cat :noun
                         :sem {:pred :student}}}

   "suonare"  {:synsem {:cat :verb 
                      :sem {:subj {:human true}
                            :pred :suonare}}}
;                                            :obj {:music true}}}})

   "svegliarsi" {:unify [reflexive]
                 :synsem {:sem {:pred :wake-up
                                :subj {:animate true}}}}

   "svenire" {:synsem {:cat :verb
                      :essere true
                      :sem {:pred :faint}}
             :italiano {:passato "svenuto"
                        :present {:1sing "svengo"
                                  :2sing "svieni"
                                  :3sing "sviene"
                                  :3plur "svengono"}
                        :future-stem "sverr"}}

   "sviluppare"  {:synsem {:cat :verb 
                      :sem {:pred :develop}}}

   "tagliare"  {:synsem {:cat :verb
                         :sem {:pred :cut}}}

   "telefonare"  {:synsem {:cat :verb 
                      :essere false
                           :sem {:pred :telefonare}}}

   "tenere"  {:synsem {:cat :verb 
                      :sem {:pred :hold}}
              :italiano {:passato "tenuto"
                         :present {:1sing "tengo"
                                   :2sing "tieni"
                                   :3sing "tiene"
                                   :3plur "tengono"}
                         :future-stem "terr"}}
   "ti"
   {:synsem {:cat :noun
             :pronoun true
             :case pronoun-acc
             :agr {:person :2nd
                   :number :sing}
             :sem {:human true}
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
              :sem {:pred :tu
                    :human true}
              :subcat '()}}
    {:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:person :2nd
                    :gender :masc
                    :number :sing}
              :sem {:pred :tu
                    :human true}
              :subcat '()}}]

   "un" [{:synsem {:cat :det
                   :def :indef
                   :mass false
                   :agr {:gender :masc
                         :number :sing}}}
         {:generate-with false ;; parsing only, not generation: don't generate e.g. "un donna".
          :synsem {:cat :det
                   :def :indef
                   :mass false
                   :agr {:gender :fem ;; 'un' can be feminine in the case where next word is an adjective (e.g. "alta")
                         ;; and we tokenize "un'alta donna" as [un] [alta] [donna].
                         :number :sing}}}]
   "una"
    {:synsem {:cat :det
              :def :indef
              :mass false
              :agr {:gender :fem
                    :number :sing}}}
   "uno"
    {:synsem {:cat :det
              :def :indef
              :mass false
              :agr {:gender :masc
                    :number :sing}}}
   "uomo"
   {:italiano {:plur "uomini"}
    :synsem {:agr {:gender :masc}
             :cat :noun
             :sem {:human true
                   :pred :man
                   :child false}}}
   
   "usare"  {:synsem {:cat :verb
                      :sem {:pred :usare}}}

   "uscire" (let [common {:synsem {:cat :verb
                                   :essere true
                                   :sem {:subj {:animate true}}}
                          :italiano {:italiano "uscire"
                                     :boot-stem1 "esc"
                                     :boot-stem2 "usc"}}]
              [{:unify [common]
                :synsem {:sem {:pred :go-out}}}
               {:unify [common]
                :synsem {:sem {:pred :exit}}}])
   "vedere" 
   {:synsem {:cat :verb
             :sem {:pred :see}}
    :italiano {:passato "visto"
               :future-stem "vedr"}}
   
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
                        :future-stem "verr"}}

   "ventotto" {:synsem {:cat :det
                        :def :twentyeight
                        :agr {:number :plur}}}
   
     "vestirsi" {:unify [reflexive]
                 :synsem {:sem {:pred :get-dressed
                                :subj {:human true}}}}
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
   "vino" {:synsem {:agr {:gender :masc}
                    :cat :noun
                    :pred :vino
                    :sem {:pred :vino}}}
   "vivere" 
   ;; FIXME: this is simply a duplicate apparently?
   [{:synsem {:cat :verb
              :sem {:subj {:animate true}
                    :pred :live}
              :essere true}
     :italiano {:passato "vissuto"
                :future-stem "vivr"}}

    {:synsem {:cat :verb
              :sem {:pred :live}
              :essere true}
     :italiano {:passato "vissuto"
                :future-stem "vivr"}}]
   "voi"
   [{:synsem {:cat cat-of-pronoun
              :pronoun true
              :case :nom
              :agr {:person :2nd
                    :gender :fem
                    :number :plur}
              :sem {:pred :voi
                    :human true}
              :subcat '()}}

    {:synsem {:cat cat-of-pronoun
              :pronoun true
              :case :nom
              :agr {:person :2nd
                    :gender :masc
                    :number :plur}
              :sem {:pred :voi
                    :human true}
              :subcat '()}}]
            
    "volere" {:synsem {:cat :verb
                       :sem {:pred :want}}
              :italiano {:future-stem "vorr"
                         :drop-e true
                         :present {:1sing "voglio"
                                   :2sing "vuoi"
                                   :3sing "vuole"
                                   :1plur "vogliamo"
                                   :2plur "volete"
                                   :3plur "vogliono"}}}

   "ziano" {:synsem {:agr {:gender :masc}
                     :cat :noun
                     :sem {:pred :shoe}}}
   
   ":top" {:synsem :top
           :top true}
   ":top-noun" {:synsem {:cat :noun}
                :top true}


     }))

(def lexicon-source-unify
  (zipmap
   (sort (keys lexicon-source))
   (map (fn [k]
          (let [vals (get lexicon-source k)
                vals (cond (seq? vals)
                           vals
                           (vector? vals)
                           vals
                           true [vals])]
            (map (fn [v]
                   (reduce unify
                           (cons
                            (dissoc v :unify)
                            (map (fn [each-unify-arg]
                                   (cond (fn? each-unify-arg)
                                         (each-unify-arg)
                                         true each-unify-arg))
                                 (:unify v)))))
                 vals)))
        (sort (keys lexicon-source)))))

;; see TODOs in lexiconfn/compile-lex (should be more of a pipeline as opposed to a argument-position-sensitive function.
(def lexicon (-> (compile-lex lexicon-source-unify
                              exception-generator 
                              ;; TODO: rewrite phonize as (constrain-val-if)(one or more)
                              phonize
                              ;; TODO: rewrite italian-specific-rules as (constrain-vals-if)(one or more)
                              italian-specific-rules)

                 (constrain-vals-if
                  (fn [val]
                    (not (nil? (get universals (get-in val [:synsem :sem :pred])))))
                  (fn [val]
                    (get universals (get-in val [:synsem :sem :pred]))))

                 ;; TODO: refactor this; it's very monolithic currently:
                 it-pos/intransitivize

                 ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
                 ;; TODO: refactor this; it's very monolithic currently:
                 it-pos/transitivize

                 (constrain-vals-if
                  (fn [val]
                    (and (= :noun (get-in val [:synsem :cat]))
                         (= true (get-in val [:synsem :reflexive]))
                         (= true (get-in val [:synsem :pronoun]))))
                  (fn [val]
                    (unify (let [case (atom :acc)
                                 cat (atom :noun)]
                             {:synsem {:cat cat
                                       :pronoun true
                                       :subcat '()
                                       :reflexive true
                                       :case case}
                              :italiano {:cat cat
                                         :case case}}))))
                 (constrain-vals-if
                  (fn [val]
                    (and (= (get-in val [:synsem :cat]) :noun)
                         (or (= (get-in val [:synsem :agr :gender]) :masc)
                             (= (get-in val [:synsem :agr :gender]) :fem))
                         (= false (get-in val [:synsem :propernoun] false))
                         (= false (get-in val [:synsem :pronoun] false))
                         (not (= '() (get-in val [:synsem :subcat] :top))))) ;; some
                  ;; some nouns e.g. "casa" may have a sense that requires no determiner (subcat = '())
                  ;; in such cases, don't apply agreement-noun.
                         
                  (fn [val]
                    (unify val it-pos/agreement-noun)))

                 (constrain-vals-if
                  (fn [val]
                    (= (get-in val [:synsem :cat])
                       :verb))
                  (fn [val]
                    (unify val {:stage1 :ok})))
                 
                 ;; If a verb is not specifically marked as reflexive, it
                 ;; is {:reflexive false}, to prevent generation of reflexive
                 ;; sentences using nonreflexive verbs.
                 ;; TODO: consider moving this to within intransitivize and transitivize:
                 ;; that is, within babel.italiano.pos, mark certain parts of speech
                 ;; as reflexive=false to accomplish the same thing as we
                 ;; are doing here.
                 (constrain-vals-if
                  (fn [val]
                    (and (= (get-in val [:synsem :cat])
                            :verb)
                         (= (get-in val [:synsem :aux] false)
                            false)
                         (= :none (get-in val [:synsem :sem :reflexive] :none))))
                  (fn [val]
                    (unify val {:synsem {:sem {:reflexive false}}})))
                 
                 ;; if object is not specified, then set to :unspec.
                 ;; this prevents translations that may have actual objects - e.g. would allow translations like:
                 ;; "io mangio" => "I eat the bread" whereas a better translation is simply "I eat".
                 (if-then {:top false
                           :synsem {:cat :verb
                                    :aux false
                                    :sem {:obj :unspec
                                          :reflexive false}}}
                          {:synsem {:sem {:obj :unspec}}})

                 ;; subject of verbs must have nominative case: prevents wrong things like article-less noun "casa" being the subject of a sentence.
                 (if-then {:synsem {:cat :verb}}
                          {:synsem {:subcat {:1 {:case :nom}}}})
                 
                 ;; filters out any verbs without an inflection: infinitive verbs should have inflection ':top', 
                 ;; rather than not having any inflection.
                 (filter-vals
                  #(or (not (= :verb (get-in % [:synsem :cat])))
                       (not (= :none (get-in % [:synsem :infl] :none)))))

                 (rewrite-keys 
                  (fn [k]
                    (cond
                      (= false analyze-lexemes)
                      k
                      
                      ;; TODO: only an example provided here:
                      ;; if analyze-lexemes is true, replace this example with use of
                      ;; italiano.morphology/preposition-plus-article regexp pairs
                      (= k "alla prossima")
                      "a la prossima"

                      true k)))))
