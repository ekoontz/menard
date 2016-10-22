(ns babel.espanol.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [babel.lexiconfn :refer [compile-lex if-then
                            map-function-on-map-vals unify]]
   [babel.espanol.morphology :as morph]
   [babel.espanol.pos :refer [agreement-noun cat-of-pronoun common-noun determiner
                              feminine-noun intransitivize masculine-noun transitivize]]
   [babel.pos :as pos :refer [pronoun-acc]]
   [dag_unify.core :refer [fail? get-in]]))

(def lexicon-source 
  {""
   [{:use-for-generation false
     :synsem {:cat :noun
              :pronoun true
              :null-pronoun true
              :case :nom
              :agr {:person :1st
                    :number :sing}
              :sem {:human true
                    :pred :I}}}
    {:use-for-generation false
     :synsem {:cat :noun
              :pronoun true
              :null-pronoun true
              :case :nom
              :agr {:person :2nd
                    :number :sing}
              :sem {:human true
                    :pred :tu}}}
    {:use-for-generation false
     :synsem {:cat :noun
              :pronoun true
              :null-pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :sing}
              :sem {:human :top ; intended for use as both "he" and "it"
                    :pred :lui}}}
    {:use-for-generation false
     :synsem {:cat :noun
              :pronoun true
              :null-pronoun true
              :case :nom
              :agr {:person :1st
                    :number :plur}
              :sem {:human true
                    :pred :noi}}}
    {:use-for-generation false
     :synsem {:cat :noun
              :pronoun true
              :null-pronoun true
              :case :nom
              :agr {:person :2nd
                    :number :plur}
              :sem {:human true
                    :pred :voi}}}
    {:use-for-generation false
     :synsem {:cat :noun
              :pronoun true
              :null-pronoun true
              :case :nom
              :agr {:person :3rd
                    :number :plur}
              :sem {:human :top ; intended for use as "they" whether human or not.
                    :pred :loro}}}
    ]

   "abandonar" {:synsem {:cat :verb
                         :sem {:pred :abandon}}}

   "abrazar" {:synsem {:cat :verb
                       :sem {:pred :hug}}}
   
   "acabar" {:synsem {:cat :verb
                      :sem {:pred :finish}}}

   "aceptar" {:synsem {:cat :verb
                       :sem {:pred :accept}}}

   "acertar" {:espanol {:boot-stem "aciert"}
              :synsem {:cat :verb
                       :sem {:pred :guess}}}
   
   "acompañar" {:synsem {:cat :verb
                         :sem {:pred :accompany}}}

   "advertir" {:espanol {:boot-stem "adviert"}
               :synsem {:cat :verb
                        :sem {:pred :warn}}}
   
   "almorzar" {:espanol {:boot-stem "almuerz"}
               :synsem {:cat :verb
                        :sem {:pred :have-lunch}}}
   
   "andar" {:espanol {:preterito-stem "anduv"}
               :synsem {:cat :verb
                        :sem {:pred :walk}}}
   
   
   
   "anunciar" {:synsem {:cat :verb
                        :sem {:pred :announce}}}

   "añadir" {:synsem {:cat :verb
                        :sem {:pred :add}}}
   
   "apagar" {:synsem {:cat :verb
                      :sem {:pred :turn-off}}}
   
   "apoyar" {:synsem {:cat :verb
                      :sem {:pred :support}}}

   "aprender" {:synsem {:cat :verb
                        :sem {:pred :learn}}}
   
   "aprobar" {:synsem {:cat :verb
                        :sem {:pred :approve}}}
   
   "aprovechar" [
                 {:synsem {:cat :verb
                           :sem {:pred :take-advantage-of}}}
                 {:synsem {:cat :verb
                           :sem {:pred :enjoy}}}]
   
   "asegurar" [{:synsem {:cat :verb
                         :sem {:pred :assure}}}
               {:synsem {:cat :verb
                         :sem {:pred :insure}}}]
   
   "aumentar" {:synsem {:cat :verb
                        :sem {:pred :increase}}}
   
   "ayudar" {:synsem {:cat :verb
                      :sem {:pred :aiutare
                            :obj {:human true}}}}
   
   "bailar" {:synsem {:cat :verb
                     :sem {:pred :dance}}}
   
   "bajar" {:synsem {:cat :verb
                     :sem {:pred :lower}}}
   
   "beber" {:synsem {:cat :verb
                     :sem {:pred :drink}}}
   
   "buscar" [{:synsem {:cat :verb
                       :sem {:pred :look-for}}}
             {:synsem {:cat :verb
                       :sem {:pred :look-up}}}]
   "caber" {}
   "caerse" (let [subject-semantics (atom {:human true})
         subject-agr (atom :top)]
     {:espanol {:boot-stem "diviert"}
      :synsem {:cat :verb
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
   
   "cambiar" {:synsem {:cat :verb
                       :sem {:pred :change}}}
   
   "cambiarse"
   (let [subject-semantics (atom {:human true})
         subject-agr (atom :top)]
     {:synsem {:cat :verb
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
   
   
   "caminar" {:synsem {:cat :verb
                        :sem {:pred :walk}}}
   "cancelar" {:synsem {:cat :verb
                        :sem {:pred :cancel}}}
   "cantar" {}
   "casarse" {}
   
   "cenar" {:synsem {:cat :verb
                       :sem {:pred :have-dinner}}}
   
   "cerrar" {:espanol {:boot-stem "cierr"}
             :synsem {:cat :verb
                      :sem {:pred :close}}}
   
   "comentar" {:synsem {:cat :verb
                        :sem {:pred :comment}}}
   
   "comenzar" {:espanol {:boot-stem "comienz"}
             :synsem {:cat :verb
                      :sem {:pred :begin}}}
   
   "comer" {:synsem {:cat :verb
                     :sem {:pred :eat}}}

   "compartir" {:synsem {:cat :verb
                         :sem {:pred :share}}}
   
   "comprar" {:synsem {:cat :verb
                       :sem {:pred :comprare
                             :subj {:human true}
                             :obj {:buyable true}}}}
   
   "comprender" {:synsem {:cat :verb
                          :sem {:pred :understand-deeply}}}
   
   "confesar" {:espanol {:boot-stem "confies"}
             :synsem {:cat :verb
                      :sem {:pred :confess}}}
   
   "conducir" {}
   "conocer" {}
   
   "consentir" {:espanol {:boot-stem "consient"}
                :synsem {:cat :verb
                         :sem {:pred :consent}}}
   
   "conservar" [{:synsem {:cat :verb
                          :sem {:pred :conserve}}}
                {:synsem {:cat :verb
                          :sem {:pred :preserve}}}]
   
   "considerar" {:synsem {:cat :verb
                          :sem {:pred :consider}}}
   
   "contar" {:espanol {:boot-stem "cuent"}
             :synsem {:cat :verb
                      :sem {:pred :count}}}
   
   "contestar" {:synsem {:cat :verb
                         :sem {:pred :answer}}}
   
   "convertir" {:espanol {:boot-stem "conviert"}
                :synsem {:cat :verb
                         :sem {:pred :convert}}}
   
   "correr" {:synsem {:cat :verb
                      :sem {:pred :run}}}
   
   "corresponder" {:synsem {:cat :verb
                            :sem {:pred :correspond}}}
   
   "cortar" {:synsem {:cat :verb
                      :sem {:pred :cut}}}
   
   "crear" {:synsem {:cat :verb
                     :sem {:pred :create}}}
   
   ;; TODO: clarify semantics of this in English 
   ;;                     "cumplir" {:synsem {:cat :verb
   ;;                                         :sem {:pred :turn-years}}}
   
   
   "creer" {:synsem {:cat :verb
                          :sem {:pred :believe}}}
   
   
   "dañar" {}
   
   "dar" {:espanol  {:present {:1sing "doy"
                               :2plur "dais"}
                     :preterito {:1sing "di"
                                 :2sing "diste"
                                 :3sing "dio"
                                 :1plur "dimos"
                                 :2plur "disteis"
                                 :3plur "dieron"}}
          :synsem {:cat :verb
                   :sem {:pred :give}}}
   
   "deber" {:synsem {:cat :verb
                     :sem {:pred :have-to}}}
   
   "defender" {:espanol {:boot-stem "defiend"}
                :synsem {:cat :verb
                         :sem {:pred :defend}}}

  "decidir" {:synsem {:cat :verb
                       :sem {:pred :decide}}}
  
  "decir" (let [common
                {:espanol {:futuro-stem "dir"
                           :present {:1sing "digo"
                                     :2sing "dices"
                                     :3sing "dice"
                                     :1plur "decimos"
                                     :2plur "decís"
                                     :3plur "dicen"}
                           :preterito-stem "dij"}}]
            [(unify common
                    {:synsem {:sem {:pred :say}}})
             (unify common
                    {:synsem {:sem {:pred :tell}}})])
   
   "dejar" {:synsem {:cat :verb
                     :sem {:pred :leave-behind}}}
   
   
   "deletrear" {}
   
   "desarrollar" {:synsem {:cat :verb
                           :sem {:pred :develop}}}
   
   "desear" {:synsem {:cat :verb
                      :sem {:pred :desire}}}
   
   "despertarse" {}
   "dibujar" {}
   
   "divertirse"
   (let [subject-semantics (atom {:human true})
         subject-agr (atom :top)]
     {:espanol {:boot-stem "diviert"}
      :synsem {:cat :verb
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
   
   "dormir" {:espanol {:boot-stem "duerm"
                       :pret-stem "durm"}
             :synsem {:cat :verb
                      :sem {:pred :sleep}}}
   "dormirse"
   (let [subject-semantics (atom {:human true})
         subject-agr (atom :top)]
     {:espanol {:boot-stem "duerm"
                :pret-stem "durm"}
      :synsem {:cat :verb
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
   
   "echar" {:synsem {:cat :verb
                     :sem {:pred :throw-out}}}
   "el"
   (unify determiner
          {:synsem {:cat :det
                    :def :def
                    :gender :masc
                    :number :sing}})
   
   "él" [{:synsem {:cat cat-of-pronoun
                   :pronoun true
                   :case :nom
                   :agr {:person :3rd
                         :gender :masc
                         :number :sing}
                   :sem {:human true
                         :gender :masc
                         :pred :lui}
                   :subcat '()}}
         {:synsem {:cat cat-of-pronoun
                   :pronoun true
                   :case :nom
                   :agr {:person :3rd
                         :gender :masc
                         :number :sing}
                   :sem {:human false
                         :gender :masc
                         :pred :lui}
                   :subcat '()}}]
   "ella"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :sing}
             :sem {:gender :fem
                   :pred :lei}
             :subcat '()}}
   "ellos"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :masc
                   :number :plur}
             :sem {:gender :masc
                   :pred :loro}
             :subcat '()}}
   "ellas"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :plur}
             :sem {:gender :fem
                   :pred :loro}
             :subcat '()}}
           
   "empezar" {:espanol {:boot-stem "empiez"}
              :synsem {:cat :verb
                       :sem {:pred :begin}}}
   
   "encender" {:espanol {:boot-stem "enciend"}
             :synsem {:cat :verb
                      :sem {:pred :light}}}
   
   "encontrar" {}
   
   "enojarse"
   (let [subject-semantics (atom {:human true})
         subject-agr (atom :top)]
     {:synsem {:cat :verb
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
   
   "enseñar" [{:note "e-"
               :synsem {:cat :verb
                        :sem {:pred :show}}}
              {:synsem {:cat :verb
                        :sem {:pred :teach}}}]
   
   "entender" {:espanol {:boot-stem "entiend"}
               :synsem {:cat :verb
                        :sem {:pred :understand-simply}}}
   
   "entrar" {:synsem {:cat :verb
                      :sem {:pred :enter}}}
   
   "enviar" {}
   
   "escapar" {:synsem {:cat :verb
                       :sem {:pred :escape}}}
   
   "escribir" {}
  
   "escuchar" {:synsem {:cat :verb
                        :sem {:pred :listen-to}}}
   
   "esperar" [{:synsem {:cat :verb
                        :sem {:pred :wait-for}}}
              {:synsem {:cat :verb
                        :sem {:pred :hope}}}]
   
   "estudiar" {:synsem {:cat :verb
                        :sem {:pred :study}}}
   
   "evitar" {:synsem {:cat :verb
                      :sem {:pred :avoid}}}
   
   "existir" {:synsem {:cat :verb
                       :sem {:pred :exist}}}
   
   "explicar" {}
   
   "expresar" {:synsem {:cat :verb
                        :sem {:pred :express}}}
   
   ;; TODO: fix up so that it does not need to be disabled.
;   "faltar" {:synsem {:cat :verb
;                      :sem {:pred :to-be-missing}}}
   
   "firmar" {}
   
   "formar" {:synsem {:cat :verb
                      :sem {:pred :form}}}
   
   "fregar" {:espanol {:boot-stem "frieg"}
             :synsem {:cat :verb
                      :sem {:pred :scrub}}}
   
   "fumar" {}
   
   "funcionar" {:synsem {:cat :verb
                         :sem {:subj {:human false}
                               :pred :work-nonhuman}}}
   
   "ganar" [{:synsem {:cat :verb
                      :sem {:pred :earn
                            :subj {:human true}
                            :obj {:human false}}}}
            {:synsem {:cat :verb
                      :sem {:pred :win
                            :subj {:human true}
                            :obj {:human false}}}}]
   
   ;; TODO: handle syntax/semantics mismatch between Italian/Espanol and English.
                                        ;                     "gustar" {:synsem {:cat :verb
                                        ;                                         :sem {:pred :??}}}
   
   "gastar" {}
   
   "hablar" [{:synsem {:cat :verb
                       :sem {:pred :speak
                             :subj {:human true}}}}
             {:synsem {:cat :verb
                       :sem {:pred :talk
                             :subj {:human true}}}}]
   "hacer"
   (let [common {:espanol {:futuro-stem "har"
                           :present {:1sing "hago"
                                     :2sing "haces"
                                     :3sing "hace"
                                     :1plur "hacemos"
                                     :2plur "hacéis"
                                     :3plur "hacen"}
                           :preterito {:1sing "hize"
                                       :2sing "hiciste"
                                       :3sing "hize"
                                       :1plur "hicimos"
                                       :2plur "hicisteis"
                                       :3plur "hicieron"}}}]
     [(unify common
             {:synsem {:sem {:pred :do}}})
      (unify common
             {:synsem {:sem {:pred :make}}})])
   
   "herir" {}
   
   "hervir" {:espanol {:boot-stem "hierv"}
             :synsem {:cat :verb
                      :sem {:pred :boil}}}
   
   "intentar" {}
   
   "ir" {:espanol {:futuro-stem "ir"
                   :present {:1sing "voy"
                             :2sing "vas"
                             :3sing "va"
                             :1plur "vamos"
                             :2plur "vais"
                             :3plur "van"}
                   :preterito {:1sing "fui"
                               :2sing "fuiste"
                               :3sing "fue"
                               :1plur "fuimos"
                               :2plur "fuisteis"
                               :3plur "fueron"}
                   :imperfecto {:1sing "iba"
                                :2sing "ibas"
                                :3sing "iba"
                                :1plur "ibamos"
                                :2plur "ibais"
                                :3plur "iban"}}
         :synsem {:cat :verb
                  :sem {:subj {:animate true}
                        :pred :go}}}

   "Juan" {:synsem {:cat :noun
                    :propernoun true
                    :case :nom
                    :agr {:gender :masc
                          :person :3rd
                          :number :sing}
                    :sem {:human true
                          :pred :Juan}
                    :subcat '()}}
   "Juana" {:synsem {:cat :noun
                    :propernoun true
                    :case :nom
                    :agr {:gender :fem
                          :person :3rd
                          :number :sing}
                     :sem {:human true
                           :pred :Juana}
                    :subcat '()}}
   "Juan y yo"
   [{:synsem {:cat :noun
              :pronoun true
              :case :nom
              :agr {:gender :masc
                    :person :1st
                    :number :plur}
              :sem {:human true
                    :pred :Juan-and-i}
              :subcat '()}}]                       
  
   "jugar" {}
  
   "la"
   (unify determiner
          {:synsem {:cat :det
                    :def :def
                    :gender :fem
                    :number :sing}})
   "lastimarse"
   (let [subject-semantics (atom {:human true})
         subject-agr (atom :top)]
     {:synsem {:cat :verb
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
   
   "lavar" {}
   "leer" {}
   
   "levantarse"
   (let [subject-semantics (atom {:human true})
         subject-agr (atom :top)]
     {:synsem {:cat :verb
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
   
   "limpiar" {}
   
   "llamarse"
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

   "llegar" {:synsem {:cat :verb
                      :sem {:pred :arrive}}}
   
   "llenar" {}
   
   "llevar" (let [shared-part-of-llevar
                  {:synsem {:cat :verb}}]
              [(unify shared-part-of-llevar
                      {:synsem {:sem {:pred :carry}}})
               (unify shared-part-of-llevar
                      {:synsem {:sem {:pred :bring}}})])
   
   "llorar" {}
   "manejar" {}
   
   "me" {:synsem {:cat :noun
                  :pronoun true
                  :reflexive true
                  :case pronoun-acc
                  :agr {:person :1st
                        :number :sing}
                  :subcat '()}}

   "mentir" {:espanol {:boot-stem "mient"}
             :synsem {:cat :verb
                      :sem {:pred :lie}}}
   
   "mirar" {}
   "mostrar" {:note "m-"
              :synsem {:cat :verb
                       :sem {:pred :show}}}
   
   "mujer"
   (unify agreement-noun
          common-noun
          feminine-noun
          {:synsem {:sem {:pred :donna
                          :human true}
                    :subcat {:1 {:cat :det
                                 :number :sing
                                 :def :def}}}})
   
   "nacer" {:espanol  {:present {:1sing "nazco"}}
            :synsem {:cat :verb
                      :sem {:pred :be-born}}}
   
   
   "nadar" {}
   "necesitar" {}
   
   "negar" {:espanol {:boot-stem "nieg"}
                :synsem {:cat :verb
                         :sem {:pred :deny}}}
   
   "nos" {:synsem {:cat :noun
                   :pronoun true
                   :reflexive true
                   :case pronoun-acc
                   :agr {:person :1st
                         :number :plur}
                   :sem {:pred :noi
                         :human true}
                   :subcat '()}}
   "nosotras"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :gender :fem
                   :pred :noi}
             :subcat '()}}
   "nosotros"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :gender :masc
                   :number :plur}
             :sem {:human true
                   :gender :masc
                   :pred :noi}
             :subcat '()}}   

   ;; 2nd person plural informal 'vosotros/as' (ES-only)
   
   "oir" {}
   "olvidar" {}
   "organizar" {}
   "pagar" {}
   "parar" {}
   "pedir" {}
   "peinarse" {}
   
   "os"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :reflexive true
             :case pronoun-acc
             :agr {:person :2nd
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}
   "pan"
   (unify agreement-noun
          common-noun
          masculine-noun
          {:synsem {:sem {:pred :bread}
                    :subcat {:1 {:cat :det
                                 :number :sing
                                 :def :def}}}})
                               
   "pensar" {:espanol {:boot-stem "piens"}
             :synsem {:cat :verb
                      :sem {:pred :think}}}
                    
   "perder" {:espanol {:boot-stem "pierd"}
                :synsem {:cat :verb
                         :sem {:pred :lose}}}
   
   "pintar" {:synsem {:cat :verb
                      :sem {:pred :paint}}}                
   
   "poder" 
   (let [shared-part-of-poder
         {:espanol {:boot-stem "pued"
                    :pret-stem "pud"}
          :synsem {:cat :verb}}]
     [(unify shared-part-of-poder
             {:synsem {:sem {:pred :can}}})
      (unify shared-part-of-poder
             {:synsem {:sem {:pred :may}}})
      (unify shared-part-of-poder
             {:synsem {:sem {:pred :be-able-to}}})])
   
   "poner" {:espanol {:futuro-stem "pondr"
                      :present {:1sing "pongo"
                                :2sing "pones"
                                :3sing "pone"
                                :1plur "ponemos"
                                :2plur "ponéis"
                                :3plur "ponen"}
                      :preterito-stem "pus"}
            :synsem {:cat :verb
                     :sem {:subj {:animate true}
                           :pred :put}}}
   
   "ponerse de pie" {}
   "preguntar" {}
   "preocuparse" {}
   "prestar" {}
   
   "quedarse" (let [subject-semantics (atom {:animate true})
                    subject-agr (atom :top)]
                {:synsem {:cat :verb
                          :sem {:pred :remain
                                :reflexive true
                                :subj subject-semantics
                                :obj subject-semantics}
                          :subcat {:1 {:agr subject-agr
                                       :sem subject-semantics}
                                   :2 {:agr subject-agr
                                       :pronoun true
                                       :reflexive true
                                       :sem subject-semantics}}}})
                                     
   "quejarse" {}
   
   "querer" (let [shared-part-of-querer
                  {:synsem {:cat :verb}
                   :espanol {:boot-stem "quier"
                             :preterito-stem "quis"
                             :futuro-stem "querr"}}]
              [(unify shared-part-of-querer
                      {:synsem {:sem {:pred :want}}})
               (unify shared-part-of-querer
                      {:synsem {:sem {:pred :love}}})])
                                     
   "reparar" {}
   "responder" {}
   "romper"  {}
  
   "saber" {}
   "sacar" {}
   
   "salir" {:espanol {:present {:1sing "salgo"}}
            :synsem {:cat :verb
                      :sem {:pred :go-out}}}
   
   "se"
   [;; 3rd singular or plural: 'él,ellas,..etc'
    {:synsem {:cat :noun
              :pronoun true
              :reflexive true
              :case pronoun-acc
              :agr {:person :3rd}
              :subcat '()}}

    ;; 2nd plural: 'ustedes'
    {:synsem {:cat :noun
              :pronoun true
              :reflexive true
              :case pronoun-acc
              :agr {:person :2nd
                    :number :plur}
              :subcat '()}}]

   "seguir" {}
   "sentarse" {}
   
   "sentir" {}
   "sentirse" {}
   "soñar" {}
   
   "te" {:synsem {:cat :noun
                  :pronoun true
                  :reflexive true
                  :case pronoun-acc
                  :agr {:person :2nd
                        :number :sing}
                  :subcat '()}}
   
   "tener" {:espanol {:futuro {:1sing "tendré"
                               :2sing "tendras"
                               :3sing "tendrà"
                               :1plur "tendremos"
                               :2plur "tendréis"
                               :3plur "tendràn"}
                      :condicional {:1sing "tendrìa"
                                    :2sing "tendrìas"
                                    :3sing "tendrìa"
                                    :1plur "tendrìamos"
                                    :2plur "tendrìais"
                                    :3plur "tendrìan"}
                      :preterito {:1sing "tuve"
                                    :2sing "tuviste"
                                    :3sing "tuvo"
                                    :1plur "tuvimos"
                                    :2plur "tuvisteis"
                                    :3plur "tuvieron"}
                      :present {:1sing "tengo"
                                :2sing "tienes"
                                :3sing "tiene"
                                :1plur "tenemos"
                                :2plur "tenéis"
                                :3plur "tienen"}
                      :preterito-stem "tuv" }
            :synsem {:cat :verb
                     :sem {:subj {:animate true}
                           :pred :have}}}
   
   
   "terminar" {}
   
   
   "tirar" [{:synsem {:cat :verb
                      :sem {:pred :throw-out}}}
            {:synsem {:cat :verb
                      :sem {:pred :throw}}}]
   "tocar" {}
   "tomar" {}
   "toser" {}
   "trabajar" {}
   "traducir" {}
   "traer" {}
   
   "tú" {:synsem {:cat :noun
                  :pronoun true
                  :case :nom
                  :agr {:person :2nd
                        :number :sing}
                  :sem {:human true
                        :pred :tu}
                  :subcat '()}}
   
   "usar" {}
   
   "ustedes"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}
   
   
   "valer" {}
   "vender" {}
   
   "venir" {:espanol {:futuro-stem "vendr"
                      :present {:1sing "vengo"
                                :2sing "vienes"
                                :3sing "viene"
                                :3plur "vienen"}
                      :preterito-stem "vin"}
            :synsem {:cat :verb
                     :sem {:subj {:animate true}
                           :pred :come}}}
   
   "ver" {:espanol  {:present {:1sing "veo"
                               :2plur "veis"}
                     :preterito {:1sing "vi"
                                 :3sing "vio"}}
          :synsem {:cat :verb
                   :sem {:pred :see}}}
   
   "vestirse" {}
   "viajar" {}
   "vivir" {}
   "volar" {}
   "volver" {}
   
   "vosotras"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :gender :fem
                   :pred :voi}
             :subcat '()}}

   "vosotros"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :2nd
                   :gender :masc
                   :number :plur}
             :sem {:human true
                   :gender :masc
                   :pred :voi}
             :subcat '()}}
   
   "yo"
   {:synsem {:cat :noun
             :pronoun true
             :case :nom
             :agr {:person :1st
                   :number :sing}
             :sem {:human true
                   :pred :I}
             :subcat '()}}})
  
;; see TODOs in lexiconfn/compile-lex (should be more of a pipeline as opposed to a
;; argument-position-sensitive function.
(def lexicon
  (-> (compile-lex lexicon-source morph/exception-generator morph/phonize)

      ;; if a non-auxiliary, non-reflexive verb has no :obj,
      ;; then its {:obj is :unspec}.
      (if-then {:synsem {:cat :verb
                         :aux false
                         :sem {:reflexive false
                               :obj :unspec}}}
               {:synsem {:sem {:obj :unspec}}})
      
      ;; make an intransitive version of every verb which has an
      ;; [:sem :obj] path.
      intransitivize
      
      ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
      transitivize

      ;; if verb has no :aux, it's {:aux false}
      (if-then {:synsem {:cat :verb
                         :aux false}}
               {:synsem {:aux false}})
      
      (if-then {:synsem {:cat :verb
                         :subcat {:2 {:reflexive false}}}}
               {:synsem {:subcat {:2 {:reflexive false}}}})
      
      (if-then {:synsem {:cat :noun
                         :pronoun true
                         :null-pronoun false}}
               {:synsem {:null-pronoun false}})
      
      (if-then {:synsem {:cat :verb
                         :subcat {:3 '()}}}
               {:synsem {:subcat {:3 '()}}})
      
      ;; Cleanup functions can go here. Number them for ease of reading.
      ;; 1. this filters out any verbs without an inflection: infinitive verbs
      ;; should have inflection ':infinitive', 
      ;; rather than not having any inflection.
      (map-function-on-map-vals 
       (fn [k vals]
         (filter #(or (not (= :verb (get-in % [:synsem :cat])))
                      (not (= :none (get-in % [:synsem :infl] :none))))
                 vals)))))

