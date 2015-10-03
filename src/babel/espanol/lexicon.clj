(ns babel.espanol.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.lexiconfn :refer [compile-lex map-function-on-map-vals unify]]
   [babel.espanol.morphology :as morph]
   [babel.espanol.pos :refer :all]
   [dag-unify.core :refer [get-in]]))

(def lexicon-source 
  {"abandonar" {:synsem {:cat :verb
                         :sem {:pred :abandon}}}

   "acabar" {:synsem {:cat :verb
                      :sem {:pred :finish}}}

   "aceptar" {:synsem {:cat :verb
                       :sem {:pred :accept}}}

   "acompañar" {:synsem {:cat :verb
                         :sem {:pred :accompany}}}

   "anunciar" {:synsem {:cat :verb
                        :sem {:pred :announce}}}

   "apoyar" {:synsem {:cat :verb
                      :sem {:pred :support}}}

   "aprender" {:synsem {:cat :verb
                        :sem {:pred :imparare}}}
   
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
   
   "bajar" {:synsem {:cat :verb
                     :sem {:pred :lower}}}
   
   "cambiar" {:synsem {:cat :verb
                       :sem {:pred :cambiare}}}
   
   "cerrar" {:espanol {:boot-stem1 "cierr"
                        :boot-stem2 "cerr"}
             :synsem {:cat :verb
                      :sem {:pred :close}}}
   
   "comentar" {:synsem {:cat :verb
                        :sem {:pred :comment}}}
   
   "comer" [{:synsem {:cat :verb
                      :subcat {:2 {:cat :noun}}
                      :sem {:pred :mangiare
                            :obj {:edible true}}}}
            {:synsem {:cat :verb
                      :sem {:pred :mangiare}}}]
   
   "compartir" {:synsem {:cat :verb
                         :sem {:pred :share}}}
   
   "comprar" {:synsem {:cat :verb
                       :sem {:pred :comprare
                             :subj {:human true}
                             :obj {:buyable true}}}}
   
   "comprender" {:synsem {:cat :verb
                          :sem {:pred :understand-deeply}}}
   
   "conservar" [{:synsem {:cat :verb
                          :sem {:pred :conserve}}}
                {:synsem {:cat :verb
                          :sem {:pred :preserve}}}]
   
   "considerar" {:synsem {:cat :verb
                          :sem {:pred :consider}}}
   
   "contestar" {:synsem {:cat :verb
                         :sem {:pred :answer}}}
   
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
   
   
   "dar" {:espanol  {:present {:1sing "doy"
                               :2plur "dais"}}
          :synsem {:cat :verb
                   :sem {:pred :give}}}
   
   "deber" {:synsem {:cat :verb
                     :sem {:pred :have-to}}}
   
   "decidir" {:synsem {:cat :verb
                       :sem {:pred :decide}}}
   
   "dejar" {:synsem {:cat :verb
                     :sem {:pred :leave-behind}}}
   
   "desarrollar" {:synsem {:cat :verb
                           :sem {:pred :develop}}}
   
   "desear" {:synsem {:cat :verb
                      :sem {:pred :desire}}}
   
   "divertirse"
   (let [subject-semantics (ref {:animate true})
                    subject-agr (ref :top)]
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
             :sem {:human true
                   :gender :fem
                   :pred :lei}
             :subcat '()}}
   
   "ellos"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :masc
                   :number :plur}
             :sem {:human true
                   :gender :masc
                   :pred :loro}
             :subcat '()}}
   
   "ellas"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :gender :fem
                   :number :plur}
             :sem {:human true
                   :gender :fem
                   :pred :loro}
             :subcat '()}}
           
   "empezar" {:espanol {:boot-stem1 "empiez"
                        :boot-stem2 "empez"}
              :synsem {:cat :verb
                       :sem {:pred :begin}}}
   
   "enseñar" [{:synsem {:cat :verb
                        :sem {:pred :show}}}
              {:synsem {:cat :verb
                        :sem {:pred :teach}}}]
   
   "entender" {:espanol {:boot-stem1 "entiend"
                         :boot-stem2 "entend"}
               :synsem {:cat :verb
                        :sem {:pred :understand-simply}}}
   
   "entrar" {:synsem {:cat :verb
                      :sem {:pred :enter}}}
   
   "escapar" {:synsem {:cat :verb
                       :sem {:pred :escape}}}
   
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
   
   "expresar" {:synsem {:cat :verb
                        :sem {:pred :express}}}
   
   ;; TODO: fix up so that it does not need to be disabled.
;   "faltar" {:synsem {:cat :verb
;                      :sem {:pred :to-be-missing}}}
   
   "formar" {:synsem {:cat :verb
                      :sem {:pred :form}}}
   
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
   
   "hablar" [{:synsem {:cat :verb
                       :sem {:pred :speak
                             :subj {:human true}}}}
             {:synsem {:cat :verb
                       :sem {:pred :talk
                             :subj {:human true}}}}]
    
    "ir" {:espanol {:future-stem "ir"
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
                                 :3plur "iban"}
            
            
            :synsem {:cat :verb
                     
                     :sem {:subj {:animate true}
                           :pred :go}}}
                           
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
   
   "la"
   (unify determiner
          {:synsem {:cat :det
                    :def :def
                    :gender :fem
                    :number :sing}})
   
   "mujer"
   (unify agreement-noun
          common-noun
          feminine-noun
          {:synsem {:sem {:pred :donna
                          :human true}
                    :subcat {:1 {:cat :det
                                 :number :sing
                                 :def :def}}}})
   
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
   
   "pan"
   (unify agreement-noun
          common-noun
          masculine-noun
          {:synsem {:sem {:pred :pane
                          :edible true
                          :artifact true}
                    :subcat {:1 {:cat :det
                                 :number :sing
                                 :def :def}}}})
                               
   "pensar" {:espanol {:boot-stem1 "piens"
                       :boot-stem2 "pens"}
             :synsem {:cat :verb
                      :sem {:pred :think}}}
                    
   "pintar" {:synsem {:cat :verb
                      :sem {:pred :paint}}}                

   "quedarse" (let [subject-semantics (ref {:animate true})
                    subject-agr (ref :top)]
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
   "tirar" [{:synsem {:cat :verb
                      :sem {:pred :throw-out}}}
            {:synsem {:cat :verb
                      :sem {:pred :throw}}}]
   
   "tú" {:synsem {:cat :noun
                  :pronoun true
                  :case :nom
                  :agr {:person :2nd
                        :number :sing}
                  :sem {:human true
                        :pred :tu}
                  :subcat '()}}
   
   "ustedes"
   {:synsem {:cat cat-of-pronoun
             :pronoun true
             :case :nom
             :agr {:person :3rd
                   :number :plur}
             :sem {:human true
                   :pred :voi}
             :subcat '()}}
   
   
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
             :subcat '()}}
   })

;; see TODOs in lexiconfn/compile-lex (should be more of a pipeline as opposed to a argument-position-sensitive function.
(def lexicon
  (future (-> (compile-lex lexicon-source morph/exception-generator morph/phonize)

              ;; make an intransitive version of every verb which has an
              ;; [:sem :obj] path.
              intransitivize
              
              ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
              transitivize
              
              ;; Cleanup functions can go here. Number them for ease of reading.
              ;; 1. this filters out any verbs without an inflection: infinitive verbs should have inflection ':infinitive', 
              ;; rather than not having any inflection.
              (map-function-on-map-vals 
               (fn [k vals]
                 (filter #(or (not (= :verb (get-in % [:synsem :cat])))
                              (not (= :none (get-in % [:synsem :infl] :none))))
                         vals))))))
