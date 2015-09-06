(ns babel.espanol.lexicon
  (:require
   [babel.lexiconfn :refer (unify)]
   [babel.espanol.pos :refer :all]))

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
                          :sem {:pred :understand}}}
   
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
   
   "dormir" {:espanol {:boot-stem "duerm"
                       :pret-stem "durm"}
             :synsem {:cat :verb
                      :sem {:pred :dormire}}}
   "echar" {:synsem {:cat :verb
                     :sem {:pred :throw-out}}}
   
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
   
   "enseñar" [{:synsem {:cat :verb
                        :sem {:pred :show}}}
              {:synsem {:cat :verb
                        :sem {:pred :teach}}}]
   
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
   
   ;; TODO: handle syntax/semantics mismatch between Italian/Spanish and English.
                                        ;                     "gustar" {:synsem {:cat :verb
                                        ;                                         :sem {:pred :??}}}
   
   "hablar" [{:synsem {:cat :verb
                       :sem {:pred :speak
                             :subj {:human true}}}}
             {:synsem {:cat :verb
                       :sem {:pred :talk
                             :subj {:human true}}}}]
   
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
   
   
   "yo"       {:synsem {:cat :noun
                        :pronoun true
                        :case :nom
                        :agr {:gender :fem
                              :person :1st
                              :number :sing}
                        :sem {:human true
                              :pred :io}
                        :subcat '()}}
   
   })




