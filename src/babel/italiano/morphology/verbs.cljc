(ns babel.italiano.morphology.verbs
  (:require 
   [dag_unify.core :refer [unify]]))

(def replace-patterns-conditional
  ;; TODO
  [])

(defn group-by-two [remaining]
  (if (> (count remaining) 1)
    (cons
     [(nth remaining 0)
      (nth remaining 1)]
     (group-by-two (rest (rest remaining))))))

(defn expand-replace-patterns [unify-with patterns]
  (mapcat (fn [x]
            (map (fn [pair]
                   {:u (unify unify-with
                              (:agr x))
                    :p (:p pair)})
                 (map (fn [p]
                        {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                                           :person :1st}}}}}
                         :p p})
                      (group-by-two (:p x)))))
          patterns))

(def replace-patterns-imperfect-tense
  (expand-replace-patterns
   {:synsem {:infl :imperfect}}
   [
    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :1st}}}}}
     :p [
         #"(.*)avo$" "$1are"
         #"(.*)avo$" "$1arsi"
         #"(.*)evo$" "$1ere"
         #"(.*)evo$" "$1ersi"
         #"(.*)ivo$" "$1ire"
         #"(.*)ivo$" "$1irsi"
         ]
     }

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :2nd}}}}}
     :p [
         #"(.*)avi$" "$1are"
         #"(.*)avi$" "$1arsi"
         #"(.*)evi$" "$1ere"
         #"(.*)evi$" "$1ersi"
         #"(.*)ivi$" "$1ire"
         #"(.*)ivi$" "$1irsi"
         ]
     }

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :3rd}}}}}
     :p [
         #"(.*)ava$" "$1are"
         #"(.*)ava$" "$1arsi"
         #"(.*)eva$" "$1ere"
         #"(.*)eva$" "$1ersi"
         #"(.*)iva$" "$1ire"
         #"(.*)iva$" "$1irsi"
         ]
     }

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :1st}}}}}
     :p [
         #"(.*)avamo$" "$1are"
         #"(.*)avamo$" "$1arsi"
         #"(.*)evamo$" "$1ere"
         #"(.*)evamo$" "$1ersi"
         #"(.*)ivamo$" "$1ire"
         #"(.*)ivamo$" "$1irsi"
         ]

     }

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :2nd}}}}}
     :p [
         #"(.*)avate$" "$1are"
         #"(.*)avate$" "$1arsi"
         #"(.*)evate$" "$1ere"
         #"(.*)evate$" "$1ersi"
         #"(.*)ivate$" "$1ire"
         #"(.*)ivate$" "$1irsi"
         ]
     }

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :3rd}}}}}
     :p [
         #"(.*)avano$" "$1are"
         #"(.*)avano$" "$1arsi"
         #"(.*)evano$" "$1ere"
         #"(.*)evano$" "$1ersi"
         #"(.*)ivano$" "$1ire"
         #"(.*)ivano$" "$1irsi"
         ]
     }
    
    ]))

(def replace-patterns-future-tense
  (expand-replace-patterns
   {:synsem {:infl :future}}
   [{:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :1st}}}}}
     :p [
         #"^([^ ]+)erò"     "$1are"
         #"^([^ ]+)cherò"   "$1care" ;; cercherò -> cercare
         #"^([^ ]+)chò"     "$1care" ;; carichò -> caricare
         #"^([^ ]+)erò"     "$1arsi"
         #"^([^ ]+)er[r]?ò" "$1ere"
         #"^([^ ]+)erò"     "$1ersi"
         #"^([^ ]+)erò"     "$1iare"
         #"^([^ ]+)errò"    "$1ere"
         #"^([^ ]+)errò"    "$1enere" ;; otterrò -> ottenere
         #"^([^ ]+)irò"     "$1ire"
         #"^([^ ]+)irò"     "$1irsi"
         #"^([^ ]+)drò"     "$1dare"
         #"^([^ ]+)drò"     "$1dere"
         #"^([^ ]+)rò"      "$1re"
         #"^([^ ]+)rò"      "$1ere" ;; potrò -> potere
         #"^([^ ]+)trò"     "$1tere"
         #"^([^ ]+)vrò"     "$1vere"
         #"^([^ ]+)rrò"     "$1lere" ;; vorrò -> volere
         #"^([^ ]+)rrò"     "$1nire" ;; verrò -> venire
         #"^([^ ]+)gherò"   "$1gare" ;; piegherò -> piegare
         #"^([^ ]+)rrò"     "$1nere" ;; rimarrò -> rimanere
         ]}
           
    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :2nd}}}}}
     :p [
         #"^([^ ]+)ai"       "$1e" ;; farai -> fare
         #"^([^ ]+)erai"     "$1are"
         #"^([^ ]+)erai"     "$1arsi"
         #"([^ ]+)cherai"    "$1care" ;; cercherai -> cercare
         #"^([^ ]+)drai"     "$1dare"
         #"^([^ ]+)erai"     "$1ersi"
         #"^([^ ]+)errai"    "$1enere" ;; otterrai -> ottenere
         #"^([^ ]+)er[r]?ai" "$1ere"
         #"^([^ ]+)erai"     "$1iare"
         #"^([^ ]+)errai"    "$1edere" ;; verrà -> vedere
         #"^([^ ]+)gherai"   "$1gare" ;; piegherai -> piegare
         #"^([^ ]+)ichai"    "$1icare"
         #"^([^ ]+)irai"     "$1ire"
         #"^([^ ]+)irai"     "$1irsi"
         #"^([^ ]+)errai"    "$1enire" ;; sverrai -> svenire
         #"^([^ ]+)rai"      "$1ere" ;; potrai -> potere
         #"^([^ ]+)vrai"     "$1vere"
         #"^([^ ]+)rrai"     "$1lere"  ;; vorrai -> volere
         #"^([^ ]+)rrai"     "$1nere"  ;; rimarrai -> rimanere
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :3rd}}}}}
     :p [
         #"^([^ ]+)chà"     "$1care"
         #"^([^ ]+)cherà"   "$1care"
         #"^([^ ]+)drà"     "$1dare"
         #"^([^ ]+)vrà"     "$1vere"
         #"^([^ ]+)erà"     "$1are"
         #"^([^ ]+)erà"     "$1arsi"
         #"^([^ ]+)er[r]?à" "$1ere"
         #"^([^ ]+)erà"     "$1ersi"
         #"^([^ ]+)erà"     "$1iare"
         #"^([^ ]+)errà"    "$1edere" ;; verrà -> vedere
         #"^([^ ]+)errà"    "$1enere" ;; otterrà -> ottenere
         #"^([^ ]+)gherà"   "$1gare" ;; piegherà -> piegare
         #"^([^ ]+)irà"     "$1ire"
         #"^([^ ]+)irà"     "$1irsi"
         #"^([^ ]+)rà"      "$1re"
         #"^([^ ]+)errà"    "$1enire" ;; sverrà -> svenire
         #"^([^ ]+)rà"      "$1ere" ;; potrà -> potere
         #"^([^ ]+)rrà"     "$1lere"  ;; vorrà -> volere
         #"^([^ ]+)rrà"     "$1nere" ;; rimarrà -> rimanere
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :1st}}}}}
     :p [
         #"^([^ ]+)cheremo"   "$1care" ;; giocheremo -> giocare
         #"^([^ ]+)eramo"     "$1ere"
         #"^([^ ]+)eremo"     "$1are"
         #"^([^ ]+)erremo"    "$1enere" ;; otterremo -> ottenere
         #"([^ ]+)ichemo"     "$1icare" ;; carichemo -> caricare
         #"^([^ ]+)remo"      "$1are" ;; "andremo" -> "andare"
         #"^([^ ]+)remo"      "$1ere" ;; "avremo" -> "avere"
         #"^([^ ]+)rremo"     "$1nere" ;; "terremo" -> "tenere"
         #"^([^ ]+)emo"       "$1e" ;; "daremo" -> "dare"
         #"^([^ ]+)gheremo"   "$1gare" ;; piegherete -> piegare
         #"^([^ ]+)iremo"     "$1ire"
         #"^([^ ]+)iremo"     "$1irsi"
         #"^([^ ]+)erremo"    "$1edere" ;; verremo -> vedere
         #"^([^ ]+)erremo"    "$1enire" ;; sverremo -> svenire
         #"^([^ ]+)eremo"     "$1arsi"
         #"^([^ ]+)eremo"     "$1ersi"
         #"^([^ ]+)er[r]?emo" "$1ere"
         #"^([^ ]+)rremo"     "$1lere"  ;; vorremo -> volere
         #"^([^ ]+)eremo"     "$1iare"
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :2nd}}}}}
     :p [
         #"^([^ ]+)arete"     "$1are"
         #"^([^ ]+)chete"     "$1care" ;; carichete -> caricare
         #"^([^ ]+)cherete"   "$1care" ;; giocherete -> giocare
         #"^([^ ]+)drete"     "$1dare"
         #"^([^ ]+)erete"     "$1are"
         #"^([^ ]+)erete"     "$1arsi"
         #"^([^ ]+)erete"     "$1ersi"
         #"^([^ ]+)erete"     "$1irsi"
         #"^([^ ]+)gherete"   "$1gare" ;; piegherete -> piegare
         #"^([^ ]+)er[r]?ete" "$1ere"
         #"^([^ ]+)erete"     "$1iare"
         #"^([^ ]+)errete"    "$1enere" ;; otterrete -> ottenere
         #"^([^ ]+)irete"     "$1ire"
         #"^([^ ]+)irete"     "$1irsi"
         #"^([^ ]+)vrete"     "$1vere"
         #"^([^ ]+)rete"      "$1ere" ;; potrete -> potere
         #"^([^ ]+)rrete"     "$1lere"  ;; vorrete -> volere
         #"^([^ ]+)rrete"     "$1nere" ;; rimarrete -> rimanere
         #"^([^ ]+)rrete"     "$1nire" ;; verrete -> venire
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :3rd}}}}}
     :p [
         #"^([^ ]+)channo"     "$1care"
         #"^([^ ]+)cheranno"   "$1care"
         #"^([^ ]+)dranno"     "$1dare"
         #"^([^ ]+)dranno"     "$1edere" ;; vedranno -> vedere
         #"^([^ ]+)eranno"     "$1are"
         #"^([^ ]+)eranno"     "$1iare"
         #"^([^ ]+)eranno"     "$1arsi"
         #"^([^ ]+)eranno"     "$1ersi"
         #"^([^ ]+)erranno"    "$1enire"
         #"^([^ ]+)erranno"    "$1enere" ;; otterranno -> ottenere
         #"^([^ ]+)er[r]?anno" "$1ere"
         #"^([^ ]+)gheranno"   "$1gare" ;; piegherete -> piegare
         #"^([^ ]+)iranno"     "$1ire"
         #"^([^ ]+)iranno"     "$1irsi"
         #"^([^ ]+)ranno"      "$1re"
         #"^([^ ]+)ranno"      "$1ere" ;; potranno -> potere
         #"^([^ ]+)vranno"     "$1vere"
         #"^([^ ]+)rranno"     "$1lere"  ;; vorranno -> volere

         ]}]))

(def replace-patterns-past-tense
  [
   {:p [#"^([^ ]+)ata" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :gender :fem}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ata" "$1arsi"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :gender :fem}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ate" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ate" "$1arsi"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ati" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :gender :masc}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ati" "$1arsi"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :gender :masc}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ato" "$1are"]
    :u {:synsem {:infl :past}}}

   {:p [#"^([^ ]+)ato" "$1arsi"]
    :u {:synsem {:infl :past}}}

   ;; scese -> sceso -> scendere
   {:p [#"^([^ ]+)ese" "$1eso"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}
                 :infl :past}}}

   ;; scesi -> sceso -> scendere
   {:p [#"^([^ ]+)esi" "$1eso"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :gender :masc}}}
                 :infl :past}}}
   
   {:p [#"^([^ ]+)ita" "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :gender :fem}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ita" "$1irsi"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :gender :fem}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ite" "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ite" "$1irsi"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)iti" "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)iti" "$1irsi"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ito" "$1ire"]
    :u {:synsem {:infl :past}}}

   {:p [#"^([^ ]+)ito" "$1irsi"]
    :u {:synsem {:infl :past}}}
   
   {:p [#"^([^ ]+)uta" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:gender :fem
                                    :number :sing}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)uta" "$1ersi"]
    :u {:synsem {:subcat {:1 {:agr {:gender :fem
                                    :number :sing}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)uta" "$1uto"]
    :u {:synsem {:subcat {:1 {:agr {:gender :fem
                                    :number :fem}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ute" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:gender :fem
                                    :number :sing}}}
                 :infl :past}}}
   {:p [#"^([^ ]+)ute" "$1ersi"]
    :u {:synsem {:subcat {:1 {:agr {:gender :fem
                                    :number :sing}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ute" "$1irsi"]
    :u {:synsem {:subcat {:1 {:agr {:gender :fem
                                    :number :plur}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ute" "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)ute" "$1uto"]
    :u {:synsem {:subcat {:1 {:agr {:gender :fem
                                    :number :plur}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)uti" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)uti" "$1ersi"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)uti" "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur}}}
                 :infl :past}}}

   {:p [#"^([^ ]+)uto" "$1ere"]
    :u {:synsem {:infl :past}}}

   {:p [#"^([^ ]+)uto" "$1ersi"]
    :u {:synsem {:infl :past}}}

   ])

(def replace-patterns-present-tense
  (expand-replace-patterns
   {:synsem {:infl :present}}
   [{:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :1st}}}}}
     :p [
         #"^([^' ]+)o$"         "$1are"
         #"^([^' ]+)o$"         "$1ere"
         #"^([^' ]+)isco$"      "$1ire"
         #"^([^' ]+)o$"         "$1ire"
         #"^([^' ]+)o$"         "$1arsi" ;; alzo -> alzarsi
         #"^([^' ]+)o$"         "$1irsi" ;; diverto -> divertirso
         #"^([^' ]+)ico$"       "$1ire" ;; dico -> dire
         ]}
    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :2nd}}}}}
     :p [
         #"^([^' ]+)i$"         "$1are" ;; lavi -> lavare
         #"^([^' ]+)i$"         "$1iare" ;; studi -> studiare
         #"^([^' ]+)i$"         "$1arsi" ;; lavi -> lavarsi
         #"^([^' ]+)cci$"       "$1cciare" ;; abbracci -> abbracciare
         #"^([^' ]+)i$"         "$1ere"
         #"^([^' ]+)i$"         "$1ire" ;; senti -> sentire
         #"^([^' ]+c)hi$"       "$1are" ;; cerchi -> cercare
         #"^([^' ]+)i$"         "$1iarsi" ;; arrabi -> arrabiarsi
         #"^([^' ]+)sci$"       "$1re" ;; finisci -> finire
         #"^([^' ]+)i$"         "$1irsi" ;; diverti -> divertirsi
         #"^([^' ]+)ici$"       "$1ire" ;; dici -> dire
         #"^([^' ]+)hi$"        "$1are" ;; pieghi -> piegare
         ]}  

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :3rd}}}}}
     
     :p [
         #"^([^' ]+)a$"         "$1are"
         #"^([^' ]+)e$"         "$1ere"
         #"^([^' ]+)e$"         "$1ire"
         #"^([^' ]+)a$"         "$1arsi" ;; prepara -> preperarsi
         #"^([^' ]+)sce$"       "$1re" ;; finisce -> finire
         #"^([^' ]+)te$"        "$1tirsi" ;; diverte -> divertirsi
         #"^([^' ]+)ice$"       "$1ire" ;; dice -> dire
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :1st}}}}}
     :p [
         #"^([^' ]+)iamo$"      "$1are"  ;; parliamo -> parlare
         #"^([^' ]+)iamo$"      "$1iare" ;; mangiamo -> mangiare
         #"^([^' ]+)iamo$"      "$1ere"
         #"^([^' ]+)iamo$"      "$1ire"
         #"^([^' ]+c)hiamo$"    "$1are" ;; sprechiamo -> sprecare
         #"^([^' ]+)iamo$"      "$1iarsi" ;; arrabiamo -> arrabiarsi
         #"^([^' ]+)iamo$"      "$1arsi" ;; chiamiamo -> chiamarsi
         #"^([^' ]+)iamo$"      "$1irsi" ;; divertiamo -> divertirsi
         #"^([^' ]+)ciamo$"     "$1re" ;; diciamo -> dire
         #"^([^' ]+)hiamo$"     "$1are" ;; pieghiamo -> piegare
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :2nd}}}}}
     :p [
         #"^([^' ]+)([aei])te$" "$1$2re" ;; parlate -> parlare
         #"^([^' ]+)([aei])te$" "$1$2rsi" ;; chiamate -> chiamarsi
         ]}
    
    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :3rd}}}}}
     :p [
         #"^([^' ]+)ano$"       "$1are"
         #"^([^' ]+)ono$"       "$1ere"
         #"^([^' ]+)ono$"       "$1ire"
         #"^([^' ]+)ano$"       "$1arsi" ;; alzano -> alzarsi
         #"^([^' ]+)scono$"     "$1re" ;; finiscono -> finire
         #"^([^' ]+)ono$"       "$1irsi" ;; divertono -> divertirsi
         #"^([^' ]+)cono$"      "$1re" ;; dicono -> dire
         #"^([^' ]+)ono$"       "$1irsi" ;; vestono -> vestirsi
         ]}]))

(def replace-patterns
  (map (fn [each]   ;; unify with {:synsem {:cat :verb}} for all rules.
         {:p (:p each)
          :u (unify (:u each)
                    {:synsem {:cat :verb}})
          :g (:g each)})
       (concat
        replace-patterns-future-tense
        replace-patterns-imperfect-tense
        replace-patterns-past-tense
        replace-patterns-present-tense)))


  
