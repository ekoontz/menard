(ns babel.italiano.morphology.verbs
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [babel.morphology :refer [expand-replace-patterns group-by-two]]
   [clojure.string :as string]
   [dag_unify.core :refer [unify]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])))

(defonce replace-patterns-gerund
 (expand-replace-patterns
  {:synsem {:infl :participle}}
  [{:agr :top
    :p [#"(.*)ando$" "$1are"
        #"(.*)ando$" "$1arsi"]}]))

(defonce replace-patterns-imperfect-past-tense
  (expand-replace-patterns
   {:synsem {:infl :imperfect}}
   [
    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :1st}}}}}
     :p [
         #"(.*)avo$"  "$1are"
         #"(.*)avo$"  "$1arsi"
         #"(.*)cevo$" "$1re"
         #"(.*)evo$"  "$1ere"
         #"(.*)evo$"  "$1ersi"
         #"(.*)ivo$"  "$1ire"
         #"(.*)ivo$"  "$1irsi"
         ]
     }

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :2nd}}}}}
     :p [
         #"(.*)avi$"  "$1are"
         #"(.*)avi$"  "$1arsi"
         #"(.*)cevi$" "$1re"
         #"(.*)evi$"  "$1ere"
         #"(.*)evi$"  "$1ersi"
         #"(.*)ivi$"  "$1ire"
         #"(.*)ivi$"  "$1irsi"
         ]
     }

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :3rd}}}}}
     :p [
         #"(.*)ava$"  "$1are"
         #"(.*)ava$"  "$1arsi"
         #"(.*)ceva$" "$1re"
         #"(.*)eva$"  "$1ere"
         #"(.*)eva$"  "$1ersi"
         #"(.*)iva$"  "$1ire"
         #"(.*)iva$"  "$1irsi"
         ]
     }

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :1st}}}}}
     :p [
         #"(.*)avamo$"  "$1are"
         #"(.*)avamo$"  "$1arsi"
         #"(.*)cevamo$" "$1re"
         #"(.*)evamo$"  "$1ere"
         #"(.*)evamo$"  "$1ersi"
         #"(.*)ivamo$"  "$1ire"
         #"(.*)ivamo$"  "$1irsi"
         ]

     }

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :2nd}}}}}
     :p [
         #"(.*)avate$"  "$1are"
         #"(.*)avate$"  "$1arsi"
         #"(.*)cevate$" "$1re"
         #"(.*)evate$"  "$1ere"
         #"(.*)evate$"  "$1ersi"
         #"(.*)ivate$"  "$1ire"
         #"(.*)ivate$"  "$1irsi"
         ]
     }

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :3rd}}}}}
     :p [
         #"(.*)avano$"  "$1are"
         #"(.*)avano$"  "$1arsi"
         #"(.*)cevano$" "$1re"
         #"(.*)evano$"  "$1ere"
         #"(.*)evano$"  "$1ersi"
         #"(.*)ivano$"  "$1ire"
         #"(.*)ivano$"  "$1irsi"
         ]
     }

    ]))

(defonce replace-patterns-future-tense
  (expand-replace-patterns
   {:synsem {:infl :future}}
   [{:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :1st}}}}}
     :p [
         #"(.*)erò"     "$1are"
         #"(.*)cherò"   "$1care" ;; cercherò -> cercare
         #"(.*)chò"     "$1care" ;; carichò -> caricare
         #"(.*)erò"     "$1arsi"
         #"(.*)er[r]?ò" "$1ere"
         #"(.*)erò"     "$1ersi"
         #"(.*)erò"     "$1iare"
         #"(.*)errò"    "$1ere"
         #"(.*)errò"    "$1enere" ;; otterrò -> ottenere
         #"(.*)irò"     "$1ire"
         #"(.*)irò"     "$1irsi"
         #"(.*)drò"     "$1dare"
         #"(.*)drò"     "$1dere"
         #"(.*)rò"      "$1re"
         #"(.*)rò"      "$1ere" ;; potrò -> potere
         #"(.*)trò"     "$1tere"
         #"(.*)vrò"     "$1vere"
         #"(.*)rrò"     "$1lere" ;; vorrò -> volere
         #"(.*)rrò"     "$1nire" ;; verrò -> venire
         #"(.*)gherò"   "$1gare" ;; piegherò -> piegare
         #"(.*)rrò"     "$1nere" ;; rimarrò -> rimanere
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :2nd}}}}}
     :p [
         #"(.*)ai"       "$1e" ;; farai -> fare
         #"(.*)erai"     "$1are"
         #"(.*)erai"     "$1arsi"
         #"(.*)cherai"    "$1care" ;; cercherai -> cercare
         #"(.*)drai"     "$1dare"
         #"(.*)erai"     "$1ersi"
         #"(.*)errai"    "$1enere" ;; otterrai -> ottenere
         #"(.*)er[r]?ai" "$1ere"
         #"(.*)erai"     "$1iare"
         #"(.*)errai"    "$1edere" ;; verrà -> vedere
         #"(.*)gherai"   "$1gare" ;; piegherai -> piegare
         #"(.*)ichai"    "$1icare"
         #"(.*)irai"     "$1ire"
         #"(.*)irai"     "$1irsi"
         #"(.*)errai"    "$1enire" ;; sverrai -> svenire
         #"(.*)rai"      "$1ere" ;; potrai -> potere
         #"(.*)vrai"     "$1vere"
         #"(.*)rrai"     "$1lere"  ;; vorrai -> volere
         #"(.*)rrai"     "$1nere"  ;; rimarrai -> rimanere
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :3rd}}}}}
     :p [
         #"(.*)chà"     "$1care"
         #"(.*)cherà"   "$1care"
         #"(.*)drà"     "$1dare"
         #"(.*)vrà"     "$1vere"
         #"(.*)erà"     "$1are"
         #"(.*)erà"     "$1arsi"
         #"(.*)er[r]?à" "$1ere"
         #"(.*)erà"     "$1ersi"
         #"(.*)erà"     "$1iare"
         #"(.*)errà"    "$1edere" ;; verrà -> vedere
         #"(.*)errà"    "$1enere" ;; otterrà -> ottenere
         #"(.*)gherà"   "$1gare" ;; piegherà -> piegare
         #"(.*)irà"     "$1ire"
         #"(.*)irà"     "$1irsi"
         #"(.*)rà"      "$1re"
         #"(.*)errà"    "$1enire" ;; sverrà -> svenire
         #"(.*)rà"      "$1ere" ;; potrà -> potere
         #"(.*)rrà"     "$1lere"  ;; vorrà -> volere
         #"(.*)rrà"     "$1nere" ;; rimarrà -> rimanere
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :1st}}}}}
     :p [
         #"(.*)cheremo"   "$1care" ;; giocheremo -> giocare
         #"(.*)eramo"     "$1ere"
         #"(.*)eremo"     "$1are"
         #"(.*)erremo"    "$1enere" ;; otterremo -> ottenere
         #"(.*)ichemo"    "$1icare" ;; carichemo -> caricare
         #"(.*)remo"      "$1are" ;; "andremo" -> "andare"
         #"(.*)remo"      "$1ere" ;; "avremo" -> "avere"
         #"(.*)rremo"     "$1nere" ;; "terremo" -> "tenere"
         #"(.*)emo"       "$1e" ;; "daremo" -> "dare"
         #"(.*)gheremo"   "$1gare" ;; piegherete -> piegare
         #"(.*)iremo"     "$1ire"
         #"(.*)iremo"     "$1irsi"
         #"(.*)erremo"    "$1edere" ;; verremo -> vedere
         #"(.*)erremo"    "$1enire" ;; sverremo -> svenire
         #"(.*)eremo"     "$1arsi"
         #"(.*)eremo"     "$1ersi"
         #"(.*)er[r]?emo" "$1ere"
         #"(.*)rremo"     "$1lere"  ;; vorremo -> volere
         #"(.*)eremo"     "$1iare"
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :2nd}}}}}
     :p [
         #"(.*)arete"     "$1are"
         #"(.*)chete"     "$1care" ;; carichete -> caricare
         #"(.*)cherete"   "$1care" ;; giocherete -> giocare
         #"(.*)drete"     "$1dare"
         #"(.*)erete"     "$1are"
         #"(.*)erete"     "$1arsi"
         #"(.*)erete"     "$1ersi"
         #"(.*)erete"     "$1irsi"
         #"(.*)gherete"   "$1gare" ;; piegherete -> piegare
         #"(.*)er[r]?ete" "$1ere"
         #"(.*)erete"     "$1iare"
         #"(.*)errete"    "$1enere" ;; otterrete -> ottenere
         #"(.*)irete"     "$1ire"
         #"(.*)irete"     "$1irsi"
         #"(.*)vrete"     "$1vere"
         #"(.*)rete"      "$1ere" ;; potrete -> potere
         #"(.*)rrete"     "$1lere"  ;; vorrete -> volere
         #"(.*)rrete"     "$1nere" ;; rimarrete -> rimanere
         #"(.*)rrete"     "$1nire" ;; verrete -> venire
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :3rd}}}}}
     :p [
         #"(.*)rranno"     "$1nere"
         #"(.*)channo"     "$1care"
         #"(.*)cheranno"   "$1care"
         #"(.*)dranno"     "$1dare"
         #"(.*)dranno"     "$1edere" ;; vedranno -> vedere
         #"(.*)eranno"     "$1are"
         #"(.*)eranno"     "$1iare"
         #"(.*)eranno"     "$1arsi"
         #"(.*)eranno"     "$1ersi"
         #"(.*)erranno"    "$1enire"
         #"(.*)erranno"    "$1enere" ;; otterranno -> ottenere
         #"(.*)er[r]?anno" "$1ere"
         #"(.*)gheranno"   "$1gare" ;; piegherete -> piegare
         #"(.*)iranno"     "$1ire"
         #"(.*)iranno"     "$1irsi"
         #"(.*)ranno"      "$1re"
         #"(.*)ranno"      "$1ere" ;; potranno -> potere
         #"(.*)vranno"     "$1vere"
         #"(.*)rranno"     "$1lere"  ;; vorranno -> volere

         ]}]))

(defonce replace-patterns-past-tense
  (concat
   (expand-replace-patterns
    {:synsem {:infl :past}}
    [
     {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                        :gender :fem}}}}}
      :p [
          #"(.*)esa"       "$1eso"
          #"(.*)([aiu])ta" "$1$2to"
          #"(.*)tita"      "$1tito"
          #"(.*)asta"      "$1asto"
          #"(.*)ata"       "$1are"
          #"(.*)ata"       "$1arsi"
          #"(.*)ita"       "$1ire"
          #"(.*)ita"       "$1irsi"
          #"(.*)uta"       "$1ere"
          #"(.*)uta"       "$1ersi"
          #"(.*)uta"       "$1uto"
          ]
      }

     {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                        :gender :fem}}}}}
      :p [
          #"(.*)ese"       "$1eso"
          #"(.*)([aiu])te" "$1$2to"
          #"(.*)tite"      "$1tito"
          #"(.*)aste"      "$1asto"
          #"(.*)ese"       "$1eso"           ;; scese -> sceso -> scendere
          #"(.*)ate"       "$1are"
          #"(.*)ate"       "$1arsi"
          #"(.*)ite"       "$1ire"
          #"(.*)ite"       "$1irsi"
          #"(.*)ute"       "$1ere"
          #"(.*)ute"       "$1ersi"
          #"(.*)ute"       "$1irsi"
          #"(.*)ute"       "$1ire"
          #"(.*)ute"       "$1uto"

          ]
      }

     {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                        :gender :masc}}}}}
      :p [
          #"(.*)esi"       "$1eso"
          #"(.*)([aiu])ti" "$1$2to"
          #"(.*)titi"      "$1tito"
          #"(.*)asti"      "$1asto"
          #"(.*)ati"       "$1are"
          #"(.*)ati"       "$1arsi"
          #"(.*)esi"       "$1eso"    ;; scesi -> sceso -> scendere
          ]
      }

     {:agr {:synsem {:subcat {:1 {:agr {:number :plur}}}}}
      :p [
          #"(.*)iti" "$1ire"
          #"(.*)iti" "$1irsi"
          #"(.*)uti" "$1ere"
          #"(.*)uti" "$1ersi"
          #"(.*)uti" "$1ire"
          ]}

     {:agr {:synsem {:essere false}}
      :p [
          #"(.*)ato" "$1are"
          #"(.*)ato" "$1arsi"
          #"(.*)ito" "$1ire"
          #"(.*)ito" "$1irsi"
          #"(.*)uto" "$1ere"
          #"(.*)uto" "$1ersi"
          ]}

     {:agr {:synsem {:essere true
                     :subcat {:1 {:agr {:number :sing
                                        :gender :masc}}}}}
      :p [
          #"(.*)ato" "$1are"
          #"(.*)ato" "$1arsi"
          #"(.*)ito" "$1ire"
          #"(.*)ito" "$1irsi"
          #"(.*)uto" "$1ere"
          #"(.*)uto" "$1ersi"
          ]}

     ])))

(defonce replace-patterns-present-tense
  (expand-replace-patterns
   {:synsem {:infl :present}}
   [{:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :1st}}}}}
     :p [
         #"(.*)o$"         "$1are"
         #"(.*)o$"         "$1ere"
         #"(.*)isco$"      "$1ire"
         #"(.*)o$"         "$1ire"
         #"(.*)o$"         "$1arsi" ;; alzo -> alzarsi
         #"(.*)o$"         "$1irsi" ;; diverto -> divertirso
         #"(.*)ico$"       "$1ire" ;; dico -> dire
         ]}
    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :2nd}}}}}
     :p [
         #"(.*)i$"         "$1are" ;; lavi -> lavare
         #"(.*)i$"         "$1iare" ;; studi -> studiare
         #"(.*)i$"         "$1arsi" ;; lavi -> lavarsi
         #"(.*)cci$"       "$1cciare" ;; abbracci -> abbracciare
         #"(.*)i$"         "$1ere"
         #"(.*)i$"         "$1ire" ;; senti -> sentire
         #"(.*c)hi$"       "$1are" ;; cerchi -> cercare
         #"(.*)i$"         "$1iarsi" ;; arrabi -> arrabiarsi
         #"(.*)sci$"       "$1re" ;; finisci -> finire
         #"(.*)i$"         "$1irsi" ;; diverti -> divertirsi
         #"(.*)ici$"       "$1ire" ;; dici -> dire
         #"(.*)hi$"        "$1are" ;; pieghi -> piegare
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :3rd}}}}}

     :p [
         #"(.*)a$"         "$1are"
         #"(.*)e$"         "$1ere"
         #"(.*)e$"         "$1ire"
         #"(.*)a$"         "$1arsi" ;; prepara -> preperarsi
         #"(.*)sce$"       "$1re" ;; finisce -> finire
         #"(.*)te$"        "$1tirsi" ;; diverte -> divertirsi
         #"(.*)ice$"       "$1ire" ;; dice -> dire
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :1st}}}}}
     :p [
         #"(.*)iamo$"      "$1are"  ;; parliamo -> parlare
         #"(.*)iamo$"      "$1iare" ;; mangiamo -> mangiare
         #"(.*)iamo$"      "$1ere"
         #"(.*)iamo$"      "$1ire"
         #"(.*c)hiamo$"    "$1are" ;; sprechiamo -> sprecare
         #"(.*)iamo$"      "$1iarsi" ;; arrabiamo -> arrabiarsi
         #"(.*)iamo$"      "$1arsi" ;; chiamiamo -> chiamarsi
         #"(.*)iamo$"      "$1irsi" ;; divertiamo -> divertirsi
         #"(.*)ciamo$"     "$1re" ;; diciamo -> dire
         #"(.*)hiamo$"     "$1are" ;; pieghiamo -> piegare
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :2nd}}}}}
     :p [
         #"(.*)([aei])te$" "$1$2re" ;; parlate -> parlare
         #"(.*)([aei])te$" "$1$2rsi" ;; chiamate -> chiamarsi
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :3rd}}}}}
     :p [
         #"(.*)ano$"       "$1are"
         #"(.*)ono$"       "$1ere"
         #"(.*)ono$"       "$1ire"
         #"(.*)ano$"       "$1arsi" ;; alzano -> alzarsi
         #"(.*)scono$"     "$1re" ;; finiscono -> finire
         #"(.*)ono$"       "$1irsi" ;; divertono -> divertirsi
         #"(.*)cono$"      "$1re" ;; dicono -> dire
         #"(.*)ono$"       "$1irsi" ;; vestono -> vestirsi
         ]}]))

(defonce replace-patterns-conditional-tense
  (expand-replace-patterns
   {:synsem {:infl :conditional}}
   [{:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :1st}}}}}
     :p [
         #"(.*)erei"     "$1are"
         #"(.*)cherei"   "$1care"   ;; cercherei -> cercare
         #"(.*)chei"     "$1care"   ;; carichei -> caricare
         #"(.*)erei"     "$1arsi"
         #"(.*)er[r]?ei" "$1ere"
         #"(.*)erei"     "$1ersi"
         #"(.*)erei"     "$1iare"
         #"(.*)errei"    "$1ere"
         #"(.*)errei"    "$1enere"  ;; otterrei -> ottenere
         #"(.*)irei"     "$1ire"
         #"(.*)irei"     "$1irsi"
         #"(.*)drei"     "$1dare"
         #"(.*)drei"     "$1dere"
         #"(.*)rei"      "$1re"
         #"(.*)rei"      "$1ere"    ;; potrei -> potere
         #"(.*)trei"     "$1tere"
         #"(.*)vrei"     "$1vere"
         #"(.*)rrei"     "$1lere"   ;; vorrei -> volere
         #"(.*)rrei"     "$1nire"   ;; verrei -> venire
         #"(.*)gherei"   "$1gare"   ;; piegherei -> piegare
         #"(.*)rrei"     "$1nere"   ;; rimarrei -> rimanere
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :2nd}}}}}
     :p [
         #"(.*)esti"       "$1e"     ;; faresti -> fare
         #"(.*)eresti"     "$1are"
         #"(.*)eresti"     "$1arsi"
         #"(.*)cheresti"   "$1care"  ;; cercheresti -> cercare
         #"(.*)dresti"     "$1dare"
         #"(.*)eresti"     "$1ersi"
         #"(.*)erresti"    "$1enere" ;; otterresti -> ottenere
         #"(.*)er[r]?esti" "$1ere"
         #"(.*)eresti"     "$1iare"
         #"(.*)erresti"    "$1edere" ;; verrà -> vedere
         #"(.*)gheresti"   "$1gare"  ;; piegheresti -> piegare
         #"(.*)ichesti"    "$1icare"
         #"(.*)iresti"     "$1ire"
         #"(.*)iresti"     "$1irsi"
         #"(.*)erresti"    "$1enire" ;; sverresti -> svenire
         #"(.*)resti"      "$1ere"   ;; potresti -> potere
         #"(.*)vresti"     "$1vere"
         #"(.*)rresti"     "$1lere"  ;; vorresti -> volere
         #"(.*)rresti"     "$1nere"  ;; rimarresti -> rimanere
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :3rd}}}}}
     :p [
         #"(.*)chebbe"     "$1care"
         #"(.*)cherebbe"   "$1care"
         #"(.*)drebbe"     "$1dare"
         #"(.*)vrebbe"     "$1vere"
         #"(.*)erebbe"     "$1are"
         #"(.*)erebbe"     "$1arsi"
         #"(.*)er[r]?ebbe" "$1ere"
         #"(.*)erebbe"     "$1ersi"
         #"(.*)erebbe"     "$1iare"
         #"(.*)errebbe"    "$1edere" ;; verrebbe -> vedere
         #"(.*)errebbe"    "$1enere" ;; otterrebbe -> ottenere
         #"(.*)gherebbe"   "$1gare"  ;; piegherebbe -> piegare
         #"(.*)irebbe"     "$1ire"
         #"(.*)irebbe"     "$1irsi"
         #"(.*)rebbe"      "$1re"
         #"(.*)errebbe"    "$1enire" ;; sverrebbe -> svenire
         #"(.*)rebbe"      "$1ere"   ;; potrebbe -> potere
         #"(.*)rrebbe"     "$1lere"  ;; vorrebbe -> volere
         #"(.*)rrebbe"     "$1nere"  ;; rimarrebbe -> rimanere
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :1st}}}}}
     :p [
         #"(.*)cheremmo"   "$1care"   ;; giocheremmo -> giocare
         #"(.*)eramo"     "$1ere"
         #"(.*)eremmo"     "$1are"
         #"(.*)erremmo"    "$1enere"  ;; otterremmo -> ottenere
         #"(.*)ichemmo"     "$1icare" ;; carichemmo -> caricare
         #"(.*)remmo"      "$1are"    ;; "andremmo" -> "andare"
         #"(.*)remmo"      "$1ere"    ;; "avremmo" -> "avere"
         #"(.*)rremmo"     "$1nere"   ;; "terremmo" -> "tenere"
         #"(.*)emmo"       "$1e"      ;; "daremmo" -> "dare"
         #"(.*)gheremmo"   "$1gare"   ;; piegherete -> piegare
         #"(.*)iremmo"     "$1ire"
         #"(.*)iremmo"     "$1irsi"
         #"(.*)erremmo"    "$1edere"  ;; verremmo -> vedere
         #"(.*)erremmo"    "$1enire"  ;; sverremmo -> svenire
         #"(.*)eremmo"     "$1arsi"
         #"(.*)eremmo"     "$1ersi"
         #"(.*)er[r]?emmo" "$1ere"
         #"(.*)rremmo"     "$1lere"   ;; vorremmo -> volere
         #"(.*)eremmo"     "$1iare"
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :2nd}}}}}
     :p [
         #"(.*)areste"     "$1are"
         #"(.*)cheste"     "$1care"  ;; caricheste -> caricare
         #"(.*)chereste"   "$1care"  ;; giochereste -> giocare
         #"(.*)dreste"     "$1dare"
         #"(.*)ereste"     "$1are"
         #"(.*)ereste"     "$1arsi"
         #"(.*)ereste"     "$1ersi"
         #"(.*)ereste"     "$1irsi"
         #"(.*)ghereste"   "$1gare"  ;; pieghereste -> piegare
         #"(.*)er[r]?este" "$1ere"
         #"(.*)ereste"     "$1iare"
         #"(.*)erreste"    "$1enere" ;; otterreste -> ottenere
         #"(.*)ireste"     "$1ire"
         #"(.*)ireste"     "$1irsi"
         #"(.*)vreste"     "$1vere"
         #"(.*)reste"      "$1ere"   ;; potreste -> potere
         #"(.*)rreste"     "$1lere"  ;; vorreste -> volere
         #"(.*)rreste"     "$1nere"  ;; rimarreste -> rimanere
         #"(.*)rreste"     "$1nire"  ;; verreste -> venire
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :3rd}}}}}
     :p [
         #"(.*)chebbero"     "$1care"
         #"(.*)cherebbero"   "$1care"
         #"(.*)drebbero"     "$1dare"
         #"(.*)drebbero"     "$1edere" ;; vedrebbero -> vedere
         #"(.*)erebbero"     "$1are"
         #"(.*)erebbero"     "$1iare"
         #"(.*)erebbero"     "$1arsi"
         #"(.*)erebbero"     "$1ersi"
         #"(.*)errebbero"    "$1enire"
         #"(.*)errebbero"    "$1enere" ;; otterrebbero -> ottenere
         #"(.*)er[r]?ebbero" "$1ere"
         #"(.*)gherebbero"   "$1gare"  ;; piegherete -> piegare
         #"(.*)irebbero"     "$1ire"
         #"(.*)irebbero"     "$1irsi"
         #"(.*)rebbero"      "$1re"
         #"(.*)rebbero"      "$1ere"   ;; potrebbero -> potere
         #"(.*)vrebbero"     "$1vere"
         #"(.*)rrebbero"     "$1nere"  ;; rimarreste -> rimanere
         #"(.*)rrebbero"     "$1lere"  ;; vorrebbero -> volere

         ]}]))
(defonce replace-patterns-present-subjunctive
  (expand-replace-patterns
   {:synsem {:infl :present}}
   [{:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :1st}}}}}
     :p [
         #"(.*)i$"         "$1are"
         #"(.*)a$"         "$1ere"
         #"(.*)isca$"      "$1ire"
         #"(.*)a$"         "$1ire"
         #"(.*)i$"         "$1arsi" ;; alzi -> alzarsi
         #"(.*)a$"         "$1irsi" ;; diverta -> divertirso
         #"(.*)ica$"       "$1ire"  ;; dica -> dire
         ]}
    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :2nd}}}}}
     :p [
         #"(.*)i$"         "$1are"     ;; lavi -> lavare
         #"(.*)i$"         "$1iare"    ;; studi -> studiare
         #"(.*)i$"         "$1arsi"    ;; lavi -> lavarsi
         #"(.*)cci$"       "$1cciare"  ;; abbracci -> abbracciare
         #"(.*)a$"         "$1ere"     ;; scriva -> scrivere
         #"(.*)a$"         "$1ire"     ;; senta -> sentire
         #"(.*c)hi$"       "$1are"     ;; cerchi -> cercare
         #"(.*)i$"         "$1iarsi"   ;; arrabbi -> arrabbiarsi
         #"(.*)sca$"       "$1re"      ;; finisca -> finire
         #"(.*)a$"         "$1irsi"    ;; diverta -> divertirsi
         #"(.*)ica$"       "$1ire"     ;; dica -> dire
         #"(.*)hi$"        "$1are"     ;; pieghi -> piegare
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :sing
                                       :person :3rd}}}}}

     :p [
         #"(.*)i$"         "$1are"
         #"(.*)a$"         "$1ere"
         #"(.*)a$"         "$1ire"
         #"(.*)i$"         "$1arsi"  ;; prepari -> preperarsi
         #"(.*)sca$"       "$1re"    ;; finisca -> finire
         #"(.*)ta$"        "$1tirsi" ;; diverta -> divertirsi
         #"(.*)ica$"       "$1ire"   ;; dica -> dire
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :1st}}}}}
     :p [
         #"(.*)iamo$"      "$1are"   ;; parliamo -> parlare
         #"(.*)iamo$"      "$1iare"  ;; mangiamo -> mangiare
         #"(.*)iamo$"      "$1ere"
         #"(.*)iamo$"      "$1ire"
         #"(.*c)hiamo$"    "$1are"   ;; sprechiamo -> sprecare
         #"(.*)iamo$"      "$1iarsi" ;; arrabiamo -> arrabiarsi
         #"(.*)iamo$"      "$1arsi"  ;; chiamiamo -> chiamarsi
         #"(.*)iamo$"      "$1irsi"  ;; divertiamo -> divertirsi
         #"(.*)ciamo$"     "$1re"    ;; diciamo -> dire
         #"(.*)hiamo$"     "$1are"   ;; pieghiamo -> piegare
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :2nd}}}}}
     :p [
         #"(.*)iate$"  "$1are"  ;; parliate -> parlare
         #"(.*)iate$"  "$1ere"  ;; scriviate -> scrivere
         #"(.*)iate$"  "$1ire"  ;; dormiate -> dormire
         #"(.*)iate$"  "$1arsi" ;; chiamate -> chiamarsi
         #"(.*)iate$"  "$1ersi" ;; mettiate -> metterrsi
         #"(.*)iate$"  "$1irsi" ;; divertiate -> divertirsi
         ]}

    {:agr {:synsem {:subcat {:1 {:agr {:number :plur
                                       :person :3rd}}}}}
     :p [
         #"(.*)ino$"       "$1are"
         #"(.*)ano$"       "$1ere"
         #"(.*)ano$"       "$1ire"
         #"(.*)ino$"       "$1arsi" ;; alzino -> alzarsi
         #"(.*)scano$"     "$1re"   ;; finiscano -> finire
         #"(.*)ano$"       "$1irsi" ;; divertano -> divertirsi
         #"(.*)cano$"      "$1re"   ;; dicano -> dire
         #"(.*)ano$"       "$1irsi" ;; vestano -> vestirsi
         ]}]))

(defonce replace-patterns
  (map (fn [each]   ;; unify with {:synsem {:cat :verb}} for all rules.
         {:p (:p each)
          :u (unify (:u each)
                    {:synsem {:cat :verb}})
          :g (:g each)})
       
       (concat
        ;; if more are added in the future, please
        ;; preserve alphabetical ordering.
        replace-patterns-conditional-tense
        replace-patterns-future-tense
        replace-patterns-gerund
        replace-patterns-imperfect-past-tense
        replace-patterns-past-tense
        replace-patterns-present-subjunctive
        replace-patterns-present-tense)))

(defonce exceptions-rules
  [;; 1. past-tense exceptions
   {:path [:italiano :passato]
    :label "past-tense exception"
    :merge-fn
    (fn [val]
      {:italiano {:infl :past
                  :italiano (get-in val [:italiano :passato] :nothing)}})}
   
   ;; 1.5 imperfect
   {:path [:italiano :imperfect :1sing]
    :label "imperfect 1sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:italiano :imperfect :2sing]
    :label "imperfect 2sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   
   {:path [:italiano :imperfect :3sing]
    :label "imperfect 3sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :3sing] :nothing)
                  :agr {:number :sing
                        :person :3rd}}})}
   
   {:path [:italiano :imperfect :1plur]
    :label "imperfect 1plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :1plur] :nothing)
                  :agr {:number :plur
                        :person :1st}}})}
   
   {:path [:italiano :imperfect :2plur]
    :label "imperfect 2plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :2plur] :nothing)
                  :agr {:number :plur
                        :person :2nd}}})}
   
   {:path [:italiano :imperfect :3plur]
    :label "imperfect 3plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :3plur] :nothing)
                  :agr {:number :plur
                        :person :3rd}}})}
   
   ;; 2. present-tense exceptions
   {:path [:italiano :present :1sing]
    :label "present 1sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:italiano :present :2sing]
    :label "present 2sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   
   {:path [:italiano :present :3sing]
    :label "present 3sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :3sing] :nothing)
                  :agr {:number :sing
                        :person :3rd}}})}
   
   {:path [:italiano :present :1plur]
    :label "present 1plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :1plur] :nothing)
                  :agr {:number :plur
                        :person :1st}}})}
   
   {:path [:italiano :present :2plur]
    :label "present 2plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :2plur] :nothing)
                  :agr {:number :plur
                        :person :2nd}}})}
   
   {:path [:italiano :present :3plur]
    :label "present 3plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :3plur] :nothing)
                  :agr {:number :plur
                        :person :3rd}}})}
   
   ;; 2.1. present tense boot-stem
   (let [surface-form (fn [val] (str (get-in val [:italiano :boot-stem1]) "o"))]
     {:path [:italiano :boot-stem1]
      :label "present boot-stem1: o"
      :surface-form surface-form
      :merge-fn
      (fn [val]
        {:italiano {:infl :present
                    :italiano (surface-form val)
                    :agr {:number :sing
                          :person :1st}}})})
   
   (let [surface-form (fn [val] (str (get-in val [:italiano :boot-stem1]) "i"))]
     {:path [:italiano :boot-stem1]
      :label "present boot-stem1: i"
      :surface-form surface-form
      :merge-fn
      (fn [val]
        {:italiano {:infl :present
                    :italiano (surface-form val)
                    :agr {:number :sing
                          :person :2nd}}})})
   
   (let [surface-form (fn [val] (str (get-in val [:italiano :boot-stem1]) "e"))]
     {:path [:italiano :boot-stem1]
      :label "present boot-stem1: e"
      :surface-form surface-form
      :merge-fn
      (fn [val]
        {:italiano {:infl :present
                    :italiano (surface-form val)
                    :agr {:number :sing
                          :person :3rd}}})})
   
   (let [surface-form (fn [val] (str (get-in val [:italiano :boot-stem1]) "ono"))]
     {:path [:italiano :boot-stem1]
      :label "present boot-stem1: ono"
      :surface-form surface-form
      :merge-fn
      (fn [val]
        {:italiano {:infl :present
                    :italiano (surface-form val)
                    :agr {:number :plur
                          :person :3rd}}})})
   
   ;; 3. future-tense exceptions
   {:path [:italiano :future :1sing]
    :label "future 1sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :future
                  :italiano (get-in val [:italiano :future :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:italiano :future :2sing]
    :label "future 2sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :future
                  :italiano (get-in val [:italiano :future :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   {:path [:italiano :future :3sing]
    :label "future 3sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :future
                  :italiano (get-in val [:italiano :future :3sing] :nothing)
                                                   :agr {:number :sing
                                                         :person :3rd}}})}
                                    {:path [:italiano :future :1plur]
                                     :label "future 1plur"
                                     :merge-fn
                                     (fn [val]
                                       {:italiano {:infl :future
                                                   :italiano (get-in val [:italiano :future :1plur] :nothing)
                                                   :agr {:number :plur
                                                         :person :1st}}})}
                                    {:path [:italiano :future :2plur]
                                     :label "future 2plur"
                                     :merge-fn
                                     (fn [val]
                                       {:italiano {:infl :future
                                                   :italiano (get-in val [:italiano :future :2plur] :nothing)
                                                   :agr {:number :plur
                                                         :person :2nd}}})}
                                    {:path [:italiano :future :3plur]
                                     :label "future 3plur"
                                     :merge-fn
                                     (fn [val]
                                       {:italiano {:infl :future
                                                   :italiano (get-in val [:italiano :future :3plur] :nothing)
                                                   :agr {:number :plur
                                                         :person :3rd}}})}
                                    
   ;; 4. conditional-tense exceptions
   {:path [:italiano :conditional :1sing]
    :label "conditional 1sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:italiano :conditional :2sing]
    :label "conditional 2sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   {:path [:italiano :conditional :3sing]
    :label "conditional 3sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :3sing] :nothing)
                  :agr {:number :sing
                        :person :3rd}}})}
   {:path [:italiano :conditional :1plur]
    :label "conditional 1plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :1plur] :nothing)
                  :agr {:number :plur
                        :person :1st}}})}
   {:path [:italiano :conditional :2plur]
    :label "conditional 2plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :2plur] :nothing)
                  :agr {:number :plur
                        :person :2nd}}})}
   {:path [:italiano :conditional :3plur]
    :label "conditional 3plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :3plur] :nothing)
                  :agr {:number :plur
                        :person :3rd}}})}])

(defn stem-for-future [infinitive drop-e]
  "turn an infinitive form into a stem that can be conjugated in the future tense."

  ;; e.g.: lavarsi => lavare
  (let [infinitive (if (re-find #"[aei]rsi$" infinitive)
                     (string/replace infinitive #"si$" "e")
                     infinitive)]
    (cond
     (re-find #"giare$" infinitive)
     (string/replace infinitive #"giare$" "ger")

     (re-find #"ciare$" infinitive)
     (string/replace infinitive #"ciare$" "cer")

     (re-find #"gare$" infinitive)
     (string/replace infinitive #"gare$" "gher")

     (re-find #"care$" infinitive)
     (string/replace infinitive #"care$" "cher")

     (and
      (= true drop-e)
      (re-find #"are$" infinitive))
     (string/replace infinitive #"are$" "r")

     (re-find #"are$" infinitive)
     (string/replace infinitive #"are$" "er")

     (and
      (= true drop-e)
      (re-find #"ere$" infinitive))
     (string/replace infinitive #"ere$" "r")

     (re-find #"ere$" infinitive)
     (string/replace infinitive #"ere$" "er")

     (re-find #"ire$" infinitive)
     (string/replace infinitive #"ire$" "ir")

     true
     infinitive)))

(defn stem-for-imperfect [infinitive]
  "_infinitive_ should be a string (italian verb infinitive form)"
  ;; e.g.: lavarsi => lavare
  (let [infinitive (if (re-find #"[aei]rsi$" infinitive)
                     (string/replace infinitive #"si$" "e")
                     infinitive)]
    (cond
     (re-find #"re$" infinitive)
     (string/replace infinitive #"re$" "")
     true
     infinitive)))

(defn stem-analysis [word]
  (let [infinitive (if (get-in word [:infinitive]) ;; regular present tense
                     (get-in word [:infinitive])
                     (get-in word [:italiano]))
        ;; e.g.: lavarsi => lavare
        infinitive (if (re-find #"[aei]rsi$" infinitive)
                     (string/replace infinitive #"si$" "e")
                     infinitive)
        are-type (try (re-find #"are$" infinitive)
                      (catch Exception e
                        (throw (Exception. (str "Can't regex-find on non-string: " infinitive " from word: " word)))))
        ere-type (re-find #"ere$" infinitive)
        ire-type (re-find #"ire$" infinitive)
        boot-stem (cond (and (get-in word [:boot-stem1])
                             (or (= (get-in word [:agr :number])
                                    :sing)
                                 (and (= (get-in word [:agr :person])
                                         :3rd)
                                      (= (get-in word [:agr :number])
                                         :plur))))
                        (get-in word [:boot-stem1])
                        true
                        (string/replace infinitive #"[iae]re$" ""))
        
        ;; stem is stem without regard to :boot-stem1. It is
        ;; used for gerunds, e.g.: fornire -> 'io fornisco'
        ;; but 'io fornendo', not 'io forniscendo'.
        stem (string/replace infinitive #"[iae]re$" "")

        last-stem-char-is-i (re-find #"i[iae]re$" infinitive)
        last-stem-char-is-e (re-find #"e[iae]re$" infinitive)
        is-care-or-gare? (re-find #"[cg]are$" infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    {:infinitive infinitive
     :are-type are-type
     :ere-type ere-type
     :ire-type ire-type
     :boot-stem boot-stem
     :stem stem
     :last-stem-char-is-i last-stem-char-is-i
     :last-stem-char-is-e last-stem-char-is-e
     :is-care-or-gare? is-care-or-gare?
     :person person
     :number number}))

(defn handle1? [word]
  ;; future 1) irregular
  (and
   (= (get-in word '(:infl)) :future)
   (map? (get-in word '(:future)))))

(defn handle1 [word]
  (let [infinitive (get-in word '(:italiano)) ;; future irregular
        ;; e.g.: lavarsi => lavare
        infinitive (if (re-find #"[aei]rsi$" infinitive)
                     (string/replace infinitive #"si$" "e")
                     infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (cond
      (and (= person :1st) (= number :sing))
      (get-in word '(:future :1sing))
      (and (= person :2nd) (= number :sing))
      (get-in word '(:future :2sing))
      (and (= person :3rd) (= number :sing))
      (get-in word '(:future :3sing))
      (and (= person :1st) (= number :plur))
      (get-in word '(:future :1plur))
      (and (= person :2nd) (= number :plur))
      (get-in word '(:future :2plur))
      (and (= person :3rd) (= number :plur))
      (get-in word '(:future :3plur))
      
      (and (= (get-in word '(:infl)) :future)
           (string? (get-in word '(:italiano))))
      (str (get-in word '(:italiano)) " (future)")
      
      true ;; failthrough: should usually not get here:
      ;; TODO: describe when it might be ok, i.e. why log/warn not log/error.
      (do (log/warn (str "get-string-1 could not match: " word))
          word))))

