(ns babel.italiano.morphology.verbs)

(def replace-patterns-conditional
  [])

(def replace-patterns-future
  [
   {:p [#"^([^ ]+)erò" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :future}}}

   ;; optional second r: berro -> bere
   {:p [#"^([^ ]+)er[r]?ò" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)erò" "$1iare"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)erai" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)er[r]?ai" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)erai" "$1iare"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)erà" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)er[r]?à" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)erà" "$1iare"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)eremo" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)er[r]?emo" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)eremo" "$1iare"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :future}}}
   
   {:p [#"^([^ ]+)erete" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)er[r]?ete" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)erete" "$1iare"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :future}}}
   
   {:p [#"^([^ ]+)eranno" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)er[r]?anno" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :future}}}

   {:p [#"^([^ ]+)eranno" "$1iare"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :future}}}

   ])

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
  [
   {:p [#"^([^' ]+)o$"         "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)o$"         "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)isco$"      "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)o$"         "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)o$"         "$1arsi"] ;; alzo -> alzarsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)o$"         "$1irsi"] ;; diverto -> divertirso
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ico$"       "$1ire"] ;; dico -> dire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   ;; 2nd sing
   {:p [#"^([^' ]+)i$"         "$1are"] ;; lavi -> lavare
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$"         "$1iare"] ;; studi -> studiare
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$"         "$1arsi"] ;; lavi -> lavarsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)cci$"       "$1cciare"] ;; abbracci -> abbracciare
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$"         "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$"         "$1ire"] ;; senti -> sentire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+c)hi$"       "$1are"] ;; cerchi -> cercare
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
        
   {:p [#"^([^' ]+)i$"         "$1iarsi"] ;; arrabi -> arrabiarsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)sci$"       "$1re"] ;; finisci -> finire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$"         "$1irsi"] ;; diverti -> divertirsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ici$"       "$1ire"] ;; dici -> dire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)hi$"        "$1are"] ;; pieghi -> piegare
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)a$"         "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)e$"         "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)e$"         "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)a$"         "$1arsi"] ;; prepara -> preperarsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)sce$"       "$1re"] ;; finisce -> finire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)te$"        "$1tirsi"] ;; diverte -> divertirsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ice$"      "$1ire"] ;; dice -> dire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   
   {:p [#"^([^' ]+)iamo$"      "$1are"]  ;; parliamo -> parlare
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)iamo$"      "$1iare"] ;; mangiamo -> mangiare
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$"      "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$"      "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+c)hiamo$"    "$1are"] ;; sprechiamo -> sprecare
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$"      "$1iarsi"] ;; arrabiamo -> arrabiarsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$"      "$1arsi"] ;; chiamiamo -> chiamarsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$"      "$1irsi"] ;; divertiamo -> divertirsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ciamo$"     "$1re"] ;; diciamo -> dire
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)hiamo$"     "$1are"] ;; pieghiamo -> piegare
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   ;; 2nd plur
   {:p [#"^([^' ]+)([aei])te$" "$1$2re"] ;; parlate -> parlare; leggere -> leggere
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)([aei])te$" "$1$2rsi"] ;; chiamate -> chiamarsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)ano$"       "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ono$"       "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ono$"       "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)ano$"       "$1arsi"] ;; alzano -> alzarsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)scono$"     "$1re"] ;; finiscono -> finire
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ono$"       "$1irsi"] ;; divertono -> divertirsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)cono$"      "$1re"] ;; dicono -> dire
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ono$"       "$1irsi"] ;; vestono -> vestirsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   ])

(def replace-patterns
  (concat
   replace-patterns-future
   replace-patterns-past-tense
   replace-patterns-present-tense))
