(ns babel.italiano.morphology.verbs
  (:refer-clojure :exclude [get-in])
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [babel.morphology :refer [compile-patterns do-replace-on group-by-two]]
   [clojure.string :as string]
   [dag_unify.core :refer [get-in unify]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])))

;; <future>
(let [regular-future-source
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/future.edn")
          slurp
          read-string)]
  (def regular-future-patterns
    (compile-patterns
     {:synsem {:infl :future}}
     regular-future-source))
  
  (defonce regular-future-generate-patterns
    (compile-patterns
     {:synsem {:infl :future}}
     regular-future-source))

  (defonce patterns-future-tense
    (compile-patterns
     {:synsem {:infl :future}}
     regular-future-source)))

(defn regular-future? [word]
  (and (= (get-in word [:infl]) :future)
       (get-in word [:italiano])))

(defn regular-future [word]
  (let [unifying-patterns
        (remove nil? (mapcat #(if (not (= :fail (unify (:u %) word)))
                                (:g %))
                             regular-future-generate-patterns))
        infinitive (get-in word [:italiano])]
    (first (remove nil? (do-replace-on infinitive unifying-patterns)))))
;; </future>

;; <imperfect>
(let [regular-imperfect-source
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/imperfetto.edn")
          slurp
          read-string)]
  (defonce regular-imperfect-generate-patterns
    (compile-patterns
     {:synsem {:infl :imperfect}}
     regular-imperfect-source))
  
  (defonce patterns-imperfect-past-tense
    (compile-patterns
     {:synsem {:infl :imperfect}}
     regular-imperfect-source)))

(defn regular-imperfect? [word]
  ;; regular imperfect sense
  (and (= (get-in word '(:infl)) :imperfect)
       (get-in word '(:italiano))))

(defn regular-imperfect [word]
  (let [unifying-patterns
        (remove nil? (mapcat #(if (not (= :fail (unify (:u %) word)))
                                (:g %))
                             regular-imperfect-generate-patterns))
        infinitive (get-in word [:italiano])]
    (first (remove nil? (do-replace-on infinitive unifying-patterns)))))
;; </imperfect>

;; <present>
(let [regular-present-source
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/present.edn")
          slurp
          read-string)]
  (defonce regular-present-generate-patterns
    (compile-patterns
     {:synsem {:infl :present}}
     regular-present-source))
  
  (defonce patterns-present-tense
    (compile-patterns
     {:synsem {:infl :present}}
     regular-present-source)))

;; </present>

;; <passato>
(let [regular-past-source
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/past.edn")
          slurp
          read-string)]
  (defonce regular-past-generate-patterns
    (compile-patterns
     {:synsem {:infl :past}}
     regular-past-source))

  (def patterns-past-tense
    (compile-patterns
     {:synsem {:infl :past}}
     regular-past-source)))

;; </passato>

(defonce patterns-present-tense
  (compile-patterns
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

(defonce patterns-conditional-tense
  (compile-patterns
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
(defonce patterns-present-subjunctive
  (compile-patterns
   {:synsem {:infl :subjunctive}}
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

(defonce patterns-gerund
 (compile-patterns
  {:synsem {:infl :participle}}
  [{:agr :top
    :p [#"(.*)ando$" "$1are"
        #"(.*)ando$" "$1arsi"]}]))

(defonce patterns
  (map (fn [each]
         ;; unify with {:synsem {:cat :verb}} for all rules:
         {:p (:p each)
          :u (unify (:u each)
                    {:synsem {:cat :verb}})
          :g (:g each)})
       
       (concat
        ;; if more are added in the future, please
        ;; preserve alphabetical ordering.
        patterns-conditional-tense
        patterns-future-tense
        patterns-gerund
        patterns-imperfect-past-tense
        patterns-past-tense
        patterns-present-subjunctive
        patterns-present-tense)))

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

(defn suffix-of [word]
  "compute the final character given a lexical entry and agreement info in :agr."
  (let [suffix (cond
                 
                 (and (= (get-in word '(:obj-agr :gender)) :fem)
                      (= (get-in word '(:obj-agr :number)) :sing))
                 "a"
                 
                 (and (= (get-in word '(:obj-agr :gender)) :fem)
                      (= (get-in word '(:obj-agr :number)) :plur))
                 "e"

                 (= (get-in word '(:obj-agr :number)) :plur)
                 "i"
                 
                 (and (= (get-in word '(:agr :gender)) :fem)
                      (= (get-in word '(:agr :number)) :sing)
                      (= (get-in word '(:essere)) true))
                 "a"
                 
                 (and (= (get-in word '(:agr :gender)) :fem)
                      (= (get-in word '(:agr :number)) :plur)
                      (= (get-in word '(:essere)) true))
                 "e"
                 
                 (and (= (get-in word '(:agr :number)) :plur)
                      (= (get-in word '(:essere)) true))
                 "i"
                 
                 true
                 "o"
                 
                 )]
    suffix))

(defn irregular-future? [word]
  (and
   (= (get-in word '(:infl)) :future)
   (map? (get-in word '(:future)))))

(defn irregular-future [word]
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

;; future: 2) future-stem specified
(defn regular-future-with-future-stem? [word]
  (and (= (get-in word '(:infl)) :future)
       (get-in word '(:future-stem))))

(defn regular-future-with-future-stem [word]
  (let [stem (get-in word '(:future-stem))
        number (get-in word [:agr :number])
        person (get-in word [:agr :person])
        patterns patterns-future-tense]
    (-> patterns
        (map (fn [pattern]
               (let [[from to] (nth (:g pattern))]
                 from)))
        (remove nil?))
    (cond
      (and (= person :1st) (= number :sing))
      (str stem "ò")
      
      (and (= person :2nd) (= number :sing))
      (str stem "ai")
      
      (and (= person :3rd) (= number :sing))
      (str stem "à")
      
      (and (= person :1st) (= number :plur))
      (str stem "emo")
      
      (and (= person :2nd) (= number :plur))
      (str stem "ete")
      
      (and (= person :3rd) (= number :plur))
      (str stem "anno"))))

(defn regular-conditional-with-future-stem? [word]
  ;; regular inflection of conditional with :future-stem
  (and (= (get-in word [:infl]) :conditional)
       (string? (get-in word '(:future-stem) :none))))

(defn regular-conditional-with-future-stem [word]
  (let [stem (get-in word '(:future-stem))
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))
        drop-e (get-in word '(:italiano :drop-e) false)]
    (cond
      (and (= person :1st) (= number :sing))
      (str stem "ei")
      
      (and (= person :2nd) (= number :sing))
      (str stem "esti")
      
      (and (= person :3rd) (= number :sing))
      (str stem "ebbe")
      
      (and (= person :1st) (= number :plur))
      (str stem "emmo")
      
      (and (= person :2nd) (= number :plur))
      (str stem "este")
      
      (and (= person :3rd) (= number :plur))
      (str stem "ebbero"))))

(defn regular-conditional? [word]
  ;; regular inflection of conditional without :future-stem
  (and (= (get-in word '(:infl)) :conditional)
       (get-in word '(:italiano))))

(defn regular-conditional [word]
  (let [infinitive (get-in word '(:italiano))
        ;; e.g.: lavarsi => lavare
        infinitive (if (re-find #"[aei]rsi$" infinitive)
                     (string/replace infinitive #"si$" "e")
                        infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))
        drop-e (get-in word '(:italiano :drop-e) false)
        stem (stem-for-future infinitive drop-e)]
    
    (cond
      (and (= person :1st) (= number :sing))
      (str stem "ei")
      
      (and (= person :2nd) (= number :sing))
      (str stem "esti")
      
      (and (= person :3rd) (= number :sing))
      (str stem "ebbe")
      
      (and (= person :1st) (= number :plur))
      (str stem "emmo")
      
      (and (= person :2nd) (= number :plur))
      (str stem "este")
      
      (and (= person :3rd) (= number :plur))
      (str stem "ebbero")
      
      :else
      (get-in word '(:italiano)))))

(defn irregular-imperfect-1sing? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :sing (get-in word '(:agr :number)))
   (= :1st (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :1sing)))))

(defn irregular-imperfect-1sing [word]
  (get-in word [:imperfect :1sing]))

(defn irregular-imperfect-2sing? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :sing (get-in word '(:agr :number)))
   (= :2nd (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :2sing)))))

(defn irregular-imperfect-2sing [word]
  (get-in word [:imperfect :2sing]))

(defn irregular-imperfect-3sing? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :sing (get-in word '(:agr :number)))
   (= :3rd (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :3sing)))))

(defn irregular-imperfect-3sing [word]
  (get-in word [:imperfect :3sing]))

(defn irregular-imperfect-1plur? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :plur (get-in word '(:agr :number)))
   (= :1st (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :1plur)))))

(defn irregular-imperfect-1plur [word]
  (get-in word [:imperfect :1plur]))

(defn irregular-imperfect-2plur? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :plur (get-in word '(:agr :number)))
   (= :2nd (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :2plur)))))

(defn irregular-imperfect-2plur [word]
  (get-in word [:imperfect :2plur]))

(defn irregular-imperfect-3plur? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :plur (get-in word '(:agr :number)))
   (= :3rd (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :3plur)))))

(defn irregular-imperfect-3plur [word]
  (get-in word [:imperfect :3plur]))

(defn passato-as-head? [word]
  ;; "fare [past]" + "bene" => "fatto bene"
  (and (= (get-in word '(:cat)) :verb)
       (= (get-in word '(:infl)) :past)
       (string? (get-in word '(:a :passato)))))

(defn passato-as-head [word]
  (str (get-in word '(:a :passato)) " "
       (get-in word '(:b))))

(defn irregular-passato? [word]
  ;; TODO: do not use brackets: if there's an error about there being
  ;; not enough information, throw an exception explicitly.
  ;; return the irregular form in square brackets, indicating that there's
  ;; not enough information to conjugate the verb.
  (and (= :past (get-in word '(:infl)))
       (get-in word '(:passato))
       (get-in word '(:essere) true)
       (or (= :notfound (get-in word '(:agr :number) :notfound))
           (= :top (get-in word '(:agr :number))))))

(defn irregular-passato [word]
  ;; not enough information.
  (log/warn (str "not enough agreement specified to conjugate: " (get-in word '(:passato)) " (irreg past)]"))
  (get-in word '(:passato)))

(defn passato-stem? [word]
  (and (= :past (get-in word '(:infl)))
       (get-in word '(:passato-stem))))

(defn passato-stem [word]
  ;; conjugate irregular passato: option 1) using :passato-stem
  (let [irregular-passato (get-in word '(:passato-stem))]
    (str irregular-passato (suffix-of word))))

(defn irregular-passato? [word]
  ;; conjugate irregular passato: option 2) using :passato
  (and (= :past (get-in word '(:infl)))
       (get-in word '(:passato))))

(defn irregular-passato [word]
  (let [irregular-passato (get-in word '(:passato))
        butlast (nth (re-find #"(.*).$" irregular-passato) 1)]
    (str butlast (suffix-of word))))

(defn regular-passato? [word]
  ;; conjugate regular passato
  (and (= :past (get-in word '(:infl)))
       (string? (get-in word '(:italiano)))))

(defn regular-passato [word]
  (let [infinitive (get-in word [:italiano]) ;; regular passato
        ;; e.g.: lavarsi => lavare
        infinitive (if (re-find #"[aei]rsi$" infinitive)
                     (string/replace infinitive #"si$" "e")
                     infinitive)
        
        are-type (try (re-find #"are$" infinitive)
                      (catch Exception e
                        (throw (Exception. (str "Can't regex-find on non-string: " infinitive)))))
        ere-type (re-find #"ere$" infinitive)
        ire-type (re-find #"ire$" infinitive)
        stem (string/replace infinitive #"[iae]re$" "")
        
        ;; for passato prossimo, the last char depends on gender and number, if an essere-verb.
        suffix (suffix-of word)]
    (cond
      ere-type
      (str stem "ut" suffix) ;; "uto","uti","uta" or "ute"
      
      are-type
      (str stem "at" suffix) ;; "ato","ati","ata", or "ate"
      
      (or are-type ire-type)
      (str stem "it" suffix) ;; "ito","iti","ita", or "ite"
      
      true
      (str "(regpast:TODO):" stem))))

(defn irregular-present-1sing? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :1st) (= number :sing)
         (string? (get-in word '(:present :1sing))))))

(defn irregular-present-1sing [word]
  (get-in word '(:present :1sing)))

(defn irregular-present-2sing? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :2nd) (= number :sing)
         (string? (get-in word '(:present :2sing))))))

(defn irregular-present-2sing [word]
  (get-in word '(:present :2sing)))

(defn irregular-present-3sing? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :3rd) (= number :sing)
         (string? (get-in word '(:present :3sing))))))

(defn irregular-present-3sing [word]
  (get-in word '(:present :3sing)))

(defn irregular-present-1plur? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :1st) (= number :plur)
         (string? (get-in word '(:present :1plur))))))

(defn irregular-present-1plur [word]
  (get-in word '(:present :1plur)))

(defn irregular-present-2plur? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :2nd) (= number :plur)
         (string? (get-in word '(:present :2plur))))))

(defn irregular-present-2plur [word]
  (get-in word '(:present :2plur)))

(defn irregular-present-3plur? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :3rd) (= number :plur)
         (string? (get-in word '(:present :3plur))))))

(defn irregular-present-3plur [word]
  (get-in word '(:present :3plur)))

(defn regular-present? [word]
  (and
   (= (get-in word '(:infl)) :present)
   (string? (get-in word '(:italiano)))))

(defn regular-present [word]
  ;; TODO: use (defn stem-analysis [word])
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
        stem (cond (and (get-in word [:boot-stem1])
                        (or (= (get-in word [:agr :number])
                               :sing)
                            (and (= (get-in word [:agr :person])
                                    :3rd)
                                 (= (get-in word [:agr :number])
                                    :plur))))
                   (get-in word [:boot-stem1])
                   true
                   (string/replace infinitive #"[iae]re$" ""))
        last-stem-char-is-i (re-find #"i[iae]re$" infinitive)
        last-stem-char-is-e (re-find #"e[iae]re$" infinitive)
        is-care-or-gare? (re-find #"[cg]are$" infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (cond
      
      (and (= person :1st) (= number :sing)
           (get-in word [:boot-stem1]))
      (str (get-in word [:boot-stem1]) "o")
      
      (and (= person :2nd) (= number :sing)
           (get-in word [:boot-stem1]))
      (str (get-in word [:boot-stem1]) "i")
      
      (and (= person :3rd) (= number :sing)
           (get-in word [:boot-stem1]))
      (str (get-in word [:boot-stem1]) "e")
      
      (and (= person :1st) (= number :sing))
      (str stem "o")
      
      (and (= person :2nd) (= number :sing)
           last-stem-char-is-i)
      ;; do not add 'i' at the end here to prevent double i:
      (str stem "")
      
      (and is-care-or-gare? 
           (= person :2nd) (= number :sing))
      (str stem "hi")
      
      (and (= person :2nd) (= number :sing))
      (str stem "i")
      
      (and (= person :3rd) (= number :sing) (or ire-type ere-type))
      (str stem "e")
      
      (and (= person :3rd) (= number :sing) are-type)
      (str stem "a")
      
      (and (= person :1st) (= number :plur)
           last-stem-char-is-i)
      (str stem "amo")
      
      (and is-care-or-gare?
           (= person :1st) (= number :plur))
      (str stem "hiamo")
      
      (and (= person :1st) (= number :plur))
      (str stem "iamo")
      
      (and (= person :2nd) (= number :plur) are-type)
      (str stem "ate")
      
      (and (= person :2nd) (= number :plur) ere-type)
      (str stem "ete")
      
      (and (= person :2nd) (= number :plur) ire-type)
      (str stem "ite")
      
      (and (= person :3rd) (= number :plur)
           (get-in word [:boot-stem1]))
      (str (get-in word [:boot-stem1]) "ono")
      
      (and (= person :3rd) (= number :plur)
           ire-type)
      (str stem "ono")
      
      (and (= person :3rd) (= number :plur)
           ere-type)
      (str stem "ono")
      
      (and (= person :3rd) (= number :plur))
      (str stem "ano")
      
      :else
      (str infinitive ))))

  ;; <irregular gerund inflection>
(defn irregular-gerund? [word]
  (and
   (= (get-in word [:infl]) :participle)
   (string? (get-in word [:italiano]))
   (string? (get-in word [:gerund]))))

(defn irregular-gerund [word]
  (get-in word [:gerund]))
;; </irregular gerund inflection>

;; <default gerund inflection>
(defn regular-gerund? [word]
  (and
   (= (get-in word [:infl]) :participle)
   (string? (get-in word [:italiano]))))

(defn regular-gerund [word]
  (let [stem-analysis (stem-analysis word)
        infinitive (:infinitive stem-analysis)
        stem (:stem stem-analysis)]
    (log/debug (str "cnonjugating present participle; analysis:" stem-analysis))
    (cond (= "are" (:are-type stem-analysis))
          (str stem "ando")
          (= "ere" (:ere-type stem-analysis))
          (str stem "endo")
          (= "ire" (:ire-type stem-analysis))
          (str stem "endo")
          true
          (do
            (log/warn (str "no specific conjugation found for word with stem-analysis:" stem-analysis " - returning infinitive"))
            infinitive))))
;; </default gerund inflection>

(defn conjugate [word]
  (cond
    (irregular-future? word)
    (irregular-future word)
    
    (regular-future-with-future-stem? word)
    (regular-future-with-future-stem word)
    
    (regular-future? word)
    (regular-future word)
    
    (regular-conditional-with-future-stem? word)
    (regular-conditional-with-future-stem word)
    
    (regular-conditional? word)
    (regular-conditional word)
    
    (irregular-imperfect-1sing? word)
    (irregular-imperfect-1sing word)
    
    (irregular-imperfect-2sing? word)
    (irregular-imperfect-2sing word)

    (irregular-imperfect-3sing? word)
    (irregular-imperfect-3sing word)
    
    (irregular-imperfect-1plur? word)
    (irregular-imperfect-1plur word)
    
    (irregular-imperfect-2plur? word)
    (irregular-imperfect-2plur word)
    
    (irregular-imperfect-3plur? word)
    (irregular-imperfect-3plur word)
    
    (regular-imperfect? word)
    (regular-imperfect word)
    
    (passato-as-head? word)
    (passato-as-head word)
    
    (irregular-passato? word)
    (irregular-passato word)
    
    (passato-stem? word)
    (passato-stem word)
    
    (irregular-passato? word)
    (irregular-passato word)
    
    (regular-passato? word)
    (regular-passato word)
    
    (irregular-present-1sing? word)
    (irregular-present-1sing word)
    
    (irregular-present-2sing? word)
    (irregular-present-2sing word)
    
    (irregular-present-3sing? word)
    (irregular-present-3sing word)
    
    (irregular-present-1plur? word)
    (irregular-present-1plur word)
    
    (irregular-present-2plur? word)
    (irregular-present-2plur word)
    
    (irregular-present-3plur? word)
    (irregular-present-3plur word)
    
    (regular-present? word)
    (regular-present word)
    
    (irregular-gerund? word)
    (irregular-gerund word)
    
    (regular-gerund? word)
    (regular-gerund word)

    ))

