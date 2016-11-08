(ns babel.francais.morphology.verbs
  (:refer-clojure :exclude [future get-in resolve])
  (:require
   [babel.francais.morphology.nouns :as nouns]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unifyc)]))

;; TODO: convert to (babel.morphology/conjugation) (see babel.latin.morphology for an example of how to use conjugation)
(def present-nonreflexive-er-verb
  [
   {:p [#"^([^' ]+)e$"         "$1er"]
    :g [#"^([^' ]+)er$"        "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)es$"        "$1er"]
    :g [#"^([^' ]+)er$"        "$1es"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)e$"         "$1er"]
    :g [#"^([^' ]+)er$"        "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   
   {:p [#"^([^' ]+)([^e])ons$" "$1$2er"]
    :g [#"^([^' ]+)([^g])er$"  "$1$2ons"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)(g)eons$"   "$1$2er"]
    :g [#"^([^' ]+)(g)er$"     "$1$2eons"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)ez$"        "$1er"]
    :g [#"^([^' ]+)er$"        "$1ez"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)ent$"       "$1er"]
    :g [#"^([^' ]+)er$"        "$1ent"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   ])

(def present-nonreflexive-ir-verb
  [
   {:p [#"^([^' ]+)is$"         "$1ir"]
    :g [#"^([^' ]+)ir$"         "$1is"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)it$"        "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1it"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)e$"         "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   
   {:p [#"^([^' ]+)issons$"    "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1issons"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)issez$"     "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1issez"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)issent$"    "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1issent"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   ])

(def present-nonreflexive-re-verb
  [
   {:p [#"^([^' ]+)$"          "$1re"]
    :g [#"^([^' ]+)re$"        "$1"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)s$"        "$1re"]
    :g [#"^([^' ]+)re$"       "$1s"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)$"         "$1re"]
    :g [#"^([^' ]+)re$"       "$1"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}

   ;; e.g. apprendre => apprenons
   {:p [#"^([^' ]+)ons$"      "$1dre"]
    :g [#"^([^' ]+)dre$"      "$1ons"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)ez$"       "$1dre"]
    :g [#"^([^' ]+)dre$"      "$1ez"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)nent$"     "$1dre"]
    :g [#"^([^' ]+)dre$"      "$1nent"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
])

(def present-reflexive-re-verb
  [
   {:p [#"^([^' ]+)s$"       "se $1dre"]
    :g [#"^se ([^' ]+)dre$"  "$1s"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)s$"       "se $1dre"]
    :g [#"^se ([^' ]+)dre$"  "$1s"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)t$"       "se $1dre"]
    :g [#"^se ([^' ]+)dre$"  "$1t"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}

   ;; e.g. se plaindre => se plaindrons
   {:p [#"^([^' ]+)gnons$"   "se $1ndre"]
    :g [#"^se ([^' ]+)ndre$" "se $1gnons"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)gniez$"   "se $1ndre"]
    :g [#"^se ([^' ]+)ndre$" "$1gnez"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)gnent$"   "se $1ndre"]
    :g [#"^se ([^' ]+)ndre$" "$1gnent"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
])

(def present-reflexive
  ;; reflexive present -er and -ir type verbs
  [
   {:comment "present reflexive 1st person singular, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)e$"    "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}

   {:comment "present reflexive 2nd person singular, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)es$"   "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1es"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:comment "present reflexive 3rd person singular, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)e$"    "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}

   {:comment "present reflexive 1st person plural, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)ons$"  "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1ons"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:comment "present reflexive 2nd person plural, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)ez$"   "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1ez"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:comment "present reflexive 3rd person plural, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)ent$"  "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1ent"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
  
   {:comment "present reflexive 1st person singular, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)e$"   "se $1er"]
    :g [#"^se ([^aeéiou]\S+)er$"   "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}

   {:comment "present reflexive 2nd person singular, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)es$"  "se $1er"]
    :g [#"^se ([^aeéiou]\S+)[ie]r$"     "$1es"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
  
   {:comment "present reflexive 3rd person singular, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)e$"   "se $1er"]
    :g [#"^se ([^aeéiou]\S+)[ie]r$"     "$1es"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}

   {:comment "present reflexive 1st person plural, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)ons$" "s'$1er"]
    :g [#"^se ([^aeéiou]\S+)[ie]r$"     "$1ons"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:comment "present reflexive 2nd person plural, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)ez$"  "s'$1er"]
    :g [#"^se ([^aeéiou]\S+)[ie]r$"     "$1ez"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:comment "present reflexive 3rd person plural, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)ent$" "s'$1er"]
    :g [#"^se ([^aeéiou]\S+)[ie]r$"     "$1ent"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}])

(def past-reflexive
  [
   {:comment "past participle reflexive singular masculine -er where stem begins with a vowel"
    :p [#"^([aeéiou]\S+)é$"    "s'$1er"]
    :g [#"^s'([aeéiou]\S+)er$" "$1é"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :sing
                                    :gender :masc}}}}}}

   {:comment "past participle reflexive plural masculine -er where stem begins with a vowel"
    :p [#"^([aeéiou]\S+)és$"   "s'$1er"]
    :g [#"^s'([aeéiou]\S+)er$" "$1és"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :plur
                                    :gender :masc}}}}}}

   {:comment "past participle reflexive singular feminine -er where stem begins with a vowel"
    :p [#"^([aeéiou]\S+)ée$"   "s'$1er"]
    :g [#"^s'([aeéiou]\S+)er$" "$1ée"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :sing
                                    :gender :fem}}}}}}

   {:comment "past participle reflexive plural feminine -er where stem begins with a vowel"
    :p [#"^([aeéiou]\S+)ées$"  "s'$1er"]
    :g [#"^s'([aeéiou]\S+)er$" "$1ées"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}}}}

   {:comment "past participle reflexive singular masculine -er where stem does not begin with a vowel"
    :p [#"^([^aeéiou]\S+)é$"   "se $1er"]
    :g [#"^se (\S+)er$"        "$1é"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :sing
                                    :gender :masc}}}}}}

   {:comment "past participle reflexive plural masculine -er where stem does not begin with a vowel"
    :p [#"^([^aeéiou]\S+)és$"  "se $1er"]
    :g [#"^se (\S+)er$"        "$1és"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :plur
                                    :gender :masc}}}}}}

   {:comment "past participle reflexive singular feminine -er where stem does not begin with a vowel"
    :p [#"^([^aeéiou]\S+)ée$"    "se $1er"]
    :g [#"^se ([^aeéiou]\S+)er$" "$1é"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :sing
                                    :gender :fem}}}}}}

   {:comment "past participle reflexive plural feminine -er where stem does not begin with a vowel"
    :p [#"^([^aeéiou]\S+)ées$"   "se $1er"]
    :g [#"^se ([^aeéiou]\S+)er$" "$1ées"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}}}}
   ])

(def past-reflexive-re-verb
  [
   {:p [#"^([^' ]+)t$"       "se $1dre"]
    :g [#"^se ([^' ]+)dre$"  "$1t"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :past-p}}}

   {:p [#"^([^' ]+)t$"       "se $1dre"]
    :g [#"^se ([^' ]+)dre$"  "$1t"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :past-p}}}

   {:p [#"^([^' ]+)ts$"      "se $1dre"]
    :g [#"^se ([^' ]+)dre$"  "$1ts"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :past-p}}}

   ;; e.g. se plaindre => se plaindrons
   {:p [#"^([^' ]+)ts$"      "se $1dre"]
    :g [#"^se ([^' ]+)ndre$" "se $1ts"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :past-p}}}

   {:p [#"^([^' ]+)ts$"   "se $1ndre"]
    :g [#"^se ([^' ]+)ndre$" "$1ts"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :past-p}}}

   {:p [#"^([^' ]+)ts$"   "se $1ndre"]
    :g [#"^se ([^' ]+)ndre$" "$1ts"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :past-p}}}
])

(def past
  [
   {:comment "past participle non-reflexive singular -er; essere=true"
    :p [#"^(\S+)é$"            "$1er"]
    :g [#"^([^' ]+)er$"        "$1é"]
    :u {:synsem {:infl :past-p
                 :essere true
                 :subcat {:1 {:agr {:number :sing}}}}}}

   {:comment "past participle non-reflexive singular -er; essere=false"
    :p [#"^(\S+)é$"            "$1er"]
    :g [#"^([^' ]+)er$"        "$1é"]
    :u {:synsem {:infl :past-p
                 :essere false}}}

   {:comment "past participle non-reflexive -re"
    :p [#"^(\S+)u$"            "$1re"]
    :g [#"^([^' ]+)re$"        "$1u"]
    :u {:synsem {:infl :past-p}}}

   {:comment "past participle non-reflexive -ir"
    :p [#"^(\S+)i$"            "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1i"]
    :u {:synsem {:infl :past-p}}}
   
   {:comment "past participle non-reflexive plural; essere=true"
    :p [#"^(\S+)és$"           "$1er"]
    :g [#"^([^' ]+)er$"        "$1és"]
    :u {:synsem {:infl :past-p
                 :essere true
                 :subcat {:1 {:agr {:gender :masc
                                    :number :plur}}}}}}
   ;; singular feminine
   {:comment "past participle non-reflexive singular feminine; essere=true"
    :p [#"^(\S+)ée$"           "$1er"]
    :g [#"^([^' ]+)er$"        "$1ée"]
    :u {:synsem {:infl :past-p
                 :essere true
                 :subcat {:1 {:agr {:number :sing
                                    :gender :fem}}}}}}
   ;; plural feminine
   {:comment "past participle non-reflexive plural feminine; essere=true"
    :p [#"^(\S+)ées$"          "$1er"]
    :g [#"^([^' ]+)er$"        "$1ées"]
    :u {:synsem {:infl :past-p
                 :essere true
                 :subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}}}}
   ])

(def conditional
  [
   {:p [#"^(\S+)ais$"       "$1"]
    :g [#"^(\S+)$"          "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ais$"       "s'$1"]
    :g [#"^s'(\S+)$"        "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ais$"       "se $1"]
    :g [#"^se (\S+)$"       "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ais$"       "$1"]
    :g [#"^(\S+)$"          "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ais$"       "s'$1"]
    :g [#"^s'(\S+)$"        "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ais$"       "se $1"]
    :g [#"^se (\S+)$"       "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)rais$"      "$1re"]
    :g [#"^(\S+)$"          "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rais$"      "s'$1re"]
    :g [#"^s'(\S+)$"        "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rais$"      "se $1re"]
    :g [#"^se (\S+)re$"     "$1rais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rais$"      "$1"]
    :g [#"^(\S+)re$"        "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)rais$"      "s'$1"]
    :g [#"^s'(\S+)re$"      "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)rais$"      "se $1"]
    :g [#"^se (\S+)$"       "$1ais"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ait$"       "$1"]
    :g [#"^(\S+)$"          "$1ait"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ait$"       "s'$1"]
    :g [#"^s'(\S+)$"        "$1ait"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ait$"       "se $1"]
    :g [#"^se (\S+)$"       "$1ait"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)rait$"      "$1re"]
    :g [#"^(\S+)re$"        "$1rait"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)rait$"      "s'$1re"]
    :g [#"^s'(\S+)$"        "$1rait"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)rait$"      "se $1re"]
    :g [#"^se (\S+)$"       "$1ait"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ions$"      "$1"]
    :g [#"^(\S+)$"          "$1ions"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ions$"      "s'$1"]
    :g [#"^s' (\S+)$"       "$1ions"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ions$"      "se $1"]
    :g [#"^se (\S+)$"       "$1ions"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rions$"     "$1re"]
    :g [#"^(\S+)re$"        "$1rions"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rions$"     "s'$1re"]
    :g [#"^s'(\S+)$"        "$1ions"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rions$"     "se $1re"]
    :g [#"^se (\S+)$"       "$1rions"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)iez$"       "$1"]
    :g [#"^(\S+)$"          "$1iez"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)iez$"       "s'$1"]
    :g [#"^s'(\S+)$"        "$1iez"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)iez$"       "se $1"]
    :g [#"^se (\S+)$"       "$1iez"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)riez$"      "$1re"]
    :g [#"^(\S+)re$"        "$1riez"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)riez$"      "s'$1re"]
    :g [#"^s'(\S+)re$"      "$1riez"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)riez$"      "se $1re"]
    :g [#"^se (\S+)re$"     "$1riez"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)aient$"     "$1"]
    :g [#"^(\S+)$"          "$1aient"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)aient$"     "s'$1"]
    :g [#"^s'(\S+)$"        "$1aient"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)aient$"     "se $1"]
    :g [#"^se (\S+)$"       "$1aient"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)raient$"    "$1re"]
    :g [#"^(\S+)$"          "$1aient"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)raient$"    "s'$1re"]
    :g [#"^s'(\S+)$"        "$1aient"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)raient$"    "se $1re"]
    :g [#"^se (\S+)re$"     "$1raient"]
    :u {:synsem {:infl :conditional
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   ]
  )

(def future
  [
   {:p [#"^(\S+)ai$"     "$1"]
    :g [#"^(\S+)$"       "$1ai"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ai$"     "s'$1"]
    :g [#"^s'(\S+)$"     "$1ai"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ai$"     "se $1"]
    :g [#"^se (\S+)$"    "$1ai"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)as$"     "$1"]
    :g [#"^(\S+)$"       "$1as"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)as$"     "s'$1"]
    :g [#"^s'(\S+)$"     "$1as"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)as$"     "se $1"]
    :g [#"^se (\S+)$"    "$1as"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)rai$"    "$1re"]
    :g [#"^(\S+)$"       "$1ai"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rai$"    "s'$1re"]
    :g [#"^s'(\S+)$"     "$1ai"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rai$"    "se $1re"]
    :g [#"^se (\S+)re$"  "$1rai"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ras$"    "$1re"]
    :g [#"^(\S+)re$"     "$1as"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ras$"    "s'$1re"]
    :g [#"^s'(\S+)re$"   "$1as"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ras$"    "se $1re"]
    :g [#"^se (\S+)$"    "$1as"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)a$"      "$1"]
    :g [#"^(\S+)$"       "$1a"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)a$"      "s'$1"]
    :g [#"^s'(\S+)$"     "$1a"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)a$"      "se $1"]
    :g [#"^se (\S+)$"    "$1a"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ra$"     "$1re"]
    :g [#"^(\S+)re$"     "$1ra"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ra$"     "s'$1re"]
    :g [#"^s'(\S+)$"     "$1ra"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ra$"     "se $1re"]
    :g [#"^se (\S+)$"    "$1a"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ons$"    "$1"]
    :g [#"^(\S+)$"       "$1ons"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ons$"    "s'$1"]
    :g [#"^s' (\S+)$"    "$1ons"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ons$"    "se $1"]
    :g [#"^se (\S+)$"    "$1ons"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rons$"   "$1re"]
    :g [#"^(\S+)re$"     "$1rons"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rons$"   "s'$1re"]
    :g [#"^s'(\S+)$"     "$1ons"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rons$"   "se $1re"]
    :g [#"^se (\S+)$"    "$1rons"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ez$"     "$1"]
    :g [#"^(\S+)$"       "$1ez"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ez$"     "s'$1"]
    :g [#"^s'(\S+)$"     "$1ez"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ez$"     "se $1"]
    :g [#"^se (\S+)$"    "$1ez"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)rez$"    "$1re"]
    :g [#"^(\S+)re$"     "$1rez"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)rez$"    "s'$1re"]
    :g [#"^s'(\S+)re$"   "$1rez"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)rez$"    "se $1re"]
    :g [#"^se (\S+)re$"  "$1rez"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ont$"    "$1"]
    :g [#"^(\S+)$"       "$1ont"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ont$"    "s'$1"]
    :g [#"^s'(\S+)$"     "$1ont"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ont$"    "se $1"]
    :g [#"^se (\S+)$"    "$1ont"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ront$"   "$1re"]
    :g [#"^(\S+)re$"     "$1ront"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ront$"   "s'$1re"]
    :g [#"^s'(\S+)re$"   "$1ont"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ront$"   "se $1re"]
    :g [#"^se (\S+)re$"  "$1ront"]
    :u {:synsem {:infl :future
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   ]
  )

(def replace-patterns-source
  (apply concat
         [
          conditional
          past
          past-reflexive
          present-nonreflexive-er-verb
          present-nonreflexive-ir-verb
          present-nonreflexive-re-verb
          present-reflexive
          present-reflexive-re-verb
          past-reflexive-re-verb
          future
          ]))

;; this (map..) is needed because a word within a syntactic tree will be conjugated
;; as a part of :a or :b within the tree, and with its :infl and :agr as keys within
;; the :francais part of the verb.
(def replace-patterns
  (map (fn [pattern]
         {:p (:p pattern)
          :comment (:comment pattern)
          :g (:g pattern)
          :u (unifyc (:u pattern)
                     (let [agr (atom :top)
                           essere (atom :top)
                           infl (atom :top)]
                       {:synsem {:essere essere
                                 :infl infl
                                 :subcat {:1 {:agr agr}}}
                        :essere essere
                        :infl infl
                        :agr agr}))})
       replace-patterns-source))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. error-string)))
  #?(:cljs
     (throw (js/Error. error-string))))

(declare number-and-person)

(def suppress-incomplete-morphology-errors true)

(defn reflexive-to-infinitive [reflexive-infinitive]
  "e.g.: se amuser -> amuser"
  (cond
    (re-find #"^se " reflexive-infinitive)
    (string/replace reflexive-infinitive #"^se " "")
    (re-find #"^s'" reflexive-infinitive)
    (string/replace reflexive-infinitive #"^s'" "")
    true
    reflexive-infinitive))

(defn imperfect [word]           
  (let [infinitive (reflexive-to-infinitive (get-in word [:français]))
        ar-type (try (re-find #"ar$" infinitive)
                     (catch Exception e
                       (exception (str "Can't regex-find on non-string: " infinitive " from word: " word))))
        er-type (re-find #"er$" infinitive)
        ir-type (re-find #"ir$" infinitive)
        stem (if (get-in word [:imperfect-stem])
               (get-in word [:imperfect-stem])
               (string/replace infinitive #"[iae]r$" ""))
        last-stem-char-is-i (re-find #"ir$" infinitive)
        last-stem-char-is-e (re-find #"er$" infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))
        number-and-person (number-and-person number person)]
    (cond
     (get-in word [:imperfect number-and-person])
     (get-in word [:imperfect number-and-person])

     ;; TODO: this seems wrong: 'stem' is never false
     (and (= person :1st) (= number :sing) (or er-type stem))
     (str stem "ais")

     (and (= person :1st) (= number :sing) ir-type)
     (str stem "issais")

     ;; TODO: this seems wrong: 'stem' is never false
     (and (= person :2nd) (= number :sing) (or er-type stem))
     (str stem "ais")
           
     (and (= person :2nd) (= number :sing) ir-type)
     (str stem "issais")
              
     ;; TODO: this seems wrong: 'stem' is never false
     (and (= person :3rd) (= number :sing) (or er-type stem))
     (str stem "ait")
           
     (and (= person :3rd) (= number :sing) ir-type)
     (str stem "issait")

     ;; TODO: this seems wrong: 'stem' is never false
     (and (= person :1st) (= number :plur) (or er-type stem))
     (str stem "ions")
           
     (and (= person :1st) (= number :plur) ir-type)
     (str stem "issions")
           
     ;; <second person plural imperfecto>

     ;; TODO: this seems wrong: 'stem' is never false
     (and (= person :2nd) (= number :plur) (or er-type stem))
     (str stem "iez")
           
     (and (= person :2nd) (= number :plur) ir-type)
     (str stem "issiez")
                 
     ;; </second person plural imperfecto>
           
     ;; <third person plural imperfecto>
     ;; TODO: this seems wrong: 'stem' is never false
     (and (= person :3rd) (= number :plur) (or er-type stem))
     (str stem "aient")
       
     (and (= person :3rd) (= number :plur)
          ir-type)
     (str stem "issaient")
     ;; </third person plural imperfecto>

     (and (or (= :top person)
              (= :top (get-in word [:agr :gender]))
              (= :top number))
          (string? (get-in word [:français])))
     (get-in word [:français])

     :else
     (let [message (str "get-string-1: imperfecto regular inflection: don't know what to do with input argument: " (strip-refs word))]
       (if (= true suppress-incomplete-morphology-errors)
         (do (log/warn message)
             "(" (get-in word [:francais]) ")")
         (exception message))))))

(defn number-and-person [number person]
  (cond (and (= person :1st) (= number :sing))
        :1sing
        (and (= person :1st) (= number :plur))
        :1plur
        (and (= person :2nd) (= number :sing))
        :2sing
        (and (= person :2nd) (= number :plur))
        :2plur
        (and (= person :3rd) (= number :sing))
        :3sing
        (and (= person :3rd) (= number :plur))
        :3plur
        true
        nil))
