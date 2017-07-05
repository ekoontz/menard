(ns babel.francais.morphology.verbs
  (:refer-clojure :exclude [future get-in resolve])
  (:require
   [babel.francais.morphology.nouns :as nouns]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unify)]))

;; TODO: convert to (babel.morphology/conjugation) (see babel.latin.morphology for an example of how to use conjugation)

(def agr
  (let [agr (atom :top)]
    {:synsem {:agr agr
              :subcat {:1 {:agr agr}}}}))

(def present-nonreflexive-er-verb
  (map
   #(merge %
           {:u (unify agr
                      (:u %))})
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
    ]))

(def present-nonreflexive-ir-verb
  (map
   #(merge %
           {:u (unify agr
                      (:u %))})
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
   ]))

(def present-nonreflexive-re-verb
  (map
   #(merge %
           {:u (unify agr
                      (:u %))})
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
]))

(def present-reflexive-re-verb
  (map
   #(merge %
           {:u (unify agr
                      (:u %))})
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
                  :infl :present}}}]))

(def present-reflexive
  ;; reflexive present -er and -ir type verbs
  (map
   #(merge %
           {:u (unify agr
                      (:u %))})
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
                 :infl :present}}}]))

(def past-reflexive
  (map
   #(merge %
           {:u (unify agr
                      (:u %))})
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
   ]))

(def past-reflexive-re-verb
  (map
   #(merge %
           {:u (unify agr
                      (:u %))})
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
]))

(def past
  (map
   #(merge %
           {:u (unify agr
                      (:u %))})
   [{:comment "past participle non-reflexive plural; essere=true"
     :p [#"^(\S+)és$"           "$1er"]
     :g [#"^([^' ]+)er$"        "$1és"]
     :u {:synsem {:infl :past-p
                  :essere true
                  :agr {:gender :masc
                        :number :plur}
                  :subcat {:1 {:agr {:gender :masc
                                     :number :plur}}}}}}
    ;; singular feminine
    {:comment "past participle non-reflexive singular feminine; essere=true"
     :p [#"^(\S+)ée$"           "$1er"]
     :g [#"^([^' ]+)er$"        "$1ée"]
     :u {:synsem {:infl :past-p
                  :essere true
                  :agr {:number :sing
                        :gender :fem}
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
    ]))

(def conditional
  (map
   #(merge %
           {:u (unify agr
                      (:u %))})
  [{:p [#"^(\S+)ais$"       "$1"]
    :g [#"^(\S+)$"          "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ais$"       "s'$1"]
    :g [#"^s'(\S+)$"        "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ais$"       "se $1"]
    :g [#"^se (\S+)$"       "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ais$"       "$1"]
    :g [#"^(\S+)$"          "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ais$"       "s'$1"]
    :g [#"^s'(\S+)$"        "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ais$"       "se $1"]
    :g [#"^se (\S+)$"       "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)rais$"      "$1re"]
    :g [#"^(\S+)$"          "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rais$"      "s'$1re"]
    :g [#"^s'(\S+)$"        "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rais$"      "se $1re"]
    :g [#"^se (\S+)re$"     "$1rais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rais$"      "$1"]
    :g [#"^(\S+)re$"        "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)rais$"      "s'$1"]
    :g [#"^s'(\S+)re$"      "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)rais$"      "se $1"]
    :g [#"^se (\S+)$"       "$1ais"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)ait$"       "$1"]
    :g [#"^(\S+)$"          "$1ait"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ait$"       "s'$1"]
    :g [#"^s'(\S+)$"        "$1ait"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ait$"       "se $1"]
    :g [#"^se (\S+)$"       "$1ait"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)rait$"      "$1re"]
    :g [#"^(\S+)re$"        "$1rait"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)rait$"      "s'$1re"]
    :g [#"^s'(\S+)$"        "$1rait"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)rait$"      "se $1re"]
    :g [#"^se (\S+)$"       "$1ait"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)ions$"      "$1"]
    :g [#"^(\S+)$"          "$1ions"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ions$"      "s'$1"]
    :g [#"^s' (\S+)$"       "$1ions"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)ions$"      "se $1"]
    :g [#"^se (\S+)$"       "$1ions"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rions$"     "$1re"]
    :g [#"^(\S+)re$"        "$1rions"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rions$"     "s'$1re"]
    :g [#"^s'(\S+)$"        "$1ions"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)rions$"     "se $1re"]
    :g [#"^se (\S+)$"       "$1rions"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :1st}}}}}}
   {:p [#"^(\S+)iez$"       "$1"]
    :g [#"^(\S+)$"          "$1iez"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)iez$"       "s'$1"]
    :g [#"^s'(\S+)$"        "$1iez"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)iez$"       "se $1"]
    :g [#"^se (\S+)$"       "$1iez"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)riez$"      "$1re"]
    :g [#"^(\S+)re$"        "$1riez"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)riez$"      "s'$1re"]
    :g [#"^s'(\S+)re$"      "$1riez"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)riez$"      "se $1re"]
    :g [#"^se (\S+)re$"     "$1riez"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}}}}
   {:p [#"^(\S+)aient$"     "$1"]
    :g [#"^(\S+)$"          "$1aient"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)aient$"     "s'$1"]
    :g [#"^s'(\S+)$"        "$1aient"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)aient$"     "se $1"]
    :g [#"^se (\S+)$"       "$1aient"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)raient$"    "$1re"]
    :g [#"^(\S+)$"          "$1aient"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)raient$"    "s'$1re"]
    :g [#"^s'(\S+)$"        "$1aient"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   {:p [#"^(\S+)raient$"    "se $1re"]
    :g [#"^se (\S+)re$"     "$1raient"]
    :u {:synsem {:infl :conditional
                 :cat :verb
                 :subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}}}}
   ]
  ))

(def future
  (map
   #(merge %
           {:u (unify agr
                      (:u %)
                      {:synsem {:cat :verb
                                :infl :future}})})
   [
    {:p [#"^(\S+)ai$"     "$1"]
     :g [#"^(\S+)$"       "$1ai"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :1st}}}}}}
    {:p [#"^(\S+)ai$"     "s'$1"]
     :g [#"^s'(\S+)$"     "$1ai"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :1st}}}}}}
    {:p [#"^(\S+)ai$"     "se $1"]
     :g [#"^se (\S+)$"    "$1ai"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :1st}}}}}}
    {:p [#"^(\S+)as$"     "$1"]
     :g [#"^(\S+)$"       "$1as"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)as$"     "s'$1"]
     :g [#"^s'(\S+)$"     "$1as"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)as$"     "se $1"]
     :g [#"^se (\S+)$"    "$1as"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)rai$"    "$1re"]
     :g [#"^(\S+)$"       "$1ai"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :1st}}}}}}
    {:p [#"^(\S+)rai$"    "s'$1re"]
     :g [#"^s'(\S+)$"     "$1ai"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :1st}}}}}}
    {:p [#"^(\S+)rai$"    "se $1re"]
     :g [#"^se (\S+)re$"  "$1rai"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :1st}}}}}}
    {:p [#"^(\S+)ras$"    "$1re"]
     :g [#"^(\S+)re$"     "$1as"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)ras$"    "s'$1re"]
     :g [#"^s'(\S+)re$"   "$1as"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)ras$"    "se $1re"]
     :g [#"^se (\S+)$"    "$1as"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)a$"      "$1"]
     :g [#"^(\S+)$"       "$1a"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)a$"      "s'$1"]
     :g [#"^s'(\S+)$"     "$1a"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)a$"      "se $1"]
     :g [#"^se (\S+)$"    "$1a"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)ra$"     "$1re"]
     :g [#"^(\S+)re$"     "$1ra"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)ra$"     "s'$1re"]
     :g [#"^s'(\S+)$"     "$1ra"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)ra$"     "se $1re"]
     :g [#"^se (\S+)$"    "$1a"]
     :u {:synsem {:subcat {:1 {:agr {:number :sing
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)ons$"    "$1"]
     :g [#"^(\S+)$"       "$1ons"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :1st}}}}}}
    {:p [#"^(\S+)ons$"    "s'$1"]
     :g [#"^s' (\S+)$"    "$1ons"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :1st}}}}}}
    {:p [#"^(\S+)ons$"    "se $1"]
     :g [#"^se (\S+)$"    "$1ons"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :1st}}}}}}
    {:p [#"^(\S+)rons$"   "$1re"]
     :g [#"^(\S+)re$"     "$1rons"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :1st}}}}}}
    {:p [#"^(\S+)rons$"   "s'$1re"]
     :g [#"^s'(\S+)$"     "$1ons"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :1st}}}}}}
    {:p [#"^(\S+)rons$"   "se $1re"]
     :g [#"^se (\S+)$"    "$1rons"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :1st}}}}}}
    {:p [#"^(\S+)ez$"     "$1"]
     :g [#"^(\S+)$"       "$1ez"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)ez$"     "s'$1"]
     :g [#"^s'(\S+)$"     "$1ez"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)ez$"     "se $1"]
     :g [#"^se (\S+)$"    "$1ez"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)rez$"    "$1re"]
     :g [#"^(\S+)re$"     "$1rez"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)rez$"    "s'$1re"]
     :g [#"^s'(\S+)re$"   "$1rez"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)rez$"    "se $1re"]
     :g [#"^se (\S+)re$"  "$1rez"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :2nd}}}}}}
    {:p [#"^(\S+)ont$"    "$1"]
     :g [#"^(\S+)$"       "$1ont"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)ont$"    "s'$1"]
     :g [#"^s'(\S+)$"     "$1ont"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)ont$"    "se $1"]
     :g [#"^se (\S+)$"    "$1ont"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)ront$"   "$1re"]
     :g [#"^(\S+)re$"     "$1ront"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)ront$"   "s'$1re"]
     :g [#"^s'(\S+)re$"   "$1ont"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :3rd}}}}}}
    {:p [#"^(\S+)ront$"   "se $1re"]
     :g [#"^se (\S+)re$"  "$1ront"]
     :u {:synsem {:subcat {:1 {:agr {:number :plur
                                     :person :3rd}}}}}}
    ]))

(def regular-patterns-source
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
(def regular-patterns
  (map (fn [pattern]
         {:p (:p pattern)
          :comment (:comment pattern)
          :g (:g pattern)
          :u (unify (:u pattern)
                    (let [agr (atom :top)
                          essere (atom :top)
                          infl (atom :top)]
                      {:synsem {:essere essere
                                :infl infl
                                :subcat {:1 {:agr agr}}}
                       :essere essere
                        :infl infl
                       :agr agr}))})
       regular-patterns-source))

(def irregular-patterns
  [;; 1. past-tense exceptions
   {:path [:français :past-participle]
    :merge-fn
    (fn [val]
      {:français {:infl :past-p
                  :conjugated true                   
                  :français (get-in val [:français :past-participle] :nothing)}})}
   ;; 2. present-tense exceptions
   {:path [:français :present :1sing]
    :merge-fn
    (fn [val]
      {:français {:infl :present
                  :conjugated true                   
                  :français (get-in val [:français :present :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:français :present :2sing]
    :merge-fn
    (fn [val]
      {:français {:infl :present
                  :conjugated true                   
                  :français (get-in val [:français :present :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   {:path [:français :present :3sing]
    :merge-fn
    (fn [val]
      {:français {:infl :present
                  :conjugated true                   
                  :français (get-in val [:français :present :3sing] :nothing)
                  :agr {:number :sing
                        :person :3rd}}})}
   {:path [:français :present :1plur]
    :merge-fn
    (fn [val]
      {:français {:infl :present
                  :conjugated true                   
                  :français (get-in val [:français :present :1plur] :nothing)
                  :agr {:number :plur
                        :person :1st}}})}
   {:path [:français :present :2plur]
    :merge-fn
    (fn [val]
      {:français {:infl :present
                  :conjugated true                   
                  :français (get-in val [:français :present :2plur] :nothing)
                  :agr {:number :plur
                        :person :2nd}}})}
   {:path [:français :present :3plur]
    :merge-fn
    (fn [val]
      {:français {:infl :present
                  :conjugated true                   
                  :français (get-in val [:français :present :3plur] :nothing)
                  :agr {:number :plur
                        :person :3rd}}})}
   ;; 3. imperfect-tense exceptions
   {:path [:français :imperfect :1sing]
    :merge-fn
    (fn [val]
      {:français {:infl :imperfect
                  :conjugated true                   
                  :français (get-in val [:français :imperfect :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:français :imperfect :2sing]
    :merge-fn
    (fn [val]
      {:français {:infl :imperfect
                  :conjugated true                   
                  :français (get-in val [:français :imperfect :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   {:path [:français :imperfect :3sing]
    :merge-fn
    (fn [val]
      {:français {:infl :imperfect
                   :conjugated true                   
                  :français (get-in val [:français :imperfect :3sing] :nothing)
                  :agr {:number :sing
                        :person :3rd}}})}
   {:path [:français :imperfect :1plur]
    :merge-fn
    (fn [val]
      {:français {:infl :imperfect
                  :français (get-in val [:français :imperfect :1plur] :nothing)
                  :agr {:number :plur
                        :person :1st}}})}
   {:path [:français :imperfect :2plur]
    :merge-fn
    (fn [val]
      {:français {:infl :imperfect
                   :conjugated true                   
                  :français (get-in val [:français :imperfect :2plur] :nothing)
                  :agr {:number :plur
                        :person :2nd}}})}
   {:path [:français :imperfect :3plur]
    :merge-fn
    (fn [val]
      {:français {:infl :imperfect
                  :conjugated true                   
                  :français (get-in val [:français :imperfect :3plur] :nothing)
                  :agr {:number :plur
                        :person :3rd}}})}

   {:path [:français :imperfect-stem]
    :merge-fn
    (fn [val]
      [{:français {:infl :imperfect
                   :conjugated true                   
                   :imperfect-stem (get-in val [:français :imperfect-stem])
                   :français (str (get-in val [:français :imperfect-stem]) "ais")
                   :agr {:number :sing
                         :person :1st}}}])}
   
   {:path [:français :imperfect-stem]
    :merge-fn
    (fn [val]
      [{:français {:infl :imperfect
                   :conjugated true                   
                   :imperfect-stem (get-in val [:français :imperfect-stem])
                   :français (str (get-in val [:français :imperfect-stem]) "ais")
                   :agr {:number :sing
                         :person :2nd}}}])}
   
   {:path [:français :imperfect-stem]
    :merge-fn
    (fn [val]
      [{:français {:infl :imperfect
                   :conjugated true                   
                   :imperfect-stem (get-in val [:français :imperfect-stem])
                   :français (str (get-in val [:français :imperfect-stem]) "ait")
                   :agr {:number :sing
                         :person :3rd}}}])}
   
   {:path [:français :imperfect-stem]
    :merge-fn
    (fn [val]
      [{:français {:infl :imperfect
                   :conjugated true                   
                   :imperfect-stem (get-in val [:français :imperfect-stem])
                   :français (str (get-in val [:français :imperfect-stem]) "ions")
                   :agr {:number :plur
                         :person :1st}}}])}
   
   {:path [:français :imperfect-stem]
    :merge-fn
    (fn [val]
      [{:français {:infl :imperfect
                   :conjugated true                   
                   :imperfect-stem (get-in val [:français :imperfect-stem])
                   :français (str (get-in val [:français :imperfect-stem]) "iez")
                   :agr {:number :plur
                         :person :2nd}}}])}
   
   {:path [:français :imperfect-stem]
    :merge-fn
    (fn [val]
      [{:français {:infl :imperfect
                   :conjugated true                   
                   :imperfect-stem (get-in val [:français :imperfect-stem])
                   :français (str (get-in val [:français :imperfect-stem]) "aient")
                   :agr {:number :plur
                         :person :3rd}}}])}
   
   ;; 3. present-tense boot-stem exception: :boot-stem1.
   {:path [:français :boot-stem1]
    :merge-fn
    (fn [val]
      [{:français {:infl :present
                   :conjugated true                   
                   :français (str (get-in val [:français :boot-stem1])
                                  "s")
                   :agr {:number :sing
                         :person :1st}}}
       {:français {:infl :present
                   :conjugated true                   
                   :français (str (get-in val [:français :boot-stem1])
                                  "s")
                   :agr {:number :sing
                         :person :2nd}}}
       {:français {:infl :present
                   :conjugated true                   
                   :français (str (get-in val [:français :boot-stem1])
                                  "t")
                   :agr {:number :sing
                         :person :3rd}}}
       {:français {:infl :present
                   :conjugated true                   
                   :français (str (get-in val [:français :boot-stem1])
                                  "vent")
                   :agr {:number :plur
                         :person :3rd}}}])}
   
   {:path [:français :boot-stem2]
    :merge-fn
    (fn [val]
      [{:français {:infl :present
                   :conjugated true
                   :français (str (get-in val [:français :boot-stem2])
                                  "ons")
                   :agr {:number :plur
                         :person :1st}}}
       {:français {:infl :present
                   :conjugated true
                   :français (str (get-in val [:français :boot-stem2])
                                  "ez")
                   :agr {:number :plur
                         :person :2nd}}}])}])


(def irregular-conjugations
  (concat
   [{:surface-fn #(str (get-in % [:français :imperfect-stem]) "ais")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :sing}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}
    {:surface-fn #(str (get-in % [:français :imperfect-stem]) "ais")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :sing}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}
    {:surface-fn #(str (get-in % [:français :imperfect-stem]) "ait")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :sing}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}
    {:surface-fn #(str (get-in % [:français :imperfect-stem]) "ions")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :plur}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}
    {:surface-fn #(str (get-in % [:français :imperfect-stem]) "iez")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :plur}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}
    {:surface-fn #(str (get-in % [:français :imperfect-stem]) "aient")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :plur}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}


    {:surface-fn #(str (get-in % [:français :boot-stem1]) "s")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :sing}
                           :infl :present}
                  :français {:present {:boot-stem1 true}}}}

    {:surface-fn #(str (get-in % [:français :boot-stem1]) "s")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :sing}
                           :infl :present}
                  :français {:present {:boot-stem1 true}}}}

    {:surface-fn #(str (get-in % [:français :boot-stem1]) "t")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :sing}
                           :infl :present}
                  :français {:present {:boot-stem1 true}}}}
    
    {:surface-fn #(str (get-in % [:français :boot-stem2]) "ons")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :plur}
                           :infl :present}
                  :français {:present {:boot-stem2 true}}}}

    {:surface-fn #(str (get-in % [:français :boot-stem2]) "ez")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :plur}
                           :infl :present}
                  :français {:present {:boot-stem2 true}}}}

    {:surface-fn #(str (get-in % [:français :boot-stem1]) "ent")
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :plur}
                           :infl :present}
                  :français {:present {:boot-stem1 true}}}}]
    
   (mapcat (fn [infl]
             [{:surface-fn #(get-in % [:français infl :1sing])
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :1st
                                             :number :sing}}
                            :synsem {:cat :verb
                                     :infl infl}}}
              {:surface-fn #(get-in % [:français infl :2sing])
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :2nd
                                             :number :sing}}
                            :synsem {:cat :verb
                                     :infl infl}}}
              {:surface-fn #(get-in % [:français infl :3sing])
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :3rd
                                             :number :sing}}
                            :synsem {:cat :verb
                                     :infl infl}}}
              {:surface-fn #(get-in % [:français infl :1plur])
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :1st
                                             :number :plur}}
                            :synsem {:cat :verb
                                     :infl infl}}}
              {:surface-fn #(get-in % [:français infl :2plur])
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :2nd
                                             :number :plur}}
                            :synsem {:cat :verb
                                     :infl infl}}}
              {:surface-fn #(get-in % [:français infl :3plur])
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :3rd
                                             :number :plur}}
                            :synsem {:cat :verb
                                     :infl infl}}}])
           [:conditional :future :imperfect :present])))

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
