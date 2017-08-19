(ns babel.francais.morphology.verbs
  (:refer-clojure :exclude [future get-in resolve])
  (:require
   [babel.francais.morphology.nouns :as nouns]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unify)]))

;; TODO: convert to (babel.morphology/conjugation)
;; (see babel.latin.morphology for an example of how to use babel.morphology/conjugation)
(def present-nonreflexive-er-verb
  (map
   #(merge %
           {:u (unify (:u %)
                      {:synsem {:infl :present}})})
   [
    {:p [#"^([^' ]+)e$"         "$1er"]
     :g [#"^([^' ]+)er$"        "$1e"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}

    {:p [#"^([^' ]+)es$"        "$1er"]
     :g [#"^([^' ]+)er$"        "$1es"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}

    {:p [#"^([^' ]+)e$"         "$1er"]
     :g [#"^([^' ]+)er$"        "$1e"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}

    {:p [#"^([^' ]+)([^e])ons$" "$1$2er"]
     :g [#"^([^' ]+)([^g])er$"  "$1$2ons"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}

    {:p [#"^([^' ]+)(g)eons$"   "$1$2er"]
     :g [#"^([^' ]+)(g)er$"     "$1$2eons"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}

    {:p [#"^([^' ]+)ez$"        "$1er"]
     :g [#"^([^' ]+)er$"        "$1ez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}

    {:p [#"^([^' ]+)ent$"       "$1er"]
     :g [#"^([^' ]+)er$"        "$1ent"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}
    ]))

(def present-nonreflexive-ir-verb
  (map
   #(merge %
           {:u (unify (:u %)
                      {:synsem {:infl :present}})})
  [
   {:p [#"^([^' ]+)is$"         "$1ir"]
    :g [#"^([^' ]+)ir$"         "$1is"]
    :u {:synsem {:agr {:number :sing
                       :person :1st}}}}

   {:p [#"^([^' ]+)it$"        "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1it"]
    :u {:synsem {:agr {:number :sing
                       :person :2nd}}}}

   {:p [#"^([^' ]+)e$"         "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1e"]
    :u {:synsem {:agr {:number :sing
                       :person :3rd}}}}

   {:p [#"^([^' ]+)issons$"    "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1issons"]
    :u {:synsem {:agr {:number :plur
                       :person :1st}}}}

   {:p [#"^([^' ]+)issez$"     "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1issez"]
    :u {:synsem {:agr {:number :plur
                       :person :2nd}}}}

   {:p [#"^([^' ]+)issent$"    "$1ir"]
    :g [#"^([^' ]+)ir$"        "$1issent"]
    :u {:synsem {:agr {:number :plur
                       :person :3rd}}}}
   ]))

(def present-nonreflexive-re-verb
  (map
   #(merge %
           {:u (unify (:u %)
                      {:synsem {:infl :present}})})
  [
   {:p [#"^([^' ]+)$"          "$1re"]
    :g [#"^([^' ]+)re$"        "$1"]
    :u {:synsem {:agr {:number :sing
                       :person :1st}}}}

   {:p [#"^([^' ]+)s$"        "$1re"]
    :g [#"^([^' ]+)re$"       "$1s"]
    :u {:synsem {:agr {:number :sing
                       :person :2nd}}}}

   {:p [#"^([^' ]+)$"         "$1re"]
    :g [#"^([^' ]+)re$"       "$1"]
    :u {:synsem {:agr {:number :sing
                       :person :3rd}}}}

   ;; e.g. apprendre => apprenons
   {:p [#"^([^' ]+)ons$"      "$1dre"]
    :g [#"^([^' ]+)dre$"      "$1ons"]
    :u {:synsem {:agr {:number :plur
                       :person :1st}}}}

   {:p [#"^([^' ]+)ez$"       "$1dre"]
    :g [#"^([^' ]+)dre$"      "$1ez"]
    :u {:synsem {:agr {:number :plur
                       :person :2nd}}}}

   {:p [#"^([^' ]+)nent$"     "$1dre"]
    :g [#"^([^' ]+)dre$"      "$1nent"]
    :u {:synsem {:agr {:number :plur
                       :person :3rd}}}}]))

(def present-reflexive-re-verb
  (map
   #(merge %
           {:u (unify (:u %)
                      {:synsem {:infl :present}})})
   [
    {:p [#"^([^' ]+)s$"       "se $1dre"]
     :g [#"^se ([^' ]+)dre$"  "$1s"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}

    {:p [#"^([^' ]+)s$"       "se $1dre"]
     :g [#"^se ([^' ]+)dre$"  "$1s"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}

    {:p [#"^([^' ]+)t$"       "se $1dre"]
     :g [#"^se ([^' ]+)dre$"  "$1t"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}

    ;; e.g. se plaindre => se plaindrons
    {:p [#"^([^' ]+)gnons$"   "se $1ndre"]
     :g [#"^se ([^' ]+)ndre$" "se $1gnons"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}

    {:p [#"^([^' ]+)gniez$"   "se $1ndre"]
     :g [#"^se ([^' ]+)ndre$" "$1gnez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}

    {:p [#"^([^' ]+)gnent$"   "se $1ndre"]
     :g [#"^se ([^' ]+)ndre$" "$1gnent"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}]))

(def present-reflexive
  ;; reflexive present -er and -ir type verbs
  (map
   #(merge %
           {:u (unify {:synsem {:infl :present}}
                      (:u %))})
  [
   {:comment "present reflexive 1st person singular, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)e$"    "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1e"]
    :u {:synsem {:agr {:number :sing
                       :person :1st}}}}

   {:comment "present reflexive 2nd person singular, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)es$"   "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1es"]
    :u {:synsem {:agr {:number :sing
                       :person :2nd}}}}

   {:comment "present reflexive 3rd person singular, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)e$"    "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1e"]
    :u {:synsem {:agr {:number :sing
                       :person :3rd}}}}

   {:comment "present reflexive 1st person plural, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)ons$"  "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1ons"]
    :u {:synsem {:agr {:number :plur
                       :person :1st}}}}

   {:comment "present reflexive 2nd person plural, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)ez$"   "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1ez"]
    :u {:synsem {:agr {:number :plur
                       :person :2nd}}}}

   {:comment "present reflexive 3rd person plural, stem begins with a vowel"
    :p [#"^([aeéiou]\S+)ent$"  "s'$1er"]
    :g [#"^s'(\S+)[ie]r$"      "$1ent"]
    :u {:synsem {:agr {:number :plur
                       :person :3rd}}}}

   {:comment "present reflexive 1st person singular, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)e$"   "se $1er"]
    :g [#"^se ([^aeéiou]\S+)er$"   "$1e"]
    :u {:synsem {:agr {:number :sing
                       :person :1st}}}}

   {:comment "present reflexive 2nd person singular, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)es$"  "se $1er"]
    :g [#"^se ([^aeéiou]\S+)[ie]r$"     "$1es"]
    :u {:synsem {:agr {:number :sing
                       :person :2nd}}}}

   {:comment "present reflexive 3rd person singular, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)e$"   "se $1er"]
    :g [#"^se ([^aeéiou]\S+)[ie]r$"     "$1es"]
    :u {:synsem {:agr {:number :sing
                       :person :3rd}}}}

   {:comment "present reflexive 1st person plural, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)ons$" "s'$1er"]
    :g [#"^se ([^aeéiou]\S+)[ie]r$"     "$1ons"]
    :u {:synsem {:agr {:number :plur
                       :person :1st}}}}

   {:comment "present reflexive 2nd person plural, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)ez$"  "s'$1er"]
    :g [#"^se ([^aeéiou]\S+)[ie]r$"     "$1ez"]
    :u {:synsem {:agr {:number :plur
                       :person :2nd}}}}

   {:comment "present reflexive 3rd person plural, stem begins with a non-vowel"
    :p [#"^([^aeéiou]\S+)ent$" "s'$1er"]
    :g [#"^se ([^aeéiou]\S+)[ie]r$"     "$1ent"]
    :u {:synsem {:agr {:number :plur
                       :person :3rd}}}}]))

(def past-reflexive
  (map
   #(merge %
           {:u (unify {:synsem {:infl :past-p}}
                      (:u %))})
  [
   {:comment "past participle reflexive singular masculine -er where stem begins with a vowel"
    :p [#"^([aeéiou]\S+)é$"    "s'$1er"]
    :g [#"^s'([aeéiou]\S+)er$" "$1é"]
    :u {:synsem {:agr {:number :sing
                       :gender :masc}}}}

   {:comment "past participle reflexive plural masculine -er where stem begins with a vowel"
    :p [#"^([aeéiou]\S+)és$"   "s'$1er"]
    :g [#"^s'([aeéiou]\S+)er$" "$1és"]
    :u {:synsem {:agr {:number :plur
                       :gender :masc}}}}

   {:comment "past participle reflexive singular feminine -er where stem begins with a vowel"
    :p [#"^([aeéiou]\S+)ée$"   "s'$1er"]
    :g [#"^s'([aeéiou]\S+)er$" "$1ée"]
    :u {:synsem {:agr {:number :sing
                       :gender :fem}}}}

   {:comment "past participle reflexive plural feminine -er where stem begins with a vowel"
    :p [#"^([aeéiou]\S+)ées$"  "s'$1er"]
    :g [#"^s'([aeéiou]\S+)er$" "$1ées"]
    :u {:synsem {:agr {:number :plur
                       :gender :fem}}}}

   {:comment "past participle reflexive singular masculine -er where stem does not begin with a vowel"
    :p [#"^([^aeéiou]\S+)é$"   "se $1er"]
    :g [#"^se (\S+)er$"        "$1é"]
    :u {:synsem {:agr {:number :sing
                       :gender :masc}}}}

   {:comment "past participle reflexive plural masculine -er where stem does not begin with a vowel"
    :p [#"^([^aeéiou]\S+)és$"  "se $1er"]
    :g [#"^se (\S+)er$"        "$1és"]
    :u {:synsem {:infl :past-p
                 :agr {:number :plur
                       :gender :masc}}}}

   {:comment "past participle reflexive singular feminine -er where stem does not begin with a vowel"
    :p [#"^([^aeéiou]\S+)ée$"    "se $1er"]
    :g [#"^se ([^aeéiou]\S+)er$" "$1é"]
    :u {:synsem {:agr {:number :sing
                       :gender :fem}}}}

   {:comment "past participle reflexive plural feminine -er where stem does not begin with a vowel"
    :p [#"^([^aeéiou]\S+)ées$"   "se $1er"]
    :g [#"^se ([^aeéiou]\S+)er$" "$1ées"]
    :u {:synsem {:agr {:number :plur
                       :gender :fem}}}}]))

(def past-reflexive-re-verb
  (map
   #(merge %
           {:u (unify {:synsem {:infl :past-p}}
                      (:u %))})
  [
   {:p [#"^([^' ]+)t$"       "se $1dre"]
    :g [#"^se ([^' ]+)dre$"  "$1t"]
    :u {:synsem {:agr {:number :sing
                       :person :1st}}}}

   {:p [#"^([^' ]+)t$"       "se $1dre"]
    :g [#"^se ([^' ]+)dre$"  "$1t"]
    :u {:synsem {:agr {:number :sing
                       :person :2nd}}}}

   {:p [#"^([^' ]+)ts$"      "se $1dre"]
    :g [#"^se ([^' ]+)dre$"  "$1ts"]
    :u {:synsem {:agr {:number :sing
                       :person :3rd}}}}

   ;; e.g. se plaindre => se plaindrons
   {:p [#"^([^' ]+)ts$"      "se $1dre"]
    :g [#"^se ([^' ]+)ndre$" "se $1ts"]
    :u {:synsem {:agr {:number :plur
                       :person :1st}}}}

   {:p [#"^([^' ]+)ts$"   "se $1ndre"]
    :g [#"^se ([^' ]+)ndre$" "$1ts"]
    :u {:synsem {:agr {:number :plur
                       :person :2nd}}}}

   {:p [#"^([^' ]+)ts$"   "se $1ndre"]
    :g [#"^se ([^' ]+)ndre$" "$1ts"]
    :u {:synsem {:agr {:number :plur
                       :person :3rd}}}}
   ]))

(def past
  (map
   #(merge %
           {:u (unify {:synsem {:cat :verb
                                :infl :past-p}}
                      (:u %))})
   [{:comment "past participle non-reflexive plural; essere=true"
     :p [#"^(\S+)és$"           "$1er"]
     :g [#"^([^' ]+)er$"        "$1és"]
     :u {:synsem {:essere true
                  :agr {:gender :masc
                        :number :plur}}}}
    ;; singular feminine
    {:comment "past participle non-reflexive singular feminine; essere=true"
     :p [#"^(\S+)ée$"           "$1er"]
     :g [#"^([^' ]+)er$"        "$1ée"]
     :u {:synsem {:essere true
                  :agr {:number :sing
                        :gender :fem}}}}
    ;; plural feminine
    {:comment "past participle non-reflexive plural feminine; essere=true"
     :p [#"^(\S+)ées$"          "$1er"]
     :g [#"^([^' ]+)er$"        "$1ées"]
     :u {:synsem {:essere true
                  :agr {:number :plur
                        :gender :fem}}}}

    {:comment "past participle non-reflexive singular -er; essere=false"
     :p [#"^(\S+)é$"            "$1er"]
     :g [#"^([^' ]+)er$"        "$1é"]
     :u {:synsem {:essere false}}}

    {:comment "past participle non-reflexive -re"
     :p [#"^(\S+)u$"            "$1re"]
     :g [#"^([^' ]+)re$"        "$1u"]
     :u {:synsem {:français {:past-p {:regular true}}}}}

    {:comment "past participle non-reflexive -ir"
     :p [#"^(\S+)i$"            "$1ir"]
     :g [#"^([^' ]+)ir$"        "$1i"]
     :u {}}
    ]))


(def future
  (map
   #(merge %
           {:u (unify (:u %)
                      {:synsem {:cat :verb
                                :infl :future}})})
   [
    ;; -re verbs: drop the e to form the future
    {:p [#"^(\S+)ai$"     "$1re"]
     :g [#"^([^' ]+r)e$"    "$1ai"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}
    {:p [#"^(\S+)ai$"     "s'$1"]
     :g [#"^s'(\S+r)e$"     "$1ai"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}
    {:p [#"^(\S+)ai$"     "se $1"]
     :g [#"^se (\S+r)e$"    "$1ai"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}

    ;; non -re verbs

                        {:p [#"^(\S+)ai$"     "$1"]
                         :g [#"^([^' ]+)$"    "$1ai"]
                         :u {:synsem {:agr {:number :sing
                                            :person :1st}}}}
                        {:p [#"^(\S+)ai$"     "s'$1"]
                         :g [#"^s'(\S+)$"     "$1ai"]
                         :u {:synsem {:agr {:number :sing
                                            :person :1st}}}}
                        {:p [#"^(\S+)ai$"     "se $1"]
                         :g [#"^se (\S+)$"    "$1ai"]
                         :u {:synsem {:agr {:number :sing
                                            :person :1st}}}}

    {:p [#"^(\S+)as$"     "$1"]
     :g [#"^([^' ]+r)e$"    "$1as"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}
    {:p [#"^(\S+)as$"     "s'$1"]
     :g [#"^s'(\S+r)e$"     "$1as"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}
    {:p [#"^(\S+)as$"     "se $1"]
     :g [#"^se (\S+r)e$"    "$1as"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}

                        {:p [#"^(\S+)as$"     "$1"]
                         :g [#"^([^' ]+)$"    "$1as"]
                         :u {:synsem {:agr {:number :sing
                                            :person :2nd}}}}
                        {:p [#"^(\S+)as$"     "s'$1"]
                         :g [#"^s'(\S+)$"     "$1as"]
                         :u {:synsem {:agr {:number :sing
                                            :person :2nd}}}}
                        {:p [#"^(\S+)as$"     "se $1"]
                         :g [#"^se (\S+)$"    "$1as"]
                         :u {:synsem {:agr {:number :sing
                                            :person :2nd}}}}



    {:p [#"^(\S+)a$"      "$1re"]
     :g [#"^([^' ]+r)e$"       "$1a"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}
    {:p [#"^(\S+)a$"      "s'$1re"]
     :g [#"^s'(\S+r)e$"     "$1a"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}
    {:p [#"^(\S+)a$"      "se $1re"]
     :g [#"^se (\S+r)e$"    "$1a"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}

                        {:p [#"^(\S+)a$"      "$1"]
                         :g [#"^([^' ]+)$"       "$1a"]
                         :u {:synsem {:agr {:number :sing
                                            :person :3rd}}}}
                        {:p [#"^(\S+)a$"      "s'$1"]
                         :g [#"^s'(\S+)$"     "$1a"]
                         :u {:synsem {:agr {:number :sing
                                            :person :3rd}}}}
                        {:p [#"^(\S+)a$"      "se $1"]
                         :g [#"^se (\S+)$"    "$1a"]
                         :u {:synsem {:agr {:number :sing
                                            :person :3rd}}}}


    {:p [#"^(\S+)ons$"    "$1re"]
     :g [#"^([^' ]+r)e$"    "$1ons"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}
    {:p [#"^(\S+)ons$"    "s'$1re"]
     :g [#"^s' (\S+r)e$"    "$1ons"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}
    {:p [#"^(\S+)ons$"    "se $1re"]
     :g [#"^se (\S+r)e$"    "$1ons"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}

                        {:p [#"^(\S+)ons$"    "$1"]
                         :g [#"^([^' ]+)$"    "$1ons"]
                         :u {:synsem {:agr {:number :plur
                                            :person :1st}}}}
                        {:p [#"^(\S+)ons$"    "s'$1"]
                         :g [#"^s' (\S+)$"    "$1ons"]
                         :u {:synsem {:agr {:number :plur
                                            :person :1st}}}}
                        {:p [#"^(\S+)ons$"    "se $1"]
                         :g [#"^se (\S+)$"    "$1ons"]
                         :u {:synsem {:agr {:number :plur
                                            :person :1st}}}}


    {:p [#"^(\S+)ez$"     "$1re"]
     :g [#"^([^' ]+r)e$"    "$1ez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}
    {:p [#"^(\S+)ez$"     "s'$1re"]
     :g [#"^s'(\S+r)e$"     "$1ez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}
    {:p [#"^(\S+)ez$"     "se $1re"]
     :g [#"^se (\S+r)e$"    "$1ez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}

                        {:p [#"^(\S+)ez$"     "$1"]
                         :g [#"^([^' ]+)$"    "$1ez"]
                         :u {:synsem {:agr {:number :plur
                                            :person :2nd}}}}
                        {:p [#"^(\S+)ez$"     "s'$1"]
                         :g [#"^s'(\S+)$"     "$1ez"]
                         :u {:synsem {:agr {:number :plur
                                            :person :2nd}}}}
                        {:p [#"^(\S+)ez$"     "se $1"]
                         :g [#"^se (\S+)$"    "$1ez"]
                         :u {:synsem {:agr {:number :plur
                                            :person :2nd}}}}


    {:p [#"^(\S+)ont$"    "$1re"]
     :g [#"^([^' ]+r)e$"    "$1ont"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}
    {:p [#"^(\S+)ont$"    "s'$1re"]
     :g [#"^s'(\S+r)e$"     "$1ont"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}
    {:p [#"^(\S+)ont$"    "se $1re"]
     :g [#"^se (\S+r)e$"    "$1ont"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}

                        {:p [#"^(\S+)ont$"    "$1"]
                         :g [#"^([^' ]+)$"    "$1ont"]
                         :u {:synsem {:agr {:number :plur
                                            :person :3rd}}}}
                        {:p [#"^(\S+)ont$"    "s'$1"]
                         :g [#"^s'(\S+)$"     "$1ont"]
                         :u {:synsem {:agr {:number :plur
                                            :person :3rd}}}}
                        {:p [#"^(\S+)ont$"    "se $1"]
                         :g [#"^se (\S+)$"    "$1ont"]
                         :u {:synsem {:agr {:number :plur
                                            :person :3rd}}}}


    ]))

(def imperfect
  (map
   #(merge %
           {:u (unify {:synsem {:infl :imperfect
                                :cat :verb}}
                      (:u %))})
   [
    ;; -ger (e.g. manger -> mangeais)
    {:p [#"^(\S+)eais$"      "$1er"]
     :g [#"^([^' ]+)ger$"    "$1geais"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}
    ;; -cer (e.g. commencer -> commençais)
    {:p [#"^(\S+)eais$"      "$1er"]
     :g [#"^([^' ]+)cer$"    "$1çais"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}
    ;; -er (e.g. parler -> parlais)
    {:p [#"^(\S+)ais$"       "$1er"]
     :g [#"^([^' ]+)er$"     "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}

    ;; -ger (e.g. manger -> mangeais)
    {:p [#"^(\S+)eais$"      "$1er"]
     :g [#"^([^' ]+)ger$"    "$1geais"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}
    ;; -cer (e.g. commencer -> commençais)
    {:p [#"^(\S+)eais$"      "$1er"]
     :g [#"^([^' ]+)cer$"    "$1çais"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}
    {:p [#"^(\S+)ais$"       "$1er"]
     :g [#"^([^' ]+)er$"     "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}

    ;; -ger (e.g. manger -> mangeait)
    {:p [#"^(\S+)eait$"      "$1er"]
     :g [#"^([^' ]+)ger$"    "$1geait"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}
    ;; -cer (e.g. commencer -> commençait)
    {:p [#"^(\S+)eait$"      "$1er"]
     :g [#"^([^' ]+)cer$"    "$1çait"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}
    {:p [#"^(\S+)ait$"       "$1er"]
     :g [#"^([^' ]+)er$"     "$1ait"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}

    ;; étudier -> étudions
    {:p [#"^(\S+)ions$"      "$1ier"]
     :g [#"^([^' ]+)ier$"    "$1ions"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}
    
    {:p [#"^(\S+)ions$"      "$1er"]
     :g [#"^([^' ]+)er$"     "$1ions"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}

    ;; étudier -> étudiez
    {:p [#"^(\S+)iez$"       "$1er"]
     :g [#"^([^' ]+)ier$"    "$1iez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}

    {:p [#"^(\S+)iez$"       "$1er"]
     :g [#"^([^' ]+)er$"     "$1iez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}

    ;; -ger (e.g. manger -> mangeaient)
    {:p [#"^(\S+)eaint$"     "$1er"]
     :g [#"^([^' ]+)ger$"    "$1geaient"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}
    ;; -cer (e.g. commencer -> commençaient)
    {:p [#"^(\S+)eaint$"     "$1er"]
     :g [#"^([^' ]+)cer$"    "$1çaient"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}
    {:p [#"^(\S+)aient$"     "$1er"]
     :g [#"^([^' ]+)er$"     "$1aient"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}

    ;; -ir
    {:p [#"^(\S+)ais$"       "$1ir"]
     :g [#"^([^' ]+)ir$"     "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}
    {:p [#"^(\S+)ais$"       "$1ir"]
     :g [#"^([^' ]+)ir$"     "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}
    {:p [#"^(\S+)ait$"       "$1ir"]
     :g [#"^([^' ]+)ir$"     "$1ait"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}
    {:p [#"^(\S+)ions$"      "$1ir"]
     :g [#"^([^' ]+)ir$"     "$1ions"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}
    {:p [#"^(\S+)iez$"       "$1ir"]
     :g [#"^([^' ]+)ir$"     "$1iez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}
    {:p [#"^(\S+)aient$"       "$1ir"]
     :g [#"^([^' ]+)ir$"     "$1aient"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}
    ;; -re
    {:p [#"^(\S+)ais$"       "$1re"]
     :g [#"^([^' ]+)re$"     "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}
    {:p [#"^(\S+)ais$"       "$1re"]
     :g [#"^([^' ]+)re$"     "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}
    {:p [#"^(\S+)ait$"       "$1re"]
     :g [#"^([^' ]+)re$"     "$1ait"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}
    {:p [#"^(\S+)ions$"      "$1re"]
     :g [#"^([^' ]+)re$"     "$1ions"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}
    {:p [#"^(\S+)iez$"       "$1re"]
     :g [#"^([^' ]+)re$"     "$1iez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}
    {:p [#"^(\S+)aient$"       "$1re"]
     :g [#"^([^' ]+)re$"     "$1aient"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}

    ]))

(def conditional
  (map
   #(merge %
           {:u (unify (:u %)
                      {:synsem {:cat :verb
                                :infl :conditional}})})
   [
    ;; -re verbs: drop the e to form the conditional
    {:p [#"^(\S+)ais$"     "$1re"]
     :g [#"^([^' ]+r)e$"    "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}
    {:p [#"^(\S+)ais$"     "s'$1"]
     :g [#"^s'(\S+r)e$"     "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}
    {:p [#"^(\S+)ais$"     "se $1"]
     :g [#"^se (\S+r)e$"    "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :1st}}}}

    ;; non -re verbs

                        {:p [#"^(\S+)ais$"     "$1"]
                         :g [#"^([^' ]+)$"    "$1ais"]
                         :u {:synsem {:agr {:number :sing
                                            :person :1st}}}}
                        {:p [#"^(\S+)ais$"     "s'$1"]
                         :g [#"^s'(\S+)$"     "$1ais"]
                         :u {:synsem {:agr {:number :sing
                                            :person :1st}}}}
                        {:p [#"^(\S+)ais$"     "se $1"]
                         :g [#"^se (\S+)$"    "$1ais"]
                         :u {:synsem {:agr {:number :sing
                                            :person :1st}}}}

    {:p [#"^(\S+)ais$"     "$1"]
     :g [#"^([^' ]+r)e$"    "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}
    {:p [#"^(\S+)ais$"     "s'$1"]
     :g [#"^s'(\S+r)e$"     "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}
    {:p [#"^(\S+)ais$"     "se $1"]
     :g [#"^se (\S+r)e$"    "$1ais"]
     :u {:synsem {:agr {:number :sing
                        :person :2nd}}}}

                        {:p [#"^(\S+)ais$"     "$1"]
                         :g [#"^([^' ]+)$"    "$1ais"]
                         :u {:synsem {:agr {:number :sing
                                            :person :2nd}}}}
                        {:p [#"^(\S+)ais$"     "s'$1"]
                         :g [#"^s'(\S+)$"     "$1ais"]
                         :u {:synsem {:agr {:number :sing
                                            :person :2nd}}}}
                        {:p [#"^(\S+)ais$"     "se $1"]
                         :g [#"^se (\S+)$"    "$1ais"]
                         :u {:synsem {:agr {:number :sing
                                            :person :2nd}}}}



    {:p [#"^(\S+)ait$"      "$1re"]
     :g [#"^([^' ]+r)e$"       "$1ait"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}
    {:p [#"^(\S+)ait$"      "s'$1re"]
     :g [#"^s'(\S+r)e$"     "$1ait"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}
    {:p [#"^(\S+)ait$"      "se $1re"]
     :g [#"^se (\S+r)e$"    "$1ait"]
     :u {:synsem {:agr {:number :sing
                        :person :3rd}}}}

                        {:p [#"^(\S+)ait$"      "$1"]
                         :g [#"^([^' ]+)$"       "$1ait"]
                         :u {:synsem {:agr {:number :sing
                                            :person :3rd}}}}
                        {:p [#"^(\S+)ait$"      "s'$1"]
                         :g [#"^s'(\S+)$"     "$1ait"]
                         :u {:synsem {:agr {:number :sing
                                            :person :3rd}}}}
                        {:p [#"^(\S+)ait$"      "se $1"]
                         :g [#"^se (\S+)$"    "$1ait"]
                         :u {:synsem {:agr {:number :sing
                                            :person :3rd}}}}


    {:p [#"^(\S+)ions$"    "$1re"]
     :g [#"^([^' ]+r)e$"    "$1ions"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}
    {:p [#"^(\S+)ions$"    "s'$1re"]
     :g [#"^s' (\S+r)e$"    "$1ions"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}
    {:p [#"^(\S+)ions$"    "se $1re"]
     :g [#"^se (\S+r)e$"    "$1ions"]
     :u {:synsem {:agr {:number :plur
                        :person :1st}}}}

                        {:p [#"^(\S+)ions$"    "$1"]
                         :g [#"^([^' ]+)$"    "$1ions"]
                         :u {:synsem {:agr {:number :plur
                                            :person :1st}}}}
                        {:p [#"^(\S+)ions$"    "s'$1"]
                         :g [#"^s' (\S+)$"    "$1ions"]
                         :u {:synsem {:agr {:number :plur
                                            :person :1st}}}}
                        {:p [#"^(\S+)ions$"    "se $1"]
                         :g [#"^se (\S+)$"    "$1ions"]
                         :u {:synsem {:agr {:number :plur
                                            :person :1st}}}}


    {:p [#"^(\S+)iez$"     "$1re"]
     :g [#"^([^' ]+r)e$"    "$1iez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}
    {:p [#"^(\S+)iez$"     "s'$1re"]
     :g [#"^s'(\S+r)e$"     "$1iez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}
    {:p [#"^(\S+)iez$"     "se $1re"]
     :g [#"^se (\S+r)e$"    "$1iez"]
     :u {:synsem {:agr {:number :plur
                        :person :2nd}}}}

                        {:p [#"^(\S+)iez$"     "$1"]
                         :g [#"^([^' ]+)$"    "$1iez"]
                         :u {:synsem {:agr {:number :plur
                                            :person :2nd}}}}
                        {:p [#"^(\S+)iez$"     "s'$1"]
                         :g [#"^s'(\S+)$"     "$1iez"]
                         :u {:synsem {:agr {:number :plur
                                            :person :2nd}}}}
                        {:p [#"^(\S+)iez$"     "se $1"]
                         :g [#"^se (\S+)$"    "$1iez"]
                         :u {:synsem {:agr {:number :plur
                                            :person :2nd}}}}


    {:p [#"^(\S+)aient$"    "$1re"]
     :g [#"^([^' ]+r)e$"    "$1aient"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}
    {:p [#"^(\S+)aient$"    "s'$1re"]
     :g [#"^s'(\S+r)e$"     "$1aient"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}
    {:p [#"^(\S+)aient$"    "se $1re"]
     :g [#"^se (\S+r)e$"    "$1aient"]
     :u {:synsem {:agr {:number :plur
                        :person :3rd}}}}

                        {:p [#"^(\S+)aient$"    "$1"]
                         :g [#"^([^' ]+)$"    "$1aient"]
                         :u {:synsem {:agr {:number :plur
                                            :person :3rd}}}}
                        {:p [#"^(\S+)aient$"    "s'$1"]
                         :g [#"^s'(\S+)$"     "$1aient"]
                         :u {:synsem {:agr {:number :plur
                                            :person :3rd}}}}
                        {:p [#"^(\S+)aient$"    "se $1"]
                         :g [#"^se (\S+)$"    "$1aient"]
                         :u {:synsem {:agr {:number :plur
                                            :person :3rd}}}}


    ]))

(def regular-patterns-source
  (apply concat
         [conditional
          imperfect
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

;; TODO: unify irregular-patterns and irregular-conjugations.
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

   ;; 3. conditional tense exceptions
   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :conditional
                  :conjugated true
                  :français (str (get-in val [:français :future-stem]) "ais")
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :conditional
                  :conjugated true
                  :français (str (get-in val [:français :future-stem]) "ais")
                  :agr {:number :sing
                        :person :2nd}}})}
   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :conditional
                  :conjugated true
                  :français (str (get-in val [:français :future-stem]) "ait")
                  :agr {:number :sing
                        :person :3rd}}})}
   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :conditional
                  :français (str (get-in val [:français :future-stem]) "ions")
                  :agr {:number :plur
                        :person :1st}}})}
   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :conditional
                  :français (str (get-in val [:français :future-stem]) "iez")
                  :conjugated true
                  :agr {:number :plur
                        :person :2nd}}})}

   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :conditional
                  :français (str (get-in val [:français :future-stem]) "aient")
                  :conjugated true
                  :agr {:number :plur
                        :person :3rd}}})}

   ;; 4. future-tense exceptions
   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :future
                  :conjugated true
                  :français (str (get-in val [:français :future-stem]) "ai")
                  :agr {:number :sing
                        :person :1st}}})}

   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :future
                  :val val
                  :got-here true
                  :conjugated true
                  :français (str (get-in val [:français :future-stem]) "as")
                  :agr {:number :sing
                        :person :2nd}}})}

   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :future
                  :conjugated true
                  :français (str (get-in val [:français :future-stem]) "a")
                  :agr {:number :sing
                        :person :3rd}}})}
   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :future
                  :français (str (get-in val [:français :future-stem]) "ons")
                  :agr {:number :plur
                        :person :1st}}})}
   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :future
                  :français (str (get-in val [:français :future-stem]) "ez")
                  :conjugated true
                  :agr {:number :plur
                        :person :2nd}}})}
   {:path [:français :future-stem]
    :merge-fn
    (fn [val]
      {:français {:infl :future
                  :français (str (get-in val [:français :future-stem] "ont"))
                  :conjugated true
                  :agr {:number :plur
                        :person :3rd}}})}

   ;; 5. imperfect irregular with :imperfect-stem
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

   ;; 4. present-tense boot-stem exception: :boot-stem1.
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

;; TODO: unify irregular-patterns (above) and irregular-conjugations.
(def irregular-conjugations
  (concat
   [;; irregular conditional with :future-stem
    {:prefix :future-stem :suffix "ais"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :sing}
                           :infl :conditional}
                  :français {:conditional {:stem true}}}}
    {:prefix :future-stem :suffix "ais"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :sing}
                           :infl :conditional}
                  :français {:conditional {:stem true}}}}
    {:prefix :future-stem :suffix "ait"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :sing}
                           :infl :conditional}
                  :français {:conditional {:stem true}}}}
    {:prefix :future-stem :suffix "ions"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :plur}
                           :infl :conditional}
                  :français {:conditional {:stem true}}}}
    {:prefix :future-stem :suffix "iez"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :plur}
                           :infl :conditional}
                  :français {:conditional {:stem true}}}}
    {:prefix :future-stem :suffix "aient"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :plur}
                           :infl :conditional}
                  :français {:conditional {:stem true}}}}
    ;; irregular future with :future-stem
    {:prefix :future-stem :suffix "ai"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :sing}
                           :infl :future}
                  :français {:future {:stem true}}}}
    {:prefix :future-stem :suffix "as"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :sing}
                           :infl :future}
                  :français {:future {:stem true}}}}
    {:prefix :future-stem :suffix "a"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :sing}
                           :infl :future}
                  :français {:future {:stem true}}}}
    {:prefix :future-stem :suffix "ons"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :plur}
                           :infl :future}
                  :français {:future {:stem true}}}}
    {:prefix :future-stem :suffix "ez"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :plur}
                           :infl :future}
                  :français {:future {:stem true}}}}
    {:prefix :future-stem :suffix "ont"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :plur}
                           :infl :future}
                  :français {:future {:stem true}}}}

    ;; imperfect with :imperfect-stem
    {:prefix :imperfect-stem :suffix "ais"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :sing}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}
    {:prefix :imperfect-stem :suffix "ais"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :sing}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}
    {:prefix :imperfect-stem :suffix "ait"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :sing}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}
    {:prefix :imperfect-stem :suffix "ions"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :plur}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}
    {:prefix :imperfect-stem :suffix "iez"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :plur}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}
    {:prefix :imperfect-stem :suffix "aient"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :plur}
                           :infl :imperfect}
                  :français {:imperfect {:stem true}}}}

    ;; present with :boot-stem1 and :boot-stem2.
    ;; :boot-stem1 is the "boot" itself (1sing,2sing,3sing,3plur)
    ;; whereas :boot-stem2 is the remainder besides the "boot" (1plur,2plur)
    {:prefix :boot-stem1 :suffix "s"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :sing}
                           :infl :present}
                  :français {:present {:boot-stem1 true}}}}

    {:prefix :boot-stem1 :suffix "s"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :sing}
                           :infl :present}
                  :français {:present {:boot-stem1 true}}}}

    {:prefix :boot-stem1 :suffix "t"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :sing}
                           :infl :present}
                  :français {:present {:boot-stem1 true}}}}

    {:prefix :boot-stem2 :suffix "ons"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :1st
                                 :number :plur}
                           :infl :present}
                  :français {:present {:boot-stem2 true}}}}

    {:prefix :boot-stem2 :suffix "ez"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :2nd
                                 :number :plur}
                           :infl :present}
                  :français {:present {:boot-stem2 true}}}}

    {:prefix :boot-stem1 :suffix "ent"
     :unify-with {:synsem {:cat :verb
                           :agr {:person :3rd
                                 :number :plur}
                           :infl :present}
                  :français {:present {:boot-stem1 true}}}}]

   [{:path [:français :past-participle]
     :comment "irregular past participle"
     :unify-with {:français {:past-p {:regular false}}
                  :synsem {:cat :verb
                           :infl :past-p}}}]

   (mapcat (fn [infl]
             [{:path [:français infl :1sing]
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :1st
                                             :number :sing}}
                            :synsem {:cat :verb
                                     :infl infl}}}
              {:path [:français infl :2sing]
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :2nd
                                             :number :sing}}
                            :synsem {:cat :verb
                                     :infl infl}}}
              {:path [:français infl :3sing]
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :3rd
                                             :number :sing}}
                            :synsem {:cat :verb
                                     :infl infl}}}
              {:path [:français infl :1plur]
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :1st
                                             :number :plur}}
                            :synsem {:cat :verb
                                     :infl infl}}}
              {:path [:français infl :2plur]
               :unify-with {:français {infl {:regular false}
                                       :agr {:person :2nd
                                             :number :plur}}
                            :synsem {:cat :verb
                                     :infl infl}}}
              {:path [:français infl :3plur]
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
