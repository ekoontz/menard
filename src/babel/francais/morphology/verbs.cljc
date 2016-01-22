(ns babel.francais.morphology.verbs
  (:refer-clojure :exclude [future get-in merge resolve])
  (:require
   [babel.francais.morphology.nouns :as nouns]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in merge ref? strip-refs unifyc)]))

(def replace-patterns-source
  [;; <reflexive past>: e.g. "amusé" => "s'amuser"
   {:i [#"^([aeéiou]\S+)é$"    "s'$1er"] ;; :i : how to transform a finite form to infinitive
    :c [#"^s'([aeéiou]\S+)er$" "$1é"]  ;; :c : how to transform an infinitive into a finite form.
    :u {:synsem {:infl :past-p ;; :u : what to unify against for these tranformations to occur.
                 :subcat {:1 {:agr {:number :sing
                                    :gender :masc}}}}}}
   {:i [#"^([aeéiou]\S+)és$"   "s'$1er"]
    :c [#"^s'([aeéiou]\S+)er$" "$1és"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :plur
                                    :gender :masc}}}}}}
   {:i [#"^([aeéiou]\S+)ée$"   "s'$1er"]
    :c [#"^s'([aeéiou]\S+)er$" "$1ée"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :sing
                                    :gender :fem}}}}}}
   {:i [#"^([aeéiou]\S+)ées$"  "s'$1er"]
    :c [#"^s'([aeéiou]\S+)er$" "$1ées"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}}}}
   {:i [#"^([^aeéiou]\S+)é$"   "se $1er"]
    :c [#"^se (\S+)er$"        "$1é"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :sing
                                    :gender :masc}}}}}}

   {:i [#"^([^aeéiou]\S+)és$"  "se $1er"]
    :c [#"^se (\S+)er$"        "$1és"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :plur
                                    :gender :masc}}}}}}
   {:i [#"^([^aeéiou]\S+)ée$"  "se $1er"]
    :c [#"^se (\S+)er$"        "$1é"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :sing
                                    :gender :fem}}}}}}
   {:i [#"^([^aeéiou]\S+)ées$" "se $1er"]
    :c [#"^se (\S+)er$"        "$1ées"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}}}}
   ;; </reflexive past>

   ;; <non-reflexive past>: e.g. "parlé" => "parler"
   ;; singular: could be masculine or feminine
   {:i [#"^(\S+)é$"            "$1er"]
    :c [#"^(\S+)er$"           "$1é"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :sing}}}}}}

   ;; plural: could be masculine or feminine
   {:i [#"^(\S+)és$"           "$1er"]
    :c [#"^(\S+)er$"           "$1é"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :plur}}}}}}
   
   ;; singular feminine
   {:i [#"^(\S+)ée$"           "$1er"]
    :c [#"^(\S+)er$"           "$1ée"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :sing
                                    :gender :fem}}}}}}
   ;; plural feminine
   {:i [#"^(\S+)ées$"          "$1er"]
    :c [#"^(\S+)er$"           "$1ée"]
    :u {:synsem {:infl :past-p
                 :subcat {:1 {:agr {:number :plur
                                    :gender :fem}}}}}}
   ;; </non-reflexive past>

   ;; <reflexive present -er and -ir type verbs>
   {:i [#"^([aeéiou]\S+)e$"    "s'$1er"]
    :c [#"^s'(\S+)[ie]r$"      "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}

   {:i [#"^([aeéiou]\S+)es$"   "s'$1er"]
    :c [#"^s'(\S+)[ie]r$"      "$1es"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:i [#"^([aeéiou]\S+)e$"    "s'$1er"]
    :c [#"^s'(\S+)[ie]r$"      "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}

   {:i [#"^([aeéiou]\S+)ons$"  "s'$1er"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:i [#"^([aeéiou]\S+)ez$"   "s'$1er"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:i [#"^([aeéiou]\S+)ent$"  "s'$1er"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
  
   {:i [#"^([^aeéiou]\S+)e$"   "se $1er"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}

   {:i [#"^([^aeéiou]\S+)es$"  "se $1er"]
    :c [#"^se (\S+)[ie]r$"     "$1es"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
  
   {:i [#"^([^aeéiou]\S+)e$"   "se $1er"]
    :c [#"^se (\S+)[ie]r$"     "$1es"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}

   {:i [#"^([^aeéiou]\S+)ons$" "s'$1er"]
    :c [#"^se (\S+)[ie]r$"     "$1ons"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:i [#"^([^aeéiou]\S+)ez$"  "s'$1er"]
    :c [#"^se (\S+)[ie]r$"     "$1ez"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
              :infl :present}}}

   {:i [#"^([^aeéiou]\S+)ent$" "s'$1er"]
    :c [#"^se (\S+)[ie]r$"     "$1ent"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   
   ;; </reflexive present -er and -ir type verbs>

   ;; <present non-reflexive>
   ;; <-er verbs>
   {:i [#"^(\S+)e$"            "$1er"]
    :c [#"^(\S+)er$"           "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}

   {:i [#"^(\S+)es$"           "$1er"]
    :c [#"^(\S+)er$"           "$1es"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:i [#"^(\S+)e$"            "$1er"]
    :c [#"^([^ ']+)er$"           "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   
   {:i [#"^(\S+)ons$"          "$1er"]
    :c [#"^(\S+)er$"           "$1ons"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:i [#"^(\S+)ez$"           "$1er"]
    :c [#"^(\S+)er$"           "$1ez"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:i [#"^(\S+)ent$"          "$1er"]
    :c [#"^(\S+)er$"           "$1ent"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   ;; </-er verbs>

   ;; <ir verbs>
   {:i [#"^(\S+)e$"            "$1ir"]
    :c [#"^(\S+)ir$"           "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}

   {:i [#"^(\S+)es$"           "$1ir"]
    :c [#"^(\S+)ir$"           "$1es"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:i [#"^(\S+)e$"            "$1ir"]
    :c [#"^(\S+)ir$"           "$1e"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   
   {:i [#"^(\S+)ons$"          "$1ir"]
    :c [#"^(\S+)ir$"           "$1ons"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:i [#"^(\S+)ez$"           "$1ir"]
    :c [#"^(\S+)ir$"           "$1ez"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:i [#"^(\S+)ent$"          "$1ir"]
    :c [#"^(\S+)ir$"           "$1ent"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   ;; </ir verbs>   
   ;; </present non-reflexive>

   ])

;; this (map..) is needed because a word within a syntactic tree will be conjugated
;; as a part of :a or :b within the tree, and with its :infl and :agr as keys within
;; the :francais part of the verb.
(def replace-patterns
  (map (fn [pattern]
         {:i (:i pattern)
          :c (:c pattern)
          :u (unifyc (unifyc (:u pattern)
                             (let [agr (atom :top)
                                   infl (atom :top)]
                               {:synsem {:infl infl
                                         :subcat {:1 {:agr agr}}}
                                :infl infl
                                :agr agr})))})
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
    
(defn conditional [word]
  (let [infinitive (reflexive-to-infinitive (get-in word [:français]))
        ar-type (try (re-find #"ar$" infinitive)
                     (catch Exception e
                       (exception (str "Can't regex-find on non-string: " infinitive " from word: " word))))
        er-type (re-find #"[eé]r$" infinitive)
        ir-type (re-find #"ir$" infinitive)
        re-type (re-find #"re$" infinitive)
        stem (string/replace infinitive #"i?[iaeé]r$" "")
        stem (if re-type
               ;; prendre -> prend
               (string/replace infinitive #"re$" "")
               stem)
        stem (if (get-in word [:future-stem])
               (get-in word [:future-stem])
               stem)
        last-stem-char-is-i (re-find #"ir$" infinitive)
        last-stem-char-is-e (re-find #"er$" infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))
        number-and-person (number-and-person number person)]
    (cond
     (get-in word [:conditional number-and-person])
     (get-in word [:conditional number-and-person])

     (and (= person :1st) (= number :sing) er-type)
     (str stem "erais")
     (and (= person :1st) (= number :sing) ir-type)
     (str stem "irais")
     (and (= person :1st) (= number :sing) re-type)
     (str stem "rais")

     (and (= person :2nd) (= number :sing) ir-type)
     (str stem "erais")
     (and (= person :2nd) (= number :sing) er-type)
     (str stem "irais")
     (and (= person :2nd) (= number :sing) re-type)
     (str stem "rais")
    
     (and (= person :3rd) (= number :sing) ir-type)
     (str stem "erait")
     (and (= person :3rd) (= number :sing) er-type)
     (str stem "irait")
     (and (= person :3rd) (= number :sing) re-type)
     (str stem "rait")
     
     (and (= person :1st) (= number :plur) er-type)
     (str stem "erions")
     (and (= person :1st) (= number :plur) ir-type)
     (str stem "irions")
     (and (= person :1st) (= number :plur) re-type)
     (str stem "rions")

     (and (= person :2nd) (= number :plur) er-type)
     (str stem "eriez")
     (and (= person :2nd) (= number :plur) ir-type)
     (str stem "iriez")
     (and (= person :2nd) (= number :plur) re-type)
     (str stem "riez")
     ;; </second person plural conditional>

     ;; <third person plural conditional>
     (and (= person :3rd) (= number :plur)
          er-type)
     (str stem "eraient")
     (and (= person :3rd) (= number :plur)
          ir-type)
     (str stem "iraient")
     (and (= person :3rd) (= number :plur)
          re-type)
     (str stem "raient")
     ;; </third person plural conditional>

     :else
     (let [message (str "get-string-1: conditional regular inflection: don't know what to do with input argument: " (strip-refs word))]
       (if (= true suppress-incomplete-morphology-errors)
         (do (log/warn message)
             "(" (get-in word [:francais]) ")")
         (exception message))))))

(defn future [word]
  (let [infinitive (reflexive-to-infinitive (get-in word '(:français)))
        ar-type (try (re-find #"ar$" infinitive)
                     (catch Exception e
                       (exception (str "Can't regex-find on non-string: " infinitive " from word: " word))))
        er-type (re-find #"[eé]r$" infinitive)
        ir-type (re-find #"ir$" infinitive)
        re-type (re-find #"re$" infinitive)
        stem (string/replace infinitive #"[iaeé]r$" "")
        stem (if re-type
               ;; prendre -> prend
               (string/replace infinitive #"re$" "")
               stem)
        stem (if (get-in word [:future-stem])
               (get-in word [:future-stem])
               stem)
        last-stem-char-is-i (re-find #"ir$" infinitive)
        last-stem-char-is-e (re-find #"er$" infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))
        number-and-person (number-and-person number person)]
    (cond
     (get-in word [:future number-and-person])
     (get-in word [:future number-and-person])
     
     (and (= person :1st) (= number :sing) er-type)
     (str stem "erai")
     (and (= person :1st) (= number :sing) ir-type)
     (str stem "iré")
     (and (= person :1st) (= number :sing) re-type)
     (str stem "rai")
     
     (and (= person :2nd) (= number :sing) ir-type)
     (str stem "iras")
     (and (= person :2nd) (= number :sing) er-type)
     (str stem "eras")
     (and (= person :2nd) (= number :sing) re-type)
     (str stem "ras")
                            
     (and (= person :3rd) (= number :sing) ir-type)
     (str stem "ira")
     (and (= person :3rd) (= number :sing) er-type)
     (str stem "era")
     (and (= person :3rd) (= number :sing) re-type)
     (str stem "ra")
              
     (and (= person :1st) (= number :plur) er-type)
     (str stem "erons")
     (and (= person :1st) (= number :plur) ir-type)
     (str stem "irons")
     (and (= person :1st) (= number :plur) re-type)
     (str stem "rons")
              
     ;; <second person plural future>
     (and (= person :2nd) (= number :plur) er-type)
     (str stem "erez")
     (and (= person :2nd) (= number :plur) ir-type)
     (str stem "irez")
     (and (= person :2nd) (= number :plur) re-type)
     (str stem "rez")
     ;; </second person plural future>

     ;; <third person plural future>
     (and (= person :3rd) (= number :plur)
          er-type)
     (str stem "eront")
     (and (= person :3rd) (= number :plur)
          ir-type)
     (str stem "iront")
     (and (= person :3rd) (= number :plur)
          re-type)
     (str stem "ront")
     ;; </third person plural future>

     (and (or (= :top (get-in word [:agr]))
              (= :top person)
              (= :top (get-in word [:agr :gender]))
              (= :top number))
          (string? (get-in word [:français])))
     (get-in word [:français])

     :else
     (let [message (str "get-string: future regular inflection: don't know what to do with input argument: " (strip-refs word))]
       (if (= true suppress-incomplete-morphology-errors)
         (do (log/warn message)
             (exception message)))))))

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

(defn present [word]
  (let [infinitive (reflexive-to-infinitive (get-in word [:français]))
        ar-type (try (re-find #"ar$" infinitive)
                     (catch Exception e
                       (exception (str "Can't regex-find on non-string: " infinitive " from word: " word))))
        er-type (re-find #"[eé]r$" infinitive)
        ir-type (re-find #"ir$" infinitive)
        re-type (re-find #"re$" infinitive)
        stem (string/replace infinitive #"[iaeé]r$" "")
        stem (if re-type
               (string/replace infinitive #"re$" "")
               stem)
        last-stem-char-is-i (re-find #"ir$" infinitive)
        last-stem-char-is-e (re-find #"er$" infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))
        g-stem (re-find #"[g]er$" infinitive)
        number-and-person (number-and-person number person)
        ]
    (cond
      (and number-and-person
           (get-in word [:present number-and-person]))
      (get-in word [:present number-and-person])

      
     ;;QUI COMINCIANO I VERBI FRANCESI REGOLARI
     (and (= person :1st) (= number :sing) er-type)
     (str stem "e")

     (and (= person :1st) (= number :sing) ir-type)
     (str stem "is")

     (and (= person :1st) (= number :sing) re-type)
     (str stem "")

     (and (= person :2nd) (= number :sing) er-type)
     (str stem "es")

     (and (= person :2nd) (= number :sing) ir-type)
     (str stem "is")

     (and (= person :2nd) (= number :sing) re-type)
     (str stem "s")

     (and (= person :3rd) (= number :sing) er-type)
     (str stem "e")

     (and (= person :3rd) (= number :sing) ir-type)
     (str stem "it")

     (and (= person :3rd) (= number :sing) re-type)
     (str stem "")

     (and (= person :1st) (= number :plur) er-type g-stem)
     (str stem "eons")

     (and (= person :1st) (= number :plur) er-type)
     (str stem "ons")

     (and (= person :1st) (= number :plur) ir-type)
     (str stem "issons")

     (and (= person :1st) (= number :plur) re-type)
     (let [stem (string/replace stem #"d$" "")]
       (str stem "ons"))

     ;; <second person plural present>
     (and (= person :2nd) (= number :plur) er-type)
     (str stem "ez")

     (and (= person :2nd) (= number :plur) ir-type)
     (str stem "issez")  

     (and (= person :2nd) (= number :plur) re-type)
     (str stem "ez")  

     ;; </second person plural present>

     ;; <third person plural present>
     (and (= person :3rd) (= number :plur)
          er-type)
     (str stem "ent")
     (and (= person :3rd) (= number :plur)
          ir-type)
     (str stem "issent")
     (and (= person :3rd) (= number :plur)
          re-type)
     (let [stem (string/replace stem #"d$" "")]
       (str stem "nent"))

     ;; </third person plural present>

     ;; agreement is underspecified, but an infinitive form (the :français key) exists, so just return that infinitive form.
     (and (= (get-in word [:agr]) :top)
          (string? (get-in word [:français])))
     (get-in word [:français])

     (and (or (= :top person)
              (= :top (get-in word [:agr :gender]))
              (= :top number))
           (string? (get-in word [:français])))
     (get-in word [:français])

     :else
     (exception (str "get-string: present regular inflection: don't know what to do with input argument: "
                     (strip-refs word)
                     " (er-type: " er-type "; ir-type: " ir-type "; re-type: " re-type)))))

(defn passe-compose [word]
  (let [infinitive (reflexive-to-infinitive (get-in word '(:français)))]
    (if-let [irregular (get-in word [:past-participle])]
      irregular

      ;; else, no :past-participle irregular form found, so use regular.
      (let [essere (get-in word [:essere])
            er-type (re-find #"[ée]r$" infinitive) ;; c.f. italiano -are
            re-type (re-find #"re$" infinitive) ;; c.f. italian -ere
            ir-type (re-find #"ir$" infinitive) ;; c.f. italian -ire
            stem (string/replace infinitive #"[iaeé]r$" "")
            stem (if re-type
                   (string/replace infinitive #"re$" "")
                   stem)
            last-stem-char-is-i (re-find #"ir$" infinitive)
            last-stem-char-is-e (re-find #"er$" infinitive)
            person (get-in word '(:agr :person))
            number (get-in word '(:agr :number))
            gender-suffix
            (cond (= :fem (get-in word [:agr :gender]))
                  "e"
                  true
                  "")

            number-suffix
            (cond (= :plur (get-in word [:agr :number]))
                  "s"
                  true
                  "")

            verb-type-suffix
            (cond er-type
                  "é"
                  re-type
                  "u"
                  true
                  "i")]        
        ;; regular
        (str stem verb-type-suffix
             (if essere
               gender-suffix "")
             (if essere
               number-suffix ""))))))

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
