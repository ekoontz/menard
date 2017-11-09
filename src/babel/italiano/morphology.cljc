(ns babel.italiano.morphology
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [babel.pos :refer (noun)]
   [babel.italiano.morphology.adjectives :as adjectives]
   [babel.italiano.morphology.adjectives
    :refer (plural-to-singular-adj-masc
            plural-to-singular-adj-fem-sing
            plural-to-singular-adj-fem-plur)]
   [babel.italiano.morphology.determiners :as determiners]
   [babel.italiano.morphology.nouns :as nouns]
   [babel.italiano.morphology.nouns
    :refer (plural-to-singular-noun-fem-1
            plural-to-singular-noun-masc-1
            plural-to-singular-noun-masc-2)]
   [babel.italiano.morphology.verbs :as verbs]
   [babel.stringutils :refer (replace-from-list)]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unify)]))

;; TODO: convert all morphology rules to this format used for prepositions:
(defonce preposition-plus-article
  [["a il"   "al"]
   ["a lo"   "allo"]
   ["a la"   "alla"]
   ["a l'"   "all'"]
   ["a i"    "ai"]
   ["a gli"  "agli"]
   ["a le"   "alle"]
   
   ["da il"  "dal"]
   ["da lo"  "dallo"]
   ["da la"  "dalla"]
   ["da l'"  "dall'"]
   ["da i"   "dai"]
   ["da gli" "dagli"]
   ["da le"  "dalle"]

   ["de il"  "del"]
   ["de lo"  "dello"]
   ["de la"  "della"]
   ["de l'"  "dell'"]
   ["de i"   "dei"]
   ["de gli" "degli"]
   ["de le"  "delle"]

   ["di il"  "del"]
   ["di lo"  "dello"]
   ["di la"  "della"]
   ["di l'"  "dell'"]
   ["di i"   "dei"]
   ["di gli" "degli"]
   ["di le"  "delle"]

   ["in il"  "nel"]
   ["in lo"  "nello"]
   ["in la"  "nella"]
   ["in l'"  "nell'"]
   ["in i"   "nei"]
   ["in gli" "negli"]
   ["in le"  "nelle"]

   ["su il"  "sul"]
   ["su lo"  "sullo"]
   ["su la"  "sulla"]
   ["su l'"  "sull'"]
   ["su i"   "sui"]
   ["su gli" "sugli"]
   ["su le"  "sulle"]
      ])


;; TODO: pre-compile these rules rather than building regexp objects at runtime.
(defn apply-one-rule [string from-to-pair]
  (let [from (second from-to-pair)
        to (first from-to-pair)]
    (let [from-pattern (re-pattern
                        (str "\\b" from "\\b"))]
      (string/replace string from-pattern to))))

(defn replace-over [strings]
  (let [result (set (reduce concat
                            (map (fn [string]
                                   (map #(apply-one-rule string %)
                                        preposition-plus-article))
                                 strings)))]
    (if (not (= result strings))
      (replace-over result)
      strings)))

(defn tokenize-prepositions-in [string & [match-pairs]]
  string)
  
;; replace-patterns are declarative data that determine how analysis (and soon conjugation) are performed.
(defonce replace-patterns
  (concat
   adjectives/replace-patterns
   determiners/replace-patterns
   nouns/replace-patterns
   verbs/replace-patterns))

(declare get-string)

;; TODO: this is an overly huge method that needs to be rewritten to be easier to understand and maintain.
;; TODO: use replace patterns instead (as with French)
;; TODO: split into morphology/verb function, morphology/noun function, etc.
(defn get-string-1 [word]
  (if (seq? word)
    (map (string/join " " #(get-string-1 %))
         word)
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (log/trace "get-string-1: input word: " word)

    (log/debug (str "word's a is a string? " (get-in word '(:a)) " => " (string? (get-in word '(:a)))))
    (log/debug (str "word's b is a map? " (get-in word '(:b)) " => " (map? (get-in word '(:b)))))

    (log/debug (str "word's a italian is a string? " (get-in word '(:a :italiano)) " => " (string? (get-in word '(:a :italiano)))))

    (cond

     (= word :top) ".."

     (ref? word)
     (get-string-1 @word)

     ;; TODO: this is a special case that should be handled below instead
     ;; of forcing every input to go through this check.
     (= word {:initial false})
     ".."
     (= word {:initial true})
     ".."

     (and (string? (get-in word '(:a)))
          (string? (get-in word '(:b))))
     (get-string (get-in word '(:a))
                 (get-in word '(:b)))

     (and (string? (get-in word '(:a)))
          (map? (get-in word '(:b))))
     (get-string (get-in word '(:a))
                 (get-in word '(:b)))

     (and (map? (get-in word '(:a)))
          (map? (get-in word '(:b))))
     (get-string
      (get-in word '(:a))
      (get-in word '(:b)))

     ;; TODO: this rule is pre-empting all of the following rules
     ;; that look in :a and :b. Either remove those following rules
     ;; if they are redundant and not needed, or move this general rule
     ;; below the following rules.
     (and (not (= :none (get-in word '(:a) :none)))
          (not (= :none (get-in word '(:b) :none))))
     (get-string (get-in word '(:a))
                 (get-in word '(:b)))

     (and
      (string? (get-in word '(:a :italiano)))
      (string? (get-in word '(:b :italiano)))
      (or (= :none (get-in word '(:b :agr :number) :none))
          (= :top (get-in word '(:b :agr :number) :none)))
      )
     (str (string/trim (get-in word '(:a :italiano)))
          " "
          (string/trim (get-in word '(:b :italiano))))

     (and
      (string? (get-in word '(:a)))
      (string? (get-in word '(:b :italiano)))
      (or (= :none (get-in word '(:b :agr :number) :none))
          (= :top (get-in word '(:b :agr :number) :none)))
      )
     (str (string/trim (get-in word '(:a)))
          " "
          (string/trim (get-in word '(:b :italiano))))

     (and
      (string? (get-in word '(:a :italiano)))
      (get-in word '(:a :italiano))
      (or (= :none (get-in word '(:b :agr :number) :none))
          (= :top (get-in word '(:b :agr :number) :none)))
      (= (get-in word '(:a :infl)) :top))
     (string/trim (str (get-in word '(:a :italiano))
                 " " (get-string-1 (get-in word '(:b)))))

     ;; TODO: all of the rules that handle exceptions should be removed:
     ;; exceptions are dealt with at compile-time now, via babel.italiano.morphology/exception-generator

     ;; handle lexical exceptions (plural feminine adjectives):
     (and
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:cat)) :adjective)
      (string? (get-in word '(:fem :plur))))
     (get-in word '(:fem :plur))

     ;; handle lexical exceptions (plural feminine adjectives):
     (and
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:cat)) :adjective)
      (string? (get-in word '(:fem :plur))))
     (get-in word '(:fem :plur))

     ;; handle lexical exceptions (plural masculine adjectives):
     (and
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:agr :gender)) :masc)
      (= (get-in word '(:cat)) :adjective)
      (string? (get-in word '(:masc :plur))))
     (get-in word '(:masc :plur))

     (and
      (string? (get-in word [:italiano]))
      (or (= (get-in word [:agr :gender]) :masc)
          (= (get-in word [:agr :gender]) :top))
      (= (get-in word [:agr :number]) :plur)
      (= (get-in word [:cat]) :adjective))
     (string/replace (get-in word '[:italiano])
                     #"[eo]$" "i") ;; nero => neri

     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :adjective)
      (re-find #"o$" (get-in word [:italiano])))
     (string/replace (get-in word '(:italiano))
                     #"[o]$" "e") ;; nero => nere
     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :adjective)
      (re-find #"e$" (get-in word [:italiano])))
     (string/replace (get-in word '(:italiano))
                     #"[e]$" "i") ;; difficile => difficili
     
     ;; handle lexical exceptions (plural nouns):
     (and
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :noun)
      (string? (get-in word '(:plur))))
     (get-in word '(:plur))

     ;; regular masculine nouns
     (and
      (string? (get-in word [:italiano]))
      (= (get-in word '(:agr :gender)) :masc)
      (= (get-in word '(:agr :number)) :plur)
      (= :noun (get-in word '(:cat)))
      (not (= true (get-in word '(:pronoun))))
      (get-in word '(:italiano)))
     (string/replace (get-in word '(:italiano))
                     #"[eo]$" "i") ;; dottore => dottori; medico => medici

     ;; regular feminine nouns ending in 'e':
     (and
      (string? (get-in word [:italiano]))
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :noun)
      (get-in word '(:italiano))
      (re-find #"e$" (get-in word [:italiano])))
     (string/replace (get-in word '(:italiano))
                     #"[e]$" "i") ;; madre => madri

     ;; regular feminine nouns not ending in 'e'
     (and
      (string? (get-in word [:italiano]))
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :noun)
      (get-in word '(:italiano)))
     (string/replace (get-in word '(:italiano))
                     #"[aà]$" "e") ;; donna => donne

     ;; TODO: move this down to other adjectives.
     ;; this was moved up here to avoid
     ;; another rule from matching it.
     (and
      (string? (get-in word [:italiano]))
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :adjective))
     (string/replace (get-in word '(:italiano))
                     #"[eo]$" "e") ;; nero => nere

     ;; TODO: move this down to other adjectives.
     ;; this was moved up here to avoid
     ;; another rule from matching it.
     ;; exceptional feminine singular adjectives
     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :sing)
      (= (get-in word '(:cat)) :adjective)
      (string? (get-in word '(:fem :sing))))
     (get-in word '(:fem :sing))

     ;; TODO: move this down to other adjectives.
     ;; this was moved up here to avoid
     ;; another rule from matching it.
     ;; regular feminine singular adjectives
     (and
      (string? (get-in word [:italiano]))
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :sing)
      (= (get-in word '(:cat)) :adjective))
     (string/replace (get-in word '(:italiano))
                     #"[eo]$" "a") ;; nero => nera
     
     (and (= :infinitive (get-in word '(:infl)))
          (string? (get-in word '(:italiano))))
     (get-in word '(:italiano))

     (verbs/handle1? word)
     (verbs/handle1 word)

     (verbs/handle2? word)
     (verbs/handle2 word)

     (verbs/handle3? word)
     (verbs/handle3 word)

     (verbs/handle4? word)
     (verbs/handle4 word)

     (verbs/handle5? word)
     (verbs/handle5 word)

     ;; irregular imperfect sense:
     ;; 1) use irregular based on number and person.
     (verbs/handle6? word)
     (verbs/handle6 word)

     (verbs/handle7? word)
     (verbs/handle7 word)

     (verbs/handle8? word)
     (verbs/handle8 word)

     (verbs/handle9? word)
     (verbs/handle9 word)

     (verbs/handle10? word)
     (verbs/handle10 word)

     (verbs/handle11? word)
     (verbs/handle11 word)

     (and
      (get-in word '(:a))
      (get-in word '(:b))
      true) (str
             (trim (get-string-1 (get-in word '(:a)))) " "
             (trim (get-string-1 (get-in word '(:b)))))

     (verbs/handle12? word)
     (verbs/handle12 word)

     (verbs/handle13? word)
     (verbs/handle13 word)

     (verbs/handle14? word)
     (verbs/handle14 word)

     (verbs/handle15? word)
     (verbs/handle15 word)
     
     (verbs/handle16? word)
     (verbs/handle16 word)

     (verbs/handle17? word)
     (verbs/handle17 word)

     (verbs/handle18? word)
     (verbs/handle18 word)

     (verbs/handle19? word)
     (verbs/handle19 word)

     (verbs/handle20? word)
     (verbs/handle20 word)

     (verbs/handle21? word)
     (verbs/handle21 word)

     (verbs/handle22? word)
     (verbs/handle22 word)

     (verbs/handle23? word)
     (verbs/handle23 word)

     (verbs/handle24? word)
     (verbs/handle24 word)

     (verbs/handle25? word)
     (verbs/handle25 word)

     (verbs/handle26? word)
     (verbs/handle26 word)

     (verbs/handle27? word)
     (verbs/handle27 word)

     (and
      (string? (get-in word '(:italiano)))
      (= :top (get-in word '(:agr :sing) :top)))
     (str (get-in word '(:italiano)))

     ;; TODO: possibly remove this: not sure it's doing anything.
     (= true (get-in word [:exception]))
     (get-in word [:italiano])

     (= (get-in word '(:infl)) :top)
     (str (get-in word '(:italiano)))

     (and
      (get-in word '(:a))
      (get-in word '(:b)))
     (get-string
      (get-in word '(:a))
      (get-in word '(:b)))

     (= (get-in word '(:a)) :top)
     (str
      ".." " " (get-string-1 (get-in word '(:b))))

     (and
      (= (get-in word '(:b)) :top)
      (string? (get-string-1 (get-in word '(:a)))))
     (str
      (get-string-1 (get-in word '(:a)))
      " " "..")

     (and
      (= (get-in word '(:b)) :top)
      (string? (get-in word '(:a :italiano))))
     (str
      (get-string-1 (get-in word '(:a :italiano)))
      " " "..")

     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :sing)
      (= (get-in word '(:cat)) :noun))
     (get-in word '(:italiano))

     (and
      (= (get-in word '(:agr :gender)) :masc)
      (= (get-in word '(:agr :number)) :sing)
      (= (get-in word '(:cat) :adjective)))
     (get-in word '(:italiano)) ;; nero

     (and
      (= (get-in word '(:agr :gender)) :masc)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :adjective)
      ;; handle lexical exceptions.
      (string? (get-in word '(:masc :plur))))
     (get-in word '(:masc :plur))

     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :adjective)
      ;; handle lexical exceptions.
      (string? (get-in word '(:fem :plur))))
     (get-in word '(:fem :plur))
     
     (string? (get-in word '(:italiano)))
     (get-in word '(:italiano))

     (or
      (not (= :none (get-in word '(:a) :none)))
      (not (= :none (get-in word '(:b) :none))))
     (get-string (get-in word '(:a))
                 (get-in word '(:b)))

     (and (map? word)
          (nil? (:italiano word)))
     ".."

     (or
      (= (get-in word '(:case)) {:not :acc})
      (= (get-in word '(:agr)) :top))
     ".."

     ;; TODO: throw exception rather than returning _word_, which is a map or something else unprintable.
     ;; in other words, if we've gotten this far, it's a bug.
     :else
     word)
    )))

;; TODO: replace 'a' and 'b' with 'left' and 'right': latter easier to talk about
(defn get-string [a & [ b ]]
  (cond (and (nil? b)
             (seq? a))
        (let [result (get-string-1 a)]
          (if (string? result)
            (trim result)
            result))
        
        true
        (let [a (if (nil? a) "" a)
              b (if (nil? b) "" b)
              a (get-string-1 a)
              b (get-string-1 b)
              info-a (log/debug (str "get-string: a: " a))
              info-b (if b (log/debug (str "get-string: b: " b)))

              it-b (log/debug "it-a is string? " (string? (get-in a '(:italiano))))
              it-b (log/debug "it-b is string? " (string? (get-in b '(:italiano))))

              cat-a (log/debug (str "cat a:" (get-in a '(:cat))))
              cat-b (log/debug (str "cat b:" (get-in b '(:cat))))

              ]
          (cond

           (and (= a "gli")
                (string? (get-in b '(:italiano)))
                (not (or (re-find #"^[aeiou]" (get-in b '(:italiano)))
                         (re-find #"^s[t]" (get-in b '(:italiano))))))
           (trim (str "i " b))

           (and (= a "i")
                (string? (get-in b '(:italiano)))
                (or (re-find #"^[aeiou]" (get-in b '(:italiano)))
                    (re-find #"^s[t]" (get-in b '(:italiano)))))
           (trim (str "gli " b))

           (and (= a "il")
                (string? b)
                (or (re-find #"^[aeiou]" b)
                    (re-find #"^s[t]" b)))
           (trim (str "lo " b))

           (and (= a "lo")
                (string? b)
                (not (or (re-find #"^[aeiou]" b)
                         (re-find #"^s[t]" b))))
           (trim (str "il " b))
           
           ;; TODO: cleanup & remove.
           (and false ;; going to throw out this logic: will use :initial and rule schemata instead.
                (= :verb (get-in a '(:cat)))
                (= :noun (get-in b '(:cat)))
                (= :acc (get-in b '(:case))))
           ;; flip order in this case:
           ;; i.e. "vedo ti" => "ti vedo".
           {:a (if (nil? b) :top b)
            :b (if (nil? a) :top a)}

           (and (string? a)
                (= a "di")
                (string? b)
                (re-find #"^il (mio|tio|suo|nostro|vostro|loro)\b" b))
           (str a " " (string/replace b #"^il " ""))

           (and (string? a)
                (= a "di")
                (string? b)
                (re-find #"^la (mia|tia|sua|nostra|vostra|loro)\b" b))
           (str a " " (string/replace b #"^la " ""))

           (and (string? a)
                (= a "di")
                (string? b)
                (re-find #"^i (miei|tuoi|suoi|nostri|vostri|loro)\b" b))
           (str a " " (string/replace b #"^i " ""))

           (and (string? a)
                (= a "di")
                (string? b)
                (re-find #"^le (mie|tue|sue|nostre|vostre|loro)\b" b))
           (str a " " (string/replace b #"^le " ""))

           (and (= a "di i")
                (string? b)
                (re-find #"^[aeiou]" b))
           (str "degli " b)

           (and (= a "di i")
                (string? b)
                (re-find #"^s[t]" b))
           (str "degli " b)

           (and (= a "di i")
                (string? b))
           (str "dei " b)

           (and (= (get-in a '(:italiano)) "di i")
                (string? b))
           (str "dei " b)

           (and (= (get-in a '(:italiano)) "di i")
                (string? (get-in b '(:italiano))))
           (str "dei " (get-string-1 (get-in b '(:italiano))))

           (and (= a "di il")
                (string? b))
           (get-string "del" b)  ;; allows this to feed next rule:

           (and (= a "del")
                (string? b)
                (re-find #"^[aeiou]" b))
           (str "dell'" b)

           (and (= a "di la")
                (string? b))
           (get-string "della" b) ;; allows this to feed next rule:

           (and (= a "della")
                (string? b)
                (re-find #"^[aeiou]" b))
           (str "dell'" b)

           (and (= a "di le")
                (string? b))
           (str "delle " b)

           (and (= a "i")
                (string? b)
                (re-find #"^[aeiou]" b))
           (str "gli " b)

           (and (= a "i")
                (string? (get-in b '(:italiano)))
                (re-find #"^[aeiou]" (get-in b '(:italiano))))
           (str "gli " b)

           (and (= a "i")
                (string? b)
                (re-find #"^s[t]" b))
           (str "gli " b)

           (and (= a "ci")
                (string? b)
                (re-find #"^[eè]" b))
           (str "c'" b)
           
           ;; handle e.g. "io lo ho visto" => "io l'ho visto"
           (and (string? a)
                (re-find #"^l[ao]$" a)
                (string? b)
                (re-find #"^[aeiouh]" b))
           (str "l'" b)

           ;; 4) handle e.g. "aiutari + ti" => "aiutarti"
           (and (string? a)
                (or (re-find #"are$" a)
                    (re-find #"ere$" a)
                    (re-find #"ire$" a))
                (or (= b "ci")
                    (= b "mi")
                    (= b "la")
                    (= b "le")
                    (= b "li")
                    (= b "lo")
                    (= b "ti")
                    (= b "vi")))
           (str (string/replace a #"[e]$" "")
                b)
           
           (and (= a "un")
                (string? b)
                (re-find #"^s[t]" b))
           (str "uno " b)

           (and (= a "una")
                (string? b)
                (re-find #"^[aeiou]" b))
           (str "un'" b)

           (and (= a "il")
                (string? b)
                (re-find #"^[aeiou]" b))
           (str "l'" b)

           (and (= a "il")
                (string? b)
                (re-find #"^s[ct]" b))
           (str "lo " b)

           (and (= a "la")
                (string? b)
                (re-find #"^[aeiou]" b))
           (str "l'" b)

           (and (= a "quell[ao]")
                (string? b)
                (re-find #"^[aeiou]" b))
           (str "quell'" b)

           (and (= a "quelli")
                (string? b)
                (re-find #"^(st|sc|[aeiou])" b))
           (str "quegli " b)

           (and (= a "quest[aeio]")
                (string? b)
                (re-find #"^[aeiou]" b))
           (str "quest'" b)

           ;; prepositional phrases
           (and (= a "a")
                (string? b)
                (re-find #"^il " b))
           (str "al " (string/replace b #"^il " ""))

           (and (= a "a")
                (string? b)
                (re-find #"^i " b))
           (str "ai " (string/replace b #"^i " ""))

           (and (= a "a")
                (string? b)
                (re-find #"^le " b))
           (str "alle " (string/replace b #"^le " ""))

           (and (= a "a")
                (string? b)
                (re-find #"^la " b))
           (str "alla " (string/replace b #"^la " ""))
     
           (and (string? a) (string? b))
           (trim (str a " " b))
           
           (and (string? a) (string? (get-in b '(:italiano))))
           (trim (str a " " (get-in b '(:italiano))))
           
           (and (string? (get-in a '(:italiano)))
                (string? b))
           (trim (str (get-in a '(:italiano)) " " b))
           
           (and (string? a)
                (map? b))
           (trim (str a " .."))

           (and (string? b)
                (map? a))
           (trim (str " .." b))

           true
           {:a (if (nil? a) :top a)
            :b (if (nil? b) :top b)}))))

(defn fo [input]
  (cond 

   (= input :fail)
   (str input)

   (string? input)
   input

   (:italiano input)
   ;; get-string should always return a string, but sometimes it (incorrectly) does not (FIXME)
   (string/trim (str (get-string (:italiano input))))
   
   (and (map? input)
        (get-in input [:a])
        (get-in input [:b]))
   (str (string/join " " 
                     (list (fo (get-in input [:a]))
                           (get-in input [:punctuation :middle])
                           (fo (get-in input [:b])))))
                     
   (or (seq? input)
       (vector? input))
   (str "(" (string/join " , " 
                         (remove #(= % "")
                                 (map #(let [f (fo %)] (if (= f "") "" (str "" f ""))) input)))
        ")")

   true
   ""))

(defonce ppa-tokens-to-surface
  (map (fn [pair]
         [(re-pattern
           (str "\\b" (first pair) "\\b"))
          (second pair)])
       preposition-plus-article))

(defonce ppa-surface-to-tokens
  (map (fn [pair]
         [(re-pattern
           (str "\\b" (second pair) "\\b"))
          (first pair)])
       preposition-plus-article))

(defn conjugate-italian-prep [prep np]
  (let [concat (str (get prep :italiano)
                    " "
                    (get np :italiano))]
    (replace-from-list
     preposition-plus-article
     concat)))

(defn analyze-regular [surface-form lexicon]
  "do regular (i.e. non-exceptional) morphological analysis to determine lexical information for a conjugated surface-form, using the (defonce replace-patterns) defined above."
  (mapcat
   (fn [replace-pattern]
     (let [ ;; regular expression that matches the surface form
           from (nth (:p replace-pattern) 0)]
       (log/debug (str "trying replace-pattern:" replace-pattern " against surface-form: " surface-form))
       (if (re-matches from surface-form)
         (do
           (log/debug (str "found matching replace-pattern:" replace-pattern " for surface-form: " surface-form))
           (log/debug (str "using unify-with: " (:u replace-pattern)))
           (let [;; expression that is used by string/replace along with the first regexp and the surface form,
                 ;; to create the lexical string
                 to (nth (:p replace-pattern) 1)
                 ;; unifies with the lexical entry to create the inflected form.
                 unify-with (if (:u replace-pattern)
                              (:u replace-pattern)
                              :top) ;; default unify-with
                 debug (log/debug (str "surface-form: " surface-form "; from: " from "; to:" to))
                 potential-lexical-form
                 (try
                   (string/replace surface-form from to)
                   (catch Exception e
                     (throw (Exception. (str "Can't string/replace on: "
                                             "surface-form: " surface-form "; from: " from "; to:" to)))))]
             (filter (fn [result] (not (= :fail result)))
                     (let [map-fn #?(:clj pmap) #?(:cljs map)]
                       (map-fn (fn [lexical-entry]
                                 (let [result (unify unify-with lexical-entry)]
                                   (log/debug (str "unify result:" result))
                                   result))
                             (get lexicon potential-lexical-form)))))))))
   replace-patterns))

(declare analyze-capitalization-variant)

(defn analyze
  "take the union of: 
      - analyzing _surface-form_ according to the (defonce replace-patterns) above
      - looking up _surface-form_ in the supplied lexicon."
  [surface-form lexicon]
  (mapcat (fn [each-variant]
            (analyze-capitalization-variant each-variant lexicon))
          (set
           (list
            surface-form
            (string/capitalize surface-form)
            (string/capitalize (string/lower-case surface-form))
            (string/upper-case surface-form)
            (string/lower-case surface-form)))))

(defn analyze-capitalization-variant [surface-form lexicon]
  "return an array of the maps, each of which represents the lexical information about a surface form."
  (concat
   (analyze-regular surface-form lexicon)

   ;; make canonical input forms fully inflected:
   (map (fn [lexeme]
          (cond (and (= :verb (get-in lexeme [:synsem :cat]))
                     (= :top (get-in lexeme [:synsem :infl])))
                ;; if a verb has no infl, it's :infinitive.
                (unify lexeme
                        {:synsem {:infl :infinitive}})

                (and (= :noun (get-in lexeme [:synsem :cat]))
                     (= :top (get-in lexeme [:synsem :agr :number])))
                ;; if a noun has no number, it's singular.
                (unify lexeme
                        {:synsem {:agr {:number :sing}}})
                true
                lexeme))
        (get lexicon surface-form))))

(defonce exceptions-rules
  (concat verbs/exceptions-rules
          adjectives/exceptions-rules
          nouns/exceptions-rules))

;; TODO: move this to babel.italiano.lexicon, since it is part of lexicon compilation
(defn exception-generator [lexicon]
  (->>
   (sort (keys lexicon))
   (mapcat (fn [k]
          (let [lexemes (get lexicon k)
                lexeme-kv [k lexemes]]
            (->> exceptions-rules
                 (mapcat (fn [{path :path
                               label :label
                               surface-form :surface-form
                               merge-fn :merge-fn}]
                           (let [surface-form-fn (or surface-form
                                                     (fn [lexeme]
                                                       (get-in lexeme path :none)))]
                             ;; a lexeme-kv is a pair of a key and value. The key is a string (the word's surface form)
                             ;; and the value is a list of lexemes for that string.
                             (->> lexemes
                                  (mapcat (fn [lexeme]
                                            (if (not (= :none (get-in lexeme path :none)))
                                              (do (log/debug (str (first lexeme-kv) " generating lexeme exceptional surface form: " (surface-form-fn lexeme)))
                                                  (list {(surface-form-fn lexeme)
                                                         [(reduce
                                                           (fn [a b]
                                                             (cond
                                                               (= a :fail)
                                                               (do
                                                                 (log/warn (str ":fail in exception rule:"
                                                                                label
                                                                                "; lexeme's italiano:"
                                                                                (strip-refs (get-in lexeme [:italiano]))))
                                                                 :fail)
                                                               (= b :fail)
                                                               (do
                                                                 (log/warn (str ":fail in exception rule:"
                                                                                label
                                                                                "; lexeme:"
                                                                                (or (strip-refs (get-in lexeme [:italiano :italiano]))
                                                                                    (strip-refs (get-in lexeme [:italiano])))))
                                                                 :fail)
                                                               true
                                                               (unify a b)))
                                                           [(dissoc-paths lexeme [[:italiano :italiano]])
                                                            (merge-fn lexeme)
                                                            {:italiano {:infinitive k
                                                                        :exception true}}])]})))))))))))))))
(defn phonize2 [lexicon]
  (into {}
        (for [[k vals] lexicon]
          [k 
           (map (fn [v]
                  (unify v
                          {:italiano {:italiano k}}))
                vals)])))

;; TODO: remove: using phonize2 now instead.
(defn phonize [a-map a-string]
  (let [common {:phrasal false}]
    (cond (or (vector? a-map) (seq? a-map))
          (map (fn [each-entry]
                 (phonize each-entry a-string))
               a-map)

          (and (map? a-map)
               (not (= :no-italiano (get-in a-map [:italiano] :no-italiano))))
          (unify {:italiano {:italiano a-string}}
                  common
                  a-map)

          true
          (unify a-map
                  {:italiano a-string}
                  common))))
