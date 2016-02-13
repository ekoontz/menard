(ns babel.italiano.morphology
  (:refer-clojure :exclude [get-in merge resolve])
  (:require
   [babel.pos :refer (noun)]
   [babel.italiano.morphology.adjectives
    :refer (plural-to-singular-adj-masc
            plural-to-singular-adj-fem-sing
            plural-to-singular-adj-fem-plur)]
   [babel.italiano.morphology.articles :as articles]
   [babel.italiano.morphology.nouns
    :refer (plural-to-singular-noun-fem-1
            plural-to-singular-noun-masc-1
            plural-to-singular-noun-masc-2)]
   [babel.italiano.morphology.verbs :as verbs]
   [babel.italiano.pos :refer (pronoun-acc verb-aux)]
   [babel.stringutils :refer (replace-from-list)]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer (copy dissoc-paths fail? get-in merge ref? unifyc)]))

(defn phrase-is-finished? [phrase]
  (cond
   (string? phrase) true
   (map? phrase)
   (or (phrase-is-finished? (get-in phrase '(:italiano)))
       (and (phrase-is-finished? (get-in phrase '(:a)))
            (phrase-is-finished? (get-in phrase '(:b)))))
   :else false))

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

(declare get-string)

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

;; TODO: this is an overly huge method that needs to be rewritten to be easier to understand and maintain.
(defn get-string-1 [word]
  (if (seq? word)
    (map (string/join " " #(get-string-1 %))
         word)
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))
        info (log/debug "get-string-1: input word: " word)]

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
     ;; exceptions are dealt with at compile-time now, via italianverbs.lexicon.italiano/exception-generator

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
                     #"[a]$" "e") ;; donna => donne

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

     ;; future 1) irregular
     (and
      (= (get-in word '(:infl)) :future)
      (map? (get-in word '(:future))))
     (let [infinitive (get-in word '(:italiano))
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
        word)))

     ;; future: 2) futueo-stem specified
     (and (= (get-in word '(:infl)) :future)
          (get-in word '(:future-stem)))
     (let [stem (get-in word '(:future-stem))]
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
        (str stem "anno")))

     ;; future 3) regular inflection of future.
     (and (= (get-in word '(:infl)) :future)
          (get-in word '(:italiano)))

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
        (str stem "anno")

        :else
        (get-in word '(:italiano))))

     ;; regular inflection of conditional
     (and (= (get-in word '(:infl)) :conditional)
          (string? (get-in word '(:future-stem) :none)))
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
        (str stem "ebbero")))

     ;; regular inflection of conditional
     (and (= (get-in word '(:infl)) :conditional)
          (get-in word '(:italiano)))

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
        (get-in word '(:italiano))))

     ;; irregular imperfect sense:
     ;; 1) use irregular based on number and person.
     (and
      (= (get-in word '(:infl)) :imperfect)
      (= :sing (get-in word '(:agr :number)))
      (= :1st (get-in word '(:agr :person)))
      (string? (get-in word '(:imperfect :1sing))))
     (get-in word '(:imperfect :1sing))

     (and
      (= (get-in word '(:infl)) :imperfect)
      (= :sing (get-in word '(:agr :number)))
      (= :2nd (get-in word '(:agr :person)))
      (string? (get-in word '(:imperfect :2sing))))
     (get-in word '(:imperfect :2sing))

     (and
      (= (get-in word '(:infl)) :imperfect)
      (= :sing (get-in word '(:agr :number)))
      (= :3rd (get-in word '(:agr :person)))
      (string? (get-in word '(:imperfect :3sing))))
     (get-in word '(:imperfect :3sing))

     (and
      (= (get-in word '(:infl)) :imperfect)
      (= :plur (get-in word '(:agr :number)))
      (= :1st (get-in word '(:agr :person)))
      (string? (get-in word '(:imperfect :1plur))))
     (get-in word '(:imperfect :1plur))
     (and
      (= (get-in word '(:infl)) :imperfect)
      (= :plur (get-in word '(:agr :number)))
      (= :2nd (get-in word '(:agr :person)))
      (string? (get-in word '(:imperfect :2plur))))
     (get-in word '(:imperfect :2plur))
     (and
      (= (get-in word '(:infl)) :imperfect)
      (= :plur (get-in word '(:agr :number)))
      (= :3rd (get-in word '(:agr :person)))
      (string? (get-in word '(:imperfect :3plur))))
     (get-in word '(:imperfect :3plur))

     ;; regular imperfect sense
     (and (= (get-in word '(:infl)) :imperfect)
          (get-in word '(:italiano)))
     (let [infinitive (if (get-in word [:infinitive])
                        (get-in word [:infinitive])
                        (get-in word [:italiano]))
           ;; e.g.: lavarsi => lavare
           infinitive (if (re-find #"[aei]rsi$" infinitive)
                        (string/replace infinitive #"si$" "e")
                        infinitive)
           person (get-in word '(:agr :person))
           number (get-in word '(:agr :number))
           stem (stem-for-imperfect infinitive)]
       (cond
        (and (= person :1st) (= number :sing))
        (str stem "vo")

        (and (= person :2nd) (= number :sing))
        (str stem "vi")

        (and (= person :3rd) (= number :sing))
        (str stem "va")

        (and (= person :1st) (= number :plur))
        (str stem "vamo")

        (and (= person :2nd) (= number :plur))
        (str stem "vate")

        (and (= person :3rd) (= number :plur))
        (str stem "vano")

        (string? infinitive)
        (str infinitive )

        :else
        (merge word
               {:error 1})))

     (and
      (get-in word '(:a))
      (get-in word '(:b))
      true) (str
             (trim (get-string-1 (get-in word '(:a)))) " "
             (trim (get-string-1 (get-in word '(:b)))))

     ;; "fare [past]" + "bene" => "fatto bene"
     (and (= (get-in word '(:cat)) :verb)
          (= (get-in word '(:infl)) :past)
          (string? (get-in word '(:a :passato))))
     (str (get-in word '(:a :passato)) " "
          (get-string-1 (get-in word '(:b))))

     ;; TODO: do not use brackets: if there's an error about there being
     ;; not enough information, throw an exception explicitly.
     ;; return the irregular form in square brackets, indicating that there's
     ;; not enough information to conjugate the verb.
     (and (= :past (get-in word '(:infl)))
          (get-in word '(:passato))
          (get-in word '(:essere) true)
          (or (= :notfound (get-in word '(:agr :number) :notfound))
              (= :top (get-in word '(:agr :number)))))
     ;; not enough information.
     (do
       (log/warn (str "not enough agreement specified to conjugate: " (get-in word '(:passato)) " (irreg past)]"))
       (get-in word '(:passato)))

     ;; TODO: do not use brackets: if there's an error about there being
     ;; regular passato prossimo and essere-verb => NEI (not enough information): defer conjugation and keep as a map.
     (and (= :past (get-in word '(:infl)))
          (= (get-in word '(:essere)) true)
          (or (= :notfound (get-in word '(:agr :number) :notfound))
              (= :top (get-in word '(:agr :number)))))
     ;; 'nei': not enough information.
     (do
       (log/warn (str "not enough agreement specified to conjugate: " (get-in word '(:passato)) " (past)]"))
       (str (get-in word [:italiano]) " (past)"))

     ;; conjugate irregular passato: option 1) using :passato-stem
     (and (= :past (get-in word '(:infl)))
          (get-in word '(:passato-stem)))
     (let [irregular-passato (get-in word '(:passato-stem))]
       (str irregular-passato (suffix-of word)))

     ;; conjugate irregular passato: option 2) using :passato
     (and (= :past (get-in word '(:infl)))
          (get-in word '(:passato)))
     (let [irregular-passato (get-in word '(:passato))
           butlast (nth (re-find #"(.*).$" irregular-passato) 1)]
       (str butlast (suffix-of word)))

     ;; conjugate regular passato
     (and (= :past (get-in word '(:infl)))
          (string? (get-in word '(:italiano))))
     (let [infinitive (get-in word [:italiano])
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
           last-stem-char-is-i (re-find #"i$" stem)

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
        (str "(regpast:TODO):" stem)))

     ;; <irregular present tense>
     (and (= (get-in word '(:infl)) :present)
          (= person :1st) (= number :sing)
          (string? (get-in word '(:present :1sing))))
     (get-in word '(:present :1sing))

     (and (= (get-in word '(:infl)) :present)
          (= person :2nd) (= number :sing)
          (string? (get-in word '(:present :2sing))))
     (get-in word '(:present :2sing))

     (and (= (get-in word '(:infl)) :present)
          (= person :3rd) (= number :sing)
          (string? (get-in word '(:present :3sing))))
     (get-in word '(:present :3sing))

     (and (= (get-in word '(:infl)) :present)
          (= person :1st) (= number :plur)
          (string? (get-in word '(:present :1plur))))
     (get-in word '(:present :1plur))

     (and (= (get-in word '(:infl)) :present)
          (= person :2nd) (= number :plur)
          (string? (get-in word '(:present :2plur))))
     (get-in word '(:present :2plur))

     (and (= (get-in word '(:infl)) :present)
          (= person :3rd) (= number :plur)
          (string? (get-in word '(:present :3plur))))
     (get-in word '(:present :3plur))
     ;; </irregular present tense>

     (and
      (= (get-in word '(:infl)) :present)
      (string? (get-in word '(:italiano))))
     (let [infinitive (if (get-in word [:infinitive])
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
             ire-type)
        (str stem "ono")

        (and (= person :3rd) (= number :plur)
             ere-type)
        (str stem "ono")

        (and (= person :3rd) (= number :plur))
        (str stem "ano")

        :else
        (str infinitive )))

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

           (and (= a "i")
                (string? (get-in b '(:italiano)))
                (re-find #"^[aeiou]" (get-in b '(:italiano))))
           (trim (str "gli " b))

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

(declare fo-ps-it)

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
                           (fo (get-in input [:b])))))
                     
   (or (seq? input)
       (vector? input))
   (str "(" (string/join " , " 
                         (remove #(= % "")
                                 (map #(let [f (fo %)] (if (= f "") "" (str "" f ""))) input)))
        ")")

   true
   ""))

(defn fo-ps [expr]
  "show the phrase-structure of a phrase structure tree, e.g [hh21 'mangiare (to eat)' [cc10 'il (the)' 'pane(bread)']]"
  ;; [:first = {:head,:comp}] will not yet be found in expr, so this head-first? will always be false.
  (let [head-first? (= :head (get-in expr [:first]))]
    (cond

     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (empty? expr))
     (str "")


     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (not (empty? expr)))

     ;; expr is a sequence of some kind. Assume each element is a phrase structure tree and show each.
     (map (fn [each]
            (fo-ps each))
          expr)

     (and (map? expr)
          (:italiano expr))
     (fo-ps-it (:italiano expr))

     (and (map? expr)
          (:rule expr)
          (= (get-in expr '(:italiano :a))
             (get-in expr '(:comp :italiano))))
     ;; complement first
     (str "[" (:rule expr) " "
          (fo-ps (get-in expr '(:comp)))
          " "
          (fo-ps (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:rule expr))
     ;; head first ('else' case of above.)
     (str "[" (:rule expr) " "
          (fo-ps (get-in expr '(:head)))
          " "
          (fo-ps (get-in expr '(:comp)))
          "]")

     (and (map? expr)
          (:comment expr)
          (= (get-in expr '(:italiano :a))
             (get-in expr '(:comp :italiano))))
     ;; complement first
     (str "[" (:comment expr) " "
          (fo-ps (get-in expr '(:comp)))
          " "
          (fo-ps (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:comment expr))
     ;; head first ('else' case of above.)
     (str "[" (:comment expr) " "
          (fo-ps (get-in expr '(:head)))
          " "
          (fo-ps (get-in expr '(:comp)))
          "]")

     (and
      (map? expr)
      (:italiano expr))
     (get-string-1 (get-in expr '(:italiano)))

     true
     expr)))

(defn conjugate-italian-prep [prep np]
  (let [concat (str (get prep :italiano)
                    " "
                    (get np :italiano))]
    (replace-from-list
     (list
      (list #"\ba il " "al ")
      (list #"\ba lo " "allo ")
      (list #"\ba la " "alla ")
      (list #"\ba l'" "all'")
      (list #"\ba i " "ai ")
      (list #"\ba gli " "agli ")
      (list #"\ba le " "alle ")

      (list #"\bda il " "dal ")
      (list #"\bda lo " "dallo ")
      (list #"\bda la " "dalla ")
      (list #"\bda l'" "dall'")
      (list #"\bda i " "dai ")
      (list #"\bda gli " "dagli ")
      (list #"\bda le " "dalle ")

      (list #"\bde il " "del ")
      (list #"\bde lo " "dello ")
      (list #"\bde la " "della ")
      (list #"\bde l'" "dell'")
      (list #"\bde i " "dei ")
      (list #"\bde gli " "degli ")
      (list #"\bde le " "delle ")

      (list #"\bdi il " "del ")
      (list #"\bdi lo " "dello ")
      (list #"\bdi la " "della ")
      (list #"\bdi l'" "dell'")
      (list #"\bdi i " "dei ")
      (list #"\bdi gli " "degli ")
      (list #"\bdi le " "delle ")

      (list #"\bin il " "nel ")
      (list #"\bin lo " "nello ")
      (list #"\bin la " "nella ")
      (list #"\bin l'" "nell'")
      (list #"\bin i " "nei ")
      (list #"\bin gli " "negli ")
      (list #"\bin le " "nelle ")

      (list #"\bsu il " "sul ")
      (list #"\bsu lo " "sullo ")
      (list #"\bsu la " "sulla ")
      (list #"\bsu l'" "sull'")
      (list #"\bsu i " "sui ")
      (list #"\bsu gli " "sugli ")
      (list #"\bsu le " "sulle ")
      )
     concat)))

(defn stem-for-passato-prossimo [infinitive]
  "_infinitive_ should be a string (italian verb infinitive form)"
  (string/replace infinitive #"^(.*)([aei])(re)$" (fn [[_ prefix vowel suffix]] (str prefix))))

(defn passato-prossimo [infinitive]
  (str (stem-for-passato-prossimo infinitive) "ato"))

;; allows reconstruction of the infinitive form from the inflected form
(def future-to-infinitive-irreg1
  {
   ;; future
   #"cherò$" 
   {:replace-with "care"
    :unify-with {:italiano {:infl :future
                            :agr {:number :sing
                                  :person :1st}}}}
   #"cherai$" 
   {:replace-with "care"
    :unify-with {:italiano {:infl :future
                            :agr {:number :sing
                                  :person :2nd}}}}
   #"cherà$" 
   {:replace-with "care"
    :unify-with {:italiano {:infl :future
                            :agr {:number :sing
                                  :person :3rd}}}}
   #"cheremo$" 
   {:replace-with "care"
    :unify-with {:italiano {:infl :future
                            :agr {:number :plur
                                  :person :1st}}}}
   #"cherete$" 
   {:replace-with "care"
    :unify-with {:italiano {:infl :future
                            :agr {:number :plur
                                  :person :2nd}}}}
   #"cheranno$" 
   {:replace-with "care"
    :unify-with {:italiano {:infl :future
                            :agr {:number :plur
                                  :person :3rd}}}}})

;; allows reconstruction of the infinitive form from the inflected form
(def future-to-infinitive
  {
   ;; future
   #"ò$" 
   {:replace-with "e"
    :unify-with {:italiano {:infl :future
                            :agr {:number :sing
                                  :person :1st}}}}
   #"ai$" 
   {:replace-with "e"
    :unify-with {:italiano {:infl :future
                            :agr {:number :sing
                                  :person :2nd}}}}
   #"à$" 
   {:replace-with "e"
    :unify-with {:italiano {:infl :future
                            :agr {:number :sing
                                  :person :3rd}}}}
   #"emo$" 
   {:replace-with "e"
    :unify-with {:italiano {:infl :future
                            :agr {:number :plur
                                  :person :1st}}}}
   #"ete$" 
   {:replace-with "e"
    :unify-with {:italiano {:infl :future
                            :agr {:number :plur
                                  :person :2nd}}}}
   #"anno$" 
   {:replace-with "e"
    :unify-with {:italiano {:infl :future
                            :agr {:number :plur
                                  :person :3rd}}}}})

(def present-to-infinitive-ire
  {
   ;; present -ire
   #"o$"
   {:replace-with "ire"
    :unify-with {:italiano {:infl :present
                            :agr {:number :sing
                                  :person :1st}}}}
   
   #"i$"
   {:replace-with "ire"
    :unify-with {:italiano {:infl :present
                            :agr {:number :sing
                                  :person :2nd}}}}

   #"e$"
   {:replace-with "ire"
    :unify-with {:italiano {:infl :present
                            :agr {:number :sing
                                  :person :3rd}}}}

   #"iamo$"
   {:replace-with "ire"
    :unify-with {:italiano {:infl :present
                            :agr {:number :plur
                                  :person :1st}}}}

   #"ete$"
   {:replace-with "ire"
    :unify-with {:italiano {:infl :present
                            :agr {:number :plur
                                  :person :2nd}}}}
   
   #"ono$"
   {:replace-with "ire"
    :unify-with {:italiano {:infl :present
                            :agr {:number :plur
                                  :person :3rd}}}}})


(def present-to-infinitive-ere
  {;; present -ere
   #"o$"
   {:replace-with "ere"
    :unify-with {:italiano {:infl :present
                            :agr {:number :sing
                                  :person :1st}}}}

   #"i$"
   {:replace-with "ere"
    :unify-with {:italiano {:infl :present
                            :agr {:number :sing
                                  :person :2nd}}}}
   
   #"e$"
   {:replace-with "ere"
    :unify-with {:italiano {:infl :present
                            :agr {:number :sing
                                  :person :3rd}}}}
   
   #"iamo$"
   {:replace-with "ere"
    :unify-with {:italiano {:infl :present
                            :agr {:number :plur
                                  :person :1st}}}}

   #"ete$"
   {:replace-with "ere"
    :unify-with {:italiano {:infl :present
                            :agr {:number :plur
                                  :person :2nd}}}}
   
   #"ano$"
   {:replace-with "ere"
    :unify-with {:italiano {:infl :present
                            :agr {:number :plur
                                  :person :3rd}}}}})

(def present-to-infinitive-are
  {
   ;; present -are
   #"o$"
   {:replace-with "are"
    :unify-with {:italiano {:infl :present
                            :agr {:number :sing
                                  :person :1st}}}}

   #"ci$" ;; e.g. "abbracci" => "abbracciare"
   {:replace-with "ciare"
    :unify-with {:italiano {:infl :present
                            :agr {:number :sing
                                  :person :2nd}}}}
   #"[^c]i$"
   {:replace-with "are"
    :unify-with {:italiano {:infl :present
                            :agr {:number :sing
                                  :person :2nd}}}}

   #"e$"
   {:replace-with "are"
    :unify-with {:italiano {:infl :present
                            :agr {:number :sing
                                  :person :3rd}}}}
   
   #"iamo$"
   {:replace-with "are"
    :unify-with {:italiano {:infl :present
                            :agr {:number :plur
                                  :person :1st}}}}

   #"ete$"
   {:replace-with "are"
    :unify-with {:italiano {:infl :present
                            :agr {:number :plur
                                  :person :2nd}}}}

   #"ano$"
   {:replace-with "are"
    :unify-with {:italiano {:infl :present
                            :agr {:number :plur
                                  :person :3rd}}}}})

(def imperfect-to-infinitive-irreg1
  {
   ;; e.g.: "bevevo/bevevi/..etc" => "bere"
   #"vevo$"
   {:replace-with "re"
    :unify-with {:italiano {:infl :imperfect
                            :agr {:number :sing
                                  :person :1st}}}}

   #"vevi$"
   {:replace-with "re"
    :unify-with {:italiano {:infl :imperfect
                            :agr {:number :sing
                                  :person :2nd}}}}

   #"veva$"
   {:replace-with "re"
    :unify-with {:italiano {:infl :imperfect
                            :agr {:number :sing
                                  :person :3rd}}}}
   })

(def past-to-infinitive
  {#"ato$"
   {:replace-with "are"
    :unify-with {:italiano {:infl :past}}}

   #"ito$"
   {:replace-with "ire"
    :unify-with {:italiano {:infl :past}}}

   #"uto$"
   {:replace-with "ere"
    :unify-with {:italiano {:infl :past}}}})

(def identity-analyzers
  {:infinitive-verbs
   {:unify-with {:synsem {:cat :verb
                          :infl :infinitive}}}
   :singular-nouns
   {:unify-with {:synsem {:cat :noun
                          :agr {:number :sing}}}}
   :determiners
   {:unify-with {:synsem {:cat :det}}}})

(def replace-patterns
  (concat
   verbs/replace-patterns))

(defn new-style [surface-form lexicon]
  (mapcat
   (fn [replace-pattern]
     (let [ ;; regular expression that matches the surface form
           from (nth (:p replace-pattern) 0)]
       (log/debug (str "matching replace-pattern:" replace-pattern " against surface-form: " surface-form))
       (if (re-matches from surface-form)
         (let [;; expression that is used by string/replace along with the first regexp and the surface form,
               ;; to create the lexical string
               to (nth (:p replace-pattern) 1)
               ;; unifies with the lexical entry to create the inflected form.
               unify-with (if (:u replace-pattern)
                            (:u replace-pattern)
                            :top) ;; default unify-with
               lex (string/replace surface-form from to)]
           (filter (fn [result] (not (= :fail result)))
                   (map (fn [lexical-entry]
                          (unifyc unify-with lexical-entry))
                        (get lexicon lex)))))))
   replace-patterns))

(defn analyze [surface-form lexicon]
  "return the map incorporating the lexical information about a surface form."
  (let [
        ;; TODO: remove: backwards compatibility
        lookup-fn (fn [surface-form]
                    (get lexicon surface-form))
        new-style (new-style surface-form lexicon)
        replace-pairs
        ;; Even though it's possible for more than one KV pair to have the same key:
        ;; e.g. plural-to-singular-noun-masc-1 and plural-to-singular-noun-masc-2 both have
        ;; #"i$", they are distinct as separate keys in this 'replace-pairs' hash, as they should be.
        (merge 

         ;; verb morphology
         future-to-infinitive-irreg1
         future-to-infinitive
         imperfect-to-infinitive-irreg1
         past-to-infinitive
         present-to-infinitive-ire
         present-to-infinitive-ere
         present-to-infinitive-are

         ;; noun morphology
         plural-to-singular-noun-fem-1
         plural-to-singular-noun-masc-1
         plural-to-singular-noun-masc-2

         ;; adjective morphology
         plural-to-singular-adj-masc
         plural-to-singular-adj-fem-plur
         plural-to-singular-adj-fem-sing

         articles/l-apostrophe
         )
        
        analyzed
        (remove fail?
                (mapcat
                 (fn [key]
                   (if (re-find key surface-form)
                        (let [replace-with (get replace-pairs key)
                              lexical-form (if (= key :identity)
                                             surface-form
                                             (string/replace surface-form key
                                                             (:replace-with replace-with)))
                              looked-up (lookup-fn lexical-form)]
                          (map #(unifyc 
                                 %
                                 (:unify-with replace-with))
                               looked-up))))
                 (keys replace-pairs)))

        ;; Analyzed-via-identity is used to: 
        ;; 1. handle infinitive verbs: converts them from unspecified inflection to
        ;; {:infl :infinitive}
        ;; 2. convert nouns from unspecified number to singular number: {:synsem {:agr {:num :sing}}}
        analyzed-via-identity
        (remove fail?
                (mapcat
                 (fn [key]
                   (let [lexical-form surface-form
                         looked-up (lookup-fn lexical-form)]
                     (map #(unifyc 
                            %
                            (:unify-with (get replace-pairs key))
                            {:via key
                             :identity-with (get replace-pairs key)})
                          looked-up)))
                 (keys identity-analyzers)))]

    (concat

     new-style

     analyzed

     ;; also lookup the surface form itself, which
     ;; might be either the canonical form of a word, or an irregular conjugation of a word.
     (if (not (empty? analyzed-via-identity))
       analyzed-via-identity
     (lookup-fn surface-form)))))

(defn exception-generator [lexicon]
  (let [lexeme-kv (first lexicon)
        lexemes (second lexeme-kv)]
    (if lexeme-kv
      (let [result (mapcat (fn [path-and-merge-fn]
                             (let [path (:path path-and-merge-fn)
                                   merge-fn (:merge-fn path-and-merge-fn)]
                               ;; a lexeme-kv is a pair of a key and value. The key is a string (the word's surface form)
                               ;; and the value is a list of lexemes for that string.
                               (log/debug (str (first lexeme-kv) " looking at path: " path))
                               (mapcat (fn [lexeme]
                                         ;; this is where a unify/dissoc that supported
                                         ;; non-maps like :top and :fail, would be useful:
                                         ;; would not need the (if (not (fail? lexeme)..)) check
                                         ;; to avoid a difficult-to-understand "java.lang.ClassCastException: clojure.lang.Keyword cannot be cast to clojure.lang.IPersistentMap" error.
                                         (let [lexeme (cond (= lexeme :fail)
                                                            :fail
                                                            (= lexeme :top)
                                                            :top
                                                            true
                                                            (dissoc (copy lexeme) :serialized))]
                                           (if (not (= :none (get-in lexeme path :none)))
                                             (list {(get-in lexeme path :none)
                                                    (merge
                                                     lexeme
                                                     (unifyc (merge-fn lexeme)
                                                             {:italiano {:exception true}}))}))))
                                       lexemes)))
                           [
                            ;; 1. past-tense exceptions
                            {:path [:italiano :passato]
                             :merge-fn
                             (fn [val]
                               {:italiano {:infl :past
                                           :italiano (get-in val [:italiano :passato] :nothing)}})}

                            ;; 2. present-tense exceptions
                            {:path [:italiano :present :1sing]
                             :merge-fn
                             (fn [val]
                               {:italiano {:infl :present
                                           :italiano (get-in val [:italiano :present :1sing] :nothing)
                                           :agr {:number :sing
                                                 :person :1st}}})}
                            {:path [:italiano :present :2sing]
                             :merge-fn
                             (fn [val]
                               {:italiano {:infl :present
                                           :italiano (get-in val [:italiano :present :2sing] :nothing)
                                           :agr {:number :sing
                                                 :person :2nd}}})}

                            {:path [:italiano :present :3sing]
                             :merge-fn
                             (fn [val]
                               {:italiano {:infl :present
                                           :italiano (get-in val [:italiano :present :3sing] :nothing)
                                           :agr {:number :sing
                                                 :person :3rd}}})}

                            {:path [:italiano :present :1plur]
                             :merge-fn
                             (fn [val]
                               {:italiano {:infl :present
                                           :italiano (get-in val [:italiano :present :1plur] :nothing)
                                           :agr {:number :plur
                                                 :person :1st}}})}

                            {:path [:italiano :present :2plur]
                             :merge-fn
                             (fn [val]
                               {:italiano {:infl :present
                                           :italiano (get-in val [:italiano :present :2plur] :nothing)
                                           :agr {:number :plur
                                                 :person :2nd}}})}

                            {:path [:italiano :present :3plur]
                             :merge-fn
                             (fn [val]
                               {:italiano {:infl :present
                                           :italiano (get-in val [:italiano :present :3plur] :nothing)
                                           :agr {:number :plur
                                                 :person :3rd}}})}

                            ;; adjectives
                            {:path [:italiano :masc :plur]
                             :merge-fn
                             (fn [val]
                               {:italiano {:agr {:gender :masc
                                                 :number :plur}}})}

                            {:path [:italiano :fem :plur]
                             :merge-fn
                             (fn [val]
                               {:italiano {:agr {:gender :fem
                                                 :number :plur}}})}
                            {:path [:italiano :fem :sing]
                             :merge-fn
                             (fn [val]
                               {:italiano {:agr {:gender :fem
                                                 :number :sing}}})}
                            ;; nouns
                            {:path [:italiano :plur]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :noun}
                                :italiano {:agr {:number :plur}}})}
                            
                            ])]
        (concat result (exception-generator (rest lexicon)))))))

(defn phonize [a-map a-string]
  (let [common {:phrasal false}]
    (cond (or (vector? a-map) (seq? a-map))
          (map (fn [each-entry]
                 (phonize each-entry a-string))
               a-map)

          (and (map? a-map)
               (not (= :no-italiano (get-in a-map [:italiano] :no-italiano))))
          (unifyc {:italiano {:italiano a-string}}
                  common
                  a-map)

          true
          (unifyc a-map
                  {:italiano a-string}
                  common))))

(def pronoun-semantic-gender-agreement
  (let [gender (atom :top)]
    {:synsem {:sem {:gender gender}
              :agr {:gender gender}}}))

(defn agreement [lexical-entry]
  (cond
   (= (get-in lexical-entry [:synsem :cat]) :verb)
   (let [cat (atom :top)
         infl (atom :top)]
     (unifyc lexical-entry
             {:italiano {:cat cat
                         :infl infl}
              :synsem {:cat cat
                       :infl infl}}))

   (and (= (get-in lexical-entry [:synsem :cat]) :noun)
        (= (get-in lexical-entry [:synsem :pronoun]) true))
   (unifyc lexical-entry
           pronoun-semantic-gender-agreement)

   (and (= (get-in lexical-entry [:synsem :cat]) :noun)
        (= (get-in lexical-entry [:synsem :subcat :1]) :top))
   ;; nouns which select articles that match in gender and number.
   (unifyc lexical-entry
           (let [number (atom :top)
                 gender (atom :top)
                 person (atom :top)
                 agr (atom {:number number
                           :gender gender
                           :person person})
                 cat (atom :top)]
             {:synsem {:cat cat
                       :case :top
                       :subcat {:1 {:number number
                                    :person person
                                    :gender gender}}
                       :agr agr}}))

   true
   lexical-entry))

(defn essere-default [lexical-entry]
  "if :essere is not set, then it's false."
  (cond
   (= (get-in lexical-entry [:synsem :essere] :top) :top)
   (unifyc lexical-entry
           {:synsem {:essere false}})
   true lexical-entry))

(defn aux-verb-rule [lexical-entry]
  "If a word's :synsem :aux is set to true, then auxify it (add all the
  things that are consequent on its being an aux verb.
   If, however, it is a verb and its :synsem :aux is not set,
  then set its aux explicitly to false."
  (cond (= (get-in lexical-entry '(:synsem :aux)) true)
        (unifyc lexical-entry
                verb-aux)
        (and (= (get-in lexical-entry '(:synsem :cat)) :verb)
             (= :none (get-in lexical-entry '(:synsem :aux) :none)))
        (unifyc lexical-entry
                {:synsem {:aux false}})
        true
        lexical-entry))

(def italian-specific-rules
  (list agreement 
        essere-default
        aux-verb-rule))

