(ns babel.espanol.morphology
  (:refer-clojure :exclude [get-in merge resolve])
  (:require [babel.espanol.morphology.nouns :as nouns]
            [babel.espanol.morphology.verbs :as verbs]
            [babel.stringutils :refer [show-as-tree]]
            [clojure.string :as string]
            [clojure.string :refer (trim)]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log])
            [dag_unify.core :refer (copy dissoc-paths fail? get-in merge ref? strip-refs unifyc)]))

(declare get-string)

(defn fo [input]
  (try
    (cond 

      (= input :fail)
      (str input)

      (= (type input) clojure.lang.LazySeq)
      (str "['" (string/join "','" (map fo input)) "']")

      (string? input)
      input

      (get-in input [:espanol])
      (string/trim (str (get-string (get-in input [:espanol]))))
   
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
      "")
    (catch Exception e
      (do
        (log/trace (str "ignoring exception: '" e "' and returning canonical form instead."))
        (show-as-tree (get-in input [:espanol]) :espanol)))))

;; TODO: this is an overly huge method that needs to be rewritten to be easier to understand and maintain.
(defn get-string-1 [word & [ {usted :usted
                              tú :tu
                              vosotros :vosotros
                              ustedes :ustedes
                              }]]
  (cond
   (string? word)
   word
   (seq? word)
   (map (string/join " " #(get-string-1 %))
        word)
   (= word :top) ".."
   (ref? word)
   (get-string-1 @word)
   (map? word)
   (let [person (get-in word '(:agr :person))
         number (get-in word '(:agr :number))
         info (log/debug "get-string-1: input word: " word)
         vosotros (if vosotros vosotros true)
         ustedes (if ustedes ustedes false)
         tú (if tú tú false)
         usted (if usted usted false)]
     (log/debug (str "get-string-1: word: " word))
     (log/debug (str "get-string-1: word (stripped-refs): " (strip-refs word)))
     (log/debug (str "word's a is a string? " (get-in word '(:a)) " => " (string? (get-in word '(:a)))))
     (log/debug (str "word's b is a map? " (get-in word '(:b)) " => " (map? (get-in word '(:b)))))
     
     (log/debug (str "word's a espanol is a string? " (get-in word '(:a :espanol)) " => " (string? (get-in word '(:a :espanol)))))

     (cond
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
       (string? (get-in word '(:a :espanol)))
       (string? (get-in word '(:b :espanol)))
       (or (= :none (get-in word '(:b :agr :number) :none))
           (= :top (get-in word '(:b :agr :number) :none)))
       )
      (str (string/trim (get-in word '(:a :espanol)))
           " "
           (string/trim (get-in word '(:b :espanol))))

      (and
       (string? (get-in word '(:a)))
       (string? (get-in word '(:b :espanol)))
       (or (= :none (get-in word '(:b :agr :number) :none))
           (= :top (get-in word '(:b :agr :number) :none)))
       )
      (str (string/trim (get-in word '(:a)))
           " "
           (string/trim (get-in word '(:b :espanol))))
      
      (and
       (string? (get-in word '(:a :espanol)))
       (get-in word '(:a :espanol))
       (or (= :none (get-in word '(:b :agr :number) :none))
           (= :top (get-in word '(:b :agr :number) :none)))
       (= (get-in word '(:a :infl)) :top))
      (string/trim (str (get-in word '(:a :espanol))
                        " " (get-string-1 (get-in word '(:b)))))

      (= true (get-in word [:exception]))
      (get-in word [:espanol])

      (and
       (= (get-in word '[:infl]) :conditional)
       (string? (get-in word [:espanol])))
      (verbs/conditional word
                         {:usted usted
                          :tú tú
                          :vosotros vosotros
                          :ustedes ustedes
                          })              
      (and
       (= (get-in word '(:infl)) :future)
       (string? (get-in word '(:espanol))))
      (verbs/future word
                    {:usted usted
                     :tú tú
                     :vosotros vosotros
                     :ustedes ustedes
                     })
      (and
       (= (get-in word '(:infl)) :imperfect)
       (string? (get-in word '(:espanol))))
      (verbs/imperfect word
                       {:usted usted
                        :tú tú
                        :vosotros vosotros
                        :ustedes ustedes
                        })
      (and
       (= (get-in word '(:infl)) :present)
       (string? (get-in word '(:espanol))))
      (verbs/present word
                     {:usted usted
                      :tú tú
                      :vosotros vosotros
                      :ustedes ustedes
                      })

      (and
       (= (get-in word '(:infl)) :preterito)
       (string? (get-in word '(:espanol))))
      (verbs/preterito word
                       {:usted usted
                        :tú tú
                        :vosotros vosotros
                        :ustedes ustedes
                        })
                      
      (string? (get-in word [:espanol]))
      (get-in word [:espanol])

      true
      (throw (Exception. (str "get-string-1: don't know what to do with input argument: " word)))))))

(defn get-string [a & [ b ]]
  (cond (and (nil? b)
             (seq? a))
        (let [result (get-string-1 a)]
          (if (string? result)
            (trim result)
            result))

        true
        (trim (string/join " "
                           (list (get-string-1 a)
                                 (if b (get-string-1 b)
                                     ""))))))

(declare fo-ps-it)

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
          (:espanol expr))
     (fo-ps-it (:espanol expr))

     (and (map? expr)
          (:rule expr)
          (= (get-in expr '(:espanol :a))
             (get-in expr '(:comp :espanol))))
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
          (= (get-in expr '(:espanol :a))
             (get-in expr '(:comp :espanol))))
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
      (:espanol expr))
     (get-string-1 (get-in expr '(:espanol)))

     true
     expr)))

(defn stem-per-passato-prossimo [infinitive]
  "_infinitive_ should be a string (italian verb infinitive form)"
  (string/replace infinitive #"^(.*)([aei])(re)$" (fn [[_ prefix vowel suffix]] (str prefix))))

(defn passato-prossimo [infinitive]
  (str (stem-per-passato-prossimo infinitive) "ato"))

;; allows reconstruction of the infinitive form from the inflected form
(def future-to-infinitive
  {
   ;; future
   #"ò$" 
   {:replace-with "e"
    :unify-with {:espanol {:infl :future
                            :agr {:number :sing
                                  :person :1st}}}}
   #"ai$" 
   {:replace-with "e"
    :unify-with {:espanol {:infl :future
                            :agr {:number :sing
                                  :person :2nd}}}}
   #"à$" 
   {:replace-with "e"
    :unify-with {:espanol {:infl :future
                            :agr {:number :sing
                                  :person :3rd}}}}
   #"emo$" 
   {:replace-with "e"
    :unify-with {:espanol {:infl :future
                            :agr {:number :plur
                                  :person :1st}}}}
   #"ete$" 
   {:replace-with "e"
    :unify-with {:espanol {:infl :future
                            :agr {:number :plur
                                  :person :2nd}}}}
   #"anno$" 
   {:replace-with "e"
    :unify-with {:espanol {:infl :future
                            :agr {:number :plur
                                  :person :3rd}}}}})

(def present-to-infinitive-ire
  {
   ;; present -ire
   #"o$"
   {:replace-with "ire"
    :unify-with {:espanol {:infl :present
                            :agr {:number :sing
                                  :person :1st}}}}
   
   #"i$"
   {:replace-with "ire"
    :unify-with {:espanol {:infl :present
                            :agr {:number :sing
                                  :person :2nd}}}}

   #"e$"
   {:replace-with "ire"
    :unify-with {:espanol {:infl :present
                            :agr {:number :sing
                                  :person :3rd}}}}

   #"iamo$"
   {:replace-with "ire"
    :unify-with {:espanol {:infl :present
                            :agr {:number :plur
                                  :person :1st}}}}

   #"ete$"
   {:replace-with "ire"
    :unify-with {:espanol {:infl :present
                            :agr {:number :plur
                                  :person :2nd}}}}
   
   #"ono$"
   {:replace-with "ire"
    :unify-with {:espanol {:infl :present
                            :agr {:number :plur
                                  :person :3rd}}}}})


(def present-to-infinitive-ere
  {;; present -ere
   #"o$"
   {:replace-with "ere"
    :unify-with {:espanol {:infl :present
                            :agr {:number :sing
                                  :person :1st}}}}

   #"i$"
   {:replace-with "ere"
    :unify-with {:espanol {:infl :present
                            :agr {:number :sing
                                  :person :2nd}}}}
   #"e$"
   {:replace-with "ere"
    :unify-with {:espanol {:infl :present
                            :agr {:number :sing
                                  :person :3rd}}}}
   #"iamo$"
   {:replace-with "ere"
    :unify-with {:espanol {:infl :present
                            :agr {:number :plur
                                  :person :1st}}}}
   #"ete$"
   {:replace-with "ere"
    :unify-with {:espanol {:infl :present
                            :agr {:number :plur
                                  :person :2nd}}}}
   #"ano$"
   {:replace-with "ere"
    :unify-with {:espanol {:infl :present
                            :agr {:number :plur
                                  :person :3rd}}}}})

(def present-to-infinitive-are
  {
   ;; present -are
   #"o$"
   {:replace-with "are"
    :unify-with {:espanol {:infl :present
                            :agr {:number :sing
                                  :person :1st}}}}

   #"i$"
   {:replace-with "are"
    :unify-with {:espanol {:infl :present
                            :agr {:number :sing
                                  :person :2nd}}}}

   #"e$"
   {:replace-with "are"
    :unify-with {:espanol {:infl :present
                            :agr {:number :sing
                                  :person :3rd}}}}
   
   #"iamo$"
   {:replace-with "are"
    :unify-with {:espanol {:infl :present
                            :agr {:number :plur
                                  :person :1st}}}}

   #"ete$"
   {:replace-with "are"
    :unify-with {:espanol {:infl :present
                            :agr {:number :plur
                                  :person :2nd}}}}

   #"ano$"
   {:replace-with "are"
    :unify-with {:espanol {:infl :present
                            :agr {:number :plur
                                  :person :3rd}}}}})

(def imperfect-to-infinitive-irreg1
  {
   ;; e.g.: "bevevo/bevevi/..etc" => "bere"
   #"vevo$"
   {:replace-with "re"
    :unify-with {:espanol {:infl :imperfect
                            :agr {:number :sing
                                  :person :1st}}}}

   #"vevi$"
   {:replace-with "re"
    :unify-with {:espanol {:infl :imperfect
                            :agr {:number :sing
                                  :person :2nd}}}}

   #"veva$"
   {:replace-with "re"
    :unify-with {:espanol {:infl :imperfect
                            :agr {:number :sing
                                  :person :3rd}}}}
   })

(def past-to-infinitive
  {#"ato$"
   {:replace-with "are"
    :unify-with {:espanol {:infl :past}}}

   #"ito$"
   {:replace-with "ire"
    :unify-with {:espanol {:infl :past}}}

   #"uto$"
   {:replace-with "ere"
    :unify-with {:espanol {:infl :past}}}})

(def plural-to-singular-noun-fem-1
  {#"e$"
   {:replace-with "a"
    :unify-with {:synsem {:cat :noun
                          :agr {:gender :fem
                                :number :plur}}}}})

(def plural-to-singular-noun-masc-1
  {#"i$"
   {:replace-with "o"
    :unify-with {:synsem {:cat :noun
                          :agr {:number :plur}}}}})

(def plural-to-singular-noun-masc-2 ;; e.g. "cani" -> "cane"
  {#"i$"
   {:replace-with "e"
    :unify-with {:synsem {:cat :noun
                          :agr {:number :plur}}}}})


(def plural-to-singular-adj-masc
  {#"i$"
   {:replace-with "o"
    :unify-with {:synsem {:cat :adjective
                          :agr {:gender :masc
                                :number :plur}}}}})

(def plural-to-singular-adj-fem-sing
  {#"a$"
   {:replace-with "o"
    :unify-with {:synsem {:cat :adjective
                          :agr {:gender :fem
                                :number :sing}}}}})

(def plural-to-singular-adj-fem-plur
  {#"e$"
   {:replace-with "o"
    :unify-with {:synsem {:cat :adjective
                          :agr {:gender :fem
                                :number :plur}}}}})

(def infinitive-to-infinitive
  {:identity
   {:unify-with {:synsem {:cat :verb
                          :infl :infinitive}}}})

(def lexical-noun-to-singular
  {:identity
   {:unify-with {:synsem {:cat :noun
                          :agr {:number :sing}}}}})

(defn analyze [surface-form lookup-fn]
  "return the map incorporating the lexical information about a surface form."
  (let [replace-pairs
        ;; Even though it's possible for more than one KV pair to have the same key:
        ;; e.g. plural-to-singular-noun-masc-1 and plural-to-singular-noun-masc-2 both have
        ;; #"i$", they are distinct as separate keys in this 'replace-pairs' hash, as they should be.
        (merge 
         future-to-infinitive
         imperfect-to-infinitive-irreg1
         infinitive-to-infinitive ;; simply turns :top into :infl
         lexical-noun-to-singular ;; turns :number :top to :number :sing
         past-to-infinitive
         present-to-infinitive-ire
         present-to-infinitive-ere
         present-to-infinitive-are
         plural-to-singular-noun-fem-1
         plural-to-singular-noun-masc-1
         plural-to-singular-noun-masc-2
         plural-to-singular-adj-masc
         plural-to-singular-adj-fem-plur
         plural-to-singular-adj-fem-sing
         )
        
        analyzed
        (remove fail?
                (mapcat
                 (fn [key]
                   (if (and (not (keyword? key)) (re-find key surface-form))
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

        ;; Analyzed-via-identity is used to handle infinitive verbs: converts them from unspecified inflection to
        ;; {:infl :infinitive}
        ;; Might also be used in the future to convert nouns from unspecified number to singular number.
        analyzed-via-identity
        (remove fail?
                (mapcat
                 (fn [key]
                   (if (and (keyword? key) (= key :identity))
                        (let [lexical-form surface-form
                              looked-up (lookup-fn lexical-form)]
                          (map #(unifyc 
                                 %
                                 (:unify-with (get replace-pairs key)))
                               looked-up))))
                 (keys replace-pairs)))]


    (concat
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
                               (log/debug (str (first lexeme-kv) ": looking at path: " path))
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
                                                     (unifyc (apply merge-fn (list lexeme))
                                                             {:espanol {:exception true}}))}))))
                                       lexemes)))
                           [
                            ;; 1. past-tense exceptions
                            {:path [:espanol :passato]
                             :merge-fn
                             (fn [val]
                               {:espanol {:infl :past
                                           :espanol (get-in val [:espanol :passato] :nothing)}})}

                            ;; 2. present-tense exceptions
                            {:path [:espanol :present :1sing]
                             :merge-fn
                             (fn [val]
                               {:espanol {:infl :present
                                           :espanol (get-in val [:espanol :present :1sing] :nothing)
                                           :agr {:number :sing
                                                 :person :1st}}})}
                            {:path [:espanol :present :2sing]
                             :merge-fn
                             (fn [val]
                               {:espanol {:infl :present
                                           :espanol (get-in val [:espanol :present :2sing] :nothing)
                                           :agr {:number :sing
                                                 :person :2nd}}})}

                            {:path [:espanol :present :3sing]
                             :merge-fn
                             (fn [val]
                               {:espanol {:infl :present
                                           :espanol (get-in val [:espanol :present :3sing] :nothing)
                                           :agr {:number :sing
                                                 :person :3rd}}})}

                            {:path [:espanol :present :1plur]
                             :merge-fn
                             (fn [val]
                               {:espanol {:infl :present
                                           :espanol (get-in val [:espanol :present :1plur] :nothing)
                                           :agr {:number :plur
                                                 :person :1st}}})}

                            {:path [:espanol :present :2plur]
                             :merge-fn
                             (fn [val]
                               {:espanol {:infl :present
                                           :espanol (get-in val [:espanol :present :2plur] :nothing)
                                           :agr {:number :plur
                                                 :person :2nd}}})}

                            {:path [:espanol :present :3plur]
                             :merge-fn
                             (fn [val]
                               {:espanol {:infl :present
                                           :espanol (get-in val [:espanol :present :3plur] :nothing)
                                           :agr {:number :plur
                                                 :person :3rd}}})}

                            ;; adjectives
                            {:path [:espanol :masc :plur]
                             :merge-fn
                             (fn [val]
                               {:espanol {:agr {:gender :masc
                                                 :number :plur}}})}

                            {:path [:espanol :fem :plur]
                             :merge-fn
                             (fn [val]
                               {:espanol {:agr {:gender :fem
                                                 :number :plur}}})}
                            ])]
        (if (not (empty? result))
          (concat result (exception-generator (rest lexicon)))
          (exception-generator (rest lexicon)))))))

(defn phonize [a-map a-string]
  (let [common {:phrasal false}]
    (cond (or (vector? a-map) (seq? a-map))
          (map (fn [each-entry]
                 (phonize each-entry a-string))
               a-map)

          (and (map? a-map)
               (not (= :no-espanol (get-in a-map [:espanol] :no-espanol))))
          (unifyc {:espanol {:espanol a-string}}
                  common
                  a-map)

        true
        (unifyc a-map
                {:espanol {:espanol a-string}}
                common))))

(defn agreement [lexical-entry]
  (cond
   (= (get-in lexical-entry [:synsem :cat]) :verb)
   (let [cat (atom :top)
         infl (atom :top)]
     (unifyc lexical-entry
             {:espanol {:cat cat
                         :infl infl}
              :synsem {:cat cat
                       :infl infl}}))

   (= (get-in lexical-entry [:synsem :cat]) :noun)
   (let [agr (atom :top)
         cat (atom :top)]
     (unifyc lexical-entry
             {:espanol {:agr agr
                        :cat cat}
              :synsem {:agr agr
                       :cat cat}}))

   true
   lexical-entry))

(def espanol-specific-rules
  (list agreement))

;; TODO: language-universal: promote.
(defn morph-walk-tree [tree]
  (log/debug (str "morph-walk-tree: " (fo tree)))
  (merge
   {:surface (fo tree)}
   (if (get-in tree [:comp])
     {:comp (morph-walk-tree (get-in tree [:comp]))}
     {})
   (if (get-in tree [:head])
     {:head (morph-walk-tree (get-in tree [:head]))})))

