(ns babel.francais.morphology
  (:refer-clojure :exclude [get-in merge resolve]))
(require '[babel.stringutils :refer :all])
(require '[babel.francais.morphology.nouns :as nouns])
(require '[babel.francais.morphology.verbs :as verbs])
(require '[clojure.core :as core])
(require '[clojure.string :as string])
(require '[clojure.string :refer (trim)])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer (copy dissoc-paths fail? get-in merge ref? strip-refs unifyc)])

(declare analyze)
(declare get-string)
(declare suffix-of)

;; TODO: separate part-of-speech -related functionality (e.g. the word is a verb) from
;; compositional functionality (e.g. the word has an :a and :b, so combine by concatenation, etc)
;; 
(defn get-string [word & [b]]
  (cond (and (nil? b)
             (seq? word))
        (let [result (get-string word)]
          (if (string? result)
            (trim result)
            result))
        
        ;; je + aime = j'aime
        (and (not (nil? b))
             (let [a (get-string word)
                   b (get-string b)]
               (and (= a "je")
                    (re-find #"^[aeéiou]" b))))
        (str "j'" (get-string b))

        (and (not (nil? word))
             (not (nil? b)))
        (trim (string/join " "
                           (list (get-string word)
                                 (if b (get-string b)
                                     ""))))
        (string? word)
        word
        
        (nil? word)
        nil
        
        (seq? word)
        (map (string/join " " #(get-string %))
             word)
        
        true
        (let [person (get-in word '(:agr :person))
              number (get-in word '(:agr :number))
              info (log/debug "get-string: input word: " word)]

          (log/trace (str "get-string: word: " word))
          (log/debug (str "get-string: word (stripped-refs): " (strip-refs word)))
          (log/debug (str "word's :a is a string? " (get-in word '(:a)) " => " (string? (get-in word '(:a)))))
          (log/debug (str "word's :b is a map? " (get-in word '(:b)) " => " (map? (get-in word '(:b)))))
          
          (log/debug (str "word's :a french is a string? " (get-in word '(:a :français)) " => " (string? (get-in word '(:a :français)))))

          (cond
           (= word :top) ".."
           
           (ref? word)
           (get-string @word)
           
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
            (string? (get-in word '(:a :français)))
            (string? (get-in word '(:b :français)))
            (or (= :none (get-in word '(:b :agr :number) :none))
                (= :top (get-in word '(:b :agr :number) :none)))
            )
           (str (string/trim (get-in word '(:a :français)))
                " "
                (string/trim (get-in word '(:b :français))))
           
           (and
            (string? (get-in word '(:a)))
            (string? (get-in word '(:b :français)))
            (or (= :none (get-in word '(:b :agr :number) :none))
                (= :top (get-in word '(:b :agr :number) :none)))
            )
           (str (string/trim (get-in word '(:a)))
                " "
                (string/trim (get-in word '(:b :français))))
           
           (and
            (string? (get-in word '(:a :français)))
            (get-in word '(:a :français))
            (or (= :none (get-in word '(:b :agr :number) :none))
                (= :top (get-in word '(:b :agr :number) :none)))
            (= (get-in word '(:a :infl)) :top))
           (string/trim (str (get-in word '(:a :français))
                             " " (get-string (get-in word '(:b)))))

           (= true (get-in word [:exception]))
           (get-in word [:français])

           (and
            (= (get-in word '(:infl)) :conditional)
            (string? (get-in word '(:français))))
           (verbs/conditional word)
              
           (and
            (= (get-in word '(:infl)) :future)
            (string? (get-in word '(:français))))
           (verbs/future word)
           
           (and
            (= (get-in word '(:infl)) :imperfect)
            (string? (get-in word '(:français))))
           (verbs/imperfect word)

           (and
            (= (get-in word '(:infl)) :past-p))
           (verbs/passe-compose word)
           
           (and
            (= (get-in word '(:infl)) :present)
            (string? (get-in word '(:français))))
           (verbs/present word)
           
           (and
            (get-in word '(:a))
            (get-in word '(:b)))
           (get-string
            (get-in word '(:a))
            (get-in word '(:b)))
           
           (= (get-in word '(:a)) :top)
           (str
            ".." " " (get-string (get-in word '(:b))))
           
           (and
            (= (get-in word '(:b)) :top)
            (string? (get-string (get-in word '(:a)))))
           (str
            (get-string (get-in word '(:a)))
            " " "..")
           
           (string? (get-in word [:français]))
           (get-in word [:français])
           
           true
           (throw (Exception. (str "get-string: don't know what to do with input argument " word ";français="
                                   (get-in word [:français]) "; stringness: "
                                   (string? (get-in word [:français])))))))))
  
(declare fo-ps-it)

(defn fo [input]
  (cond 
   (= input :fail)
   (str input)

   (= (type input) clojure.lang.LazySeq)
   (str "['" (string/join "','" (map fo input)) "']")

   (string? input)
   input

   (get-in input [:français])
   (string/trim (str (get-string (get-in input [:français]))))

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
          (:français expr))
     (fo-ps-it (:français expr))

     (and (map? expr)
          (:rule expr)
          (= (get-in expr '(:français :a))
             (get-in expr '(:comp :français))))
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
          (= (get-in expr '(:français :a))
             (get-in expr '(:comp :français))))
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
      (:français expr))
     (get-string (get-in expr '(:français)))

     true
     expr)))

(def infinitive-to-infinitive
  {:identity
   {:unify-with {:synsem {:cat :verb
                          :infl :infinitive}}}})

(defn exception-generator [lexicon]
  (let [lexeme-kv (first lexicon)
        lexemes (second lexeme-kv)]
    (if lexeme-kv
      (let [result (mapcat (fn [path-and-merge-fn]
                             (let [path (:path path-and-merge-fn)
                                   merge-fn (:merge-fn path-and-merge-fn)]
                               ;; a lexeme-kv is a pair of a key and value. The key is a string (the word's surface form)
                               ;; and the value is a list of lexemes for that string.
                               (log/trace (str (first lexeme-kv) ": looking at path: " path))
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
                                                             {:français {:exception true}}))}))))
                                       lexemes)))
                           [
                            ;; 1. past-tense exceptions
                            {:path [:français :past]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :past
                                           :français (get-in val [:français :past] :nothing)}})}

                            ;; 2. present-tense exceptions
                            {:path [:français :present :1sing]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :present
                                           :français (get-in val [:français :present :1sing] :nothing)
                                           :agr {:number :sing
                                                 :person :1st}}})}
                            {:path [:français :present :2sing]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :present
                                           :français (get-in val [:français :present :2sing] :nothing)
                                           :agr {:number :sing
                                                 :person :2nd}}})}

                            {:path [:français :present :3sing]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :present
                                           :français (get-in val [:français :present :3sing] :nothing)
                                           :agr {:number :sing
                                                 :person :3rd}}})}

                            {:path [:français :present :1plur]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :present
                                           :français (get-in val [:français :present :1plur] :nothing)
                                           :agr {:number :plur
                                                 :person :1st}}})}

                            {:path [:français :present :2plur]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :present
                                           :français (get-in val [:français :present :2plur] :nothing)
                                           :agr {:number :plur
                                                 :person :2nd}}})}

                            {:path [:français :present :3plur]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :present
                                           :français (get-in val [:français :present :3plur] :nothing)
                                           :agr {:number :plur
                                                 :person :3rd}}})}

                            ;; 3. imperfect-tense exceptions
                            {:path [:français :imperfect :1sing]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :imperfect
                                           :français (get-in val [:français :imperfect :1sing] :nothing)
                                           :agr {:number :sing
                                                 :person :1st}}})}
                            {:path [:français :imperfect :2sing]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :imperfect
                                           :français (get-in val [:français :imperfect :2sing] :nothing)
                                           :agr {:number :sing
                                                 :person :2nd}}})}

                            {:path [:français :imperfect :3sing]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :imperfect
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
                                           :français (get-in val [:français :imperfect :2plur] :nothing)
                                           :agr {:number :plur
                                                 :person :2nd}}})}

                            {:path [:français :imperfect :3plur]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :imperfect
                                           :français (get-in val [:français :imperfect :3plur] :nothing)
                                           :agr {:number :plur
                                                 :person :3rd}}})}

                            ;; 4. adjectives
                            {:path [:français :masc :plur]
                             :merge-fn
                             (fn [val]
                               {:français {:agr {:gender :masc
                                                 :number :plur}}})}

                            {:path [:français :fem :plur]
                             :merge-fn
                             (fn [val]
                               {:français {:agr {:gender :fem
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
               (not (= :no-français (get-in a-map [:français] :no-français))))
          (unifyc {:français {:français a-string}}
                  common
                  a-map)

        true
        (unifyc a-map
                {:français {:français a-string}}
                common))))

(defn agreement [lexical-entry]
  (cond
   (= (get-in lexical-entry [:synsem :cat]) :verb)
   (let [cat (ref :top)
         infl (ref :top)]
     (unifyc lexical-entry
             {:français {:cat cat
                         :infl infl}
              :synsem {:cat cat
                       :infl infl}}))

   (= (get-in lexical-entry [:synsem :cat]) :noun)
   (nouns/agreement lexical-entry)

   true
   lexical-entry))

(def french-specific-rules
  (list agreement))

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

(defn analyze [surface-form lexicon]
  "return the map incorporating the lexical information about a surface form.
   The function tries to analyze the surface form into a lexical form, and then 
   looks up the hypothesized lexeme using lookup-fn."
  (cond

   (re-find #"e$" surface-form)
   (concat

    ;; -er type verb
    (map (fn [lexeme]
           (unifyc
            lexeme
            {:synsem {:subcat {:1 {:agr {:number :sing
                                         :person :1st}}}
                      :infl :present}}))
         (get lexicon (string/replace surface-form #"(e)$" "$1r")))

    ;; -ir type verb
    (map (fn [lexeme]
           (unifyc
            lexeme
            {:synsem {:subcat {:1 {:agr {:number :sing
                                         :person :1st}}}
                      :infl :present}}))
         (get lexicon (string/replace surface-form #"(e)$" "ir")))
    
    (get lexicon surface-form))

   true
   (get lexicon surface-form)))
