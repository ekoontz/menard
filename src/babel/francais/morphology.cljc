(ns babel.francais.morphology
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [babel.francais.morphology.nouns :as nouns]
   [babel.francais.morphology.verbs :as verbs]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unifyc)]))

(def replace-patterns
  (concat
   nouns/replace-patterns
   verbs/replace-patterns))

(defn analyze [surface-form lexicon]
  "Analyze a single surface form into a set of lexical forms."
  (concat (if (get lexicon surface-form)
            (get lexicon surface-form))
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
           replace-patterns)))

(defn conjugate [infinitive unify-with]
  "Conjugate an infinitive into a surface form by taking the first 
   element of replace-patterns where the element's :u unifies successfully with
   unify-with."
  (let [result
        (take 1
              (remove #(nil? %)
                      (map
                       (fn [replace-pattern]
                         (let [from (nth (:g replace-pattern) 0)
                               to (nth (:g replace-pattern) 1)
                               unify-against (if (:u replace-pattern)
                                               (:u replace-pattern)
                                               :top)]
                            (if (and from to
                                     (re-matches from infinitive)
                                     (not (fail? (unifyc unify-against
                                                         unify-with))))
                              (do
                                (log/debug (str "matched infinitive: " infinitive))
                                (log/debug (str "from: " from))
                                (log/debug (str "to: " to))
                                (log/debug (str "input spec: " (strip-refs unify-with)))
                                (log/debug (str "pattern spec:"  (strip-refs unify-against)))
                                (string/replace infinitive from to)))))
                        replace-patterns)))]
    (if (empty? result)
      (throw (Exception. (str "nothing found to match infinitive: " infinitive " ; "
                              " unify-with: " (strip-refs unify-with) ".")))

      (first result))))

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
             (not (nil? (get-string b)))
             (let [a (get-string word)
                   b (get-string b)]
               (and (re-find #"^(je)$" a)
                    (re-find #"^[aeéiou]" b))))
        (let [a (get-string word)]
          (str (string/replace a #"^(j).*" (fn [[_ prefix]] (str prefix "'"))) (get-string b)))

        ;; (elle) la + est (amuseé) = (elle) l'est (amuseé)
        (and (not (nil? b))
             (let [a (get-string word)
                   b-str (get-string b)
                   debug (if (nil? b-str)
                           (throw (Exception. (str "get-string=nil for b: " (strip-refs b)))))
                   b b-str]
               (and (re-find #"^(la|le|me|se|te)$" a) ;;
                    (re-find #"^[aeéiou]" b))))
        (let [a (get-string word)]
          (str (string/replace a #"^(l|m|s|t).*"
                               (fn [[_ prefix]] (str prefix "'"))) (get-string b)))

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
            (or (= (get-in word [:infl]) :conditional)
                (= (get-in word [:infl]) :future)
                (= (get-in word [:infl]) :present))
            (string? (get-in word [:français])))
           (let [number-and-person (verbs/number-and-person number person)
                 infl (get-in word [:infl])]
             (cond
               (and number-and-person
                    (get-in word [infl number-and-person]))
               (get-in word [infl number-and-person])

               true
               (let [infinitive (cond (and (= :future (get-in word [:infl]))
                                           (get-in word [:future-stem]))
                                      (get-in word [:future-stem])
                                      ;; TODO: add checks for other -stem features, like :imperfect-stem.
                                      true
                                      (get-in word [:français]))]
                 (conjugate infinitive word))))
           
           (and
            (= (get-in word '(:infl)) :imperfect)
            (string? (get-in word '(:français))))
           (verbs/imperfect word)
           
           (= (get-in word '(:infl)) :past-p)
           (cond (get-in word [:past-participle])
                 (get-in word [:past-participle])
                 true
                 (conjugate (get-in word [:français]) word))

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
  
(defn fo [input &
          {:keys [from-language show-notes]
           :or {from-language nil
                show-notes false}}]
  (cond 
    (nil? input)
    nil
    
    (= input :fail)
    (str input)

    (or (seq? input)
        (vector? input))
    (str "(" (string/join " , " 
                          (remove #(= % "")
                                  (map #(let [f (fo %)] (if (= f "") "" (str "" f ""))) input)))
         ")")
    
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
                                                            (copy lexeme))]
                                           (if (not (= :none (get-in lexeme path :none)))
                                             (list {(get-in lexeme path :none)
                                                    (unifyc
                                                     (dissoc-paths lexeme [path
                                                                           [:français :français]])
                                                     (unifyc (apply merge-fn (list lexeme))
                                                             {:français {:exception true}}))}))))
                                       lexemes)))
                           [
                            ;; 1. past-tense exceptions
                            {:path [:français :past-participle]
                             :merge-fn
                             (fn [val]
                               {:français {:infl :past-p
                                           :français (get-in val [:français :past-participle] :nothing)}})}

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
