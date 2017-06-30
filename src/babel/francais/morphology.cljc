(ns babel.francais.morphology
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [babel.francais.morphology.adjectives :as adjectives]
   [babel.francais.morphology.nouns :as nouns]
   [babel.francais.morphology.verbs :as verbs]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unify unifyc)]))

(def regular-patterns
  (concat
   nouns/regular-patterns
   verbs/regular-patterns))

(def irregular-patterns
  (concat
   adjectives/irregular-patterns
   ;; TODO: noun/irregular-patterns
   verbs/irregular-patterns))

(defn analyze [surface-form lexicon]
  "Analyze a single surface form into a set of lexical forms."
  (concat (if (get lexicon surface-form)
            (get lexicon surface-form))
          (mapcat
           (fn [replace-pattern]
             (let [ ;; regular expression that matches the surface form
                   from (nth (:p replace-pattern) 0)]
               (log/trace (str "matching replace-pattern:" replace-pattern " against surface-form: " surface-form))
               (when (re-matches from surface-form)
                 (log/debug (str "matched replace-pattern:" replace-pattern " against surface-form: " surface-form))
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
           regular-patterns)))

(defn lookup-in [lexicon {spec :spec}]
  (let [infinitive (get-in spec [:français :infinitive])
        lexemes (reduce concat (vals lexicon))]
    (if (fail? spec)
      (throw (Exception. (str "lookup-in was given spec=fail."))))
    (filter (fn [lexeme]
              (and
               (= infinitive (get-in lexeme [:français :infinitive] ::none))
               
               ;; add other (get-in) constraints here, if necessary:
               ;; ..
               
               ;; (unify) with spec should be last since it is expensive:
               (not (fail? (unify spec lexeme)))
               
               (or (log/debug (str " found a unify match:" (strip-refs lexeme))) true)
               
               (or (log/debug (str "lookup-in: matched: " (strip-refs lexeme))) true)))
            lexemes)))

(defn conjugate [infinitive unify-with & [lexicon]]
  "Conjugate an infinitive into a surface form by taking the first 
   element of regular-patterns where the element's :u unifies successfully with
   unify-with. If lexicon is supplied, look up infinitive in lexicon and use exceptional form of
   first return value, if any."
  (if (nil? infinitive)
    ""
    (do
      (log/debug (str "conjugate1: infinitive=" infinitive "; unify-with=" unify-with))
      (let [exceptional-lexemes
            (lookup-in lexicon
                       {:spec {:français {:infinitive infinitive
                                          :exception true}}})
            exceptional-surface-forms
            (map #(get-in % [:français :français])
                 exceptional-lexemes)
            regulars
            (remove nil?
                    (map
                     (fn [replace-pattern]
                       (let [from (nth (:g replace-pattern) 0)
                             to (nth (:g replace-pattern) 1)
                             unify-against (if (:u replace-pattern)
                                             (:u replace-pattern)
                                             :top)
                             unified (unify unify-against
                                            unify-with)]
                         (when (and from to infinitive
                                    (not (= :fail unified))
                                    (re-matches from infinitive))
                           (log/debug (str "infinitive: " infinitive " matched pattern: "
                                           (strip-refs replace-pattern)))
                           (let [output (string/replace infinitive from to)]
                             (log/debug (str  "output: " output))
                             output))))
                     regular-patterns))
            
            diagnostics (log/debug (str "emptyness of exceptions:" (empty? exceptional-surface-forms)))
            
            diagnostics (log/debug (str "emptyness of regulars:" (empty? regulars)))
            ;; Take the first of any exceptions found; if not, take the first regular conjugation.
            ;; if neither, it's an error in this implementation of French morphology so throw so it can be
            ;; detected and fixed.
            results (take 1 (concat exceptional-surface-forms regulars))]
        (not (empty? results))
        (first results)

        true
        (do
          (log/warn (str "no conjugation (irregular nor regular) was found for: " infinitive "; "
                         "simply returning infinitive unconjugated."))
          infinitive)))))

;; TODO: separate part-of-speech -related functionality (e.g. the word is a verb) from
;; compositional functionality (e.g. the word has an :a and :b, so combine by concatenation, etc)
;; 
(defn get-string [word & [b lexicon]]
  (log/debug (str "get-string:" word))
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
          (log/trace (str "get-string: word (stripped-refs): " (strip-refs word)))
          (log/trace (str "word's :a is a string? " (get-in word '(:a)) " => " (string? (get-in word '(:a)))))
          (log/trace (str "word's :b is a map? " (get-in word '(:b)) " => " (map? (get-in word '(:b)))))
          
          (log/trace (str "word's :a french is a string? " (get-in word '(:a :français)) " => " (string? (get-in word '(:a :français)))))

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

            (= true (get-in word [:exception]))
            (let [foo (log/debug (str "exception word:" (strip-refs word)))
                  bar (log/debug (str "exception word1:" (get-in word [:français])))]
              (get-in word [:français]))
            
            true
            (let [infinitive (get-in word [:français])]
              (log/debug (str "calling: conjugate: infinitive=" infinitive "; word=" (strip-refs word)))
              (conjugate infinitive {:synsem word}))))))
  
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
  (mapcat
   (fn [lexeme-kv]
          (let [lexemes (second lexeme-kv)
                result (mapcat (fn [path-and-merge-fn]
                                 (let [path (:path path-and-merge-fn)
                                       merge-fn (:merge-fn path-and-merge-fn)]
                                   ;; a lexeme-kv is a pair of a key
                                   ;; and value. The key is a
                                   ;; string (the word's surface form)
                                   ;; and the value is a list of lexemes for that string.
                                   (log/trace (str (first lexeme-kv) ": looking at path: " path))
                                   (mapcat (fn [lexeme]
                                             (if-let [value (get-in lexeme path)]
                                               (log/debug " value@" path ": " value))
                                             (if (not (= ::none (get-in lexeme path ::none)))
                                               (let [infinitive (get-in lexeme [:français :français])
                                                     exceptional-lexeme
                                                     (apply merge-fn (list lexeme))]
                                                 (log/debug " exceptional-lexeme: " exceptional-lexeme)
                                                 (let [exceptional-lexemes
                                                       (cond (or (seq? exceptional-lexeme)
                                                                 (vector? exceptional-lexeme))
                                                             exceptional-lexeme
                                                             true [exceptional-lexeme])]
                                                   (when (not (empty? exceptional-lexemes))
                                                     (log/debug "infinitive: " infinitive)
                                                     (log/debug " path: " path))
                                                   (map (fn [exceptional-lexeme]
                                                          (log/debug "   conjugated:"
                                                                     (get-in exceptional-lexeme
                                                                             [:français :français]))
                                                          (let [exceptional-lexeme
                                                                (unify
                                                                 exceptional-lexeme
                                                                 (dissoc-paths lexeme [path
                                                                                       [:français :français]])
                                                                 {:français {:infinitive infinitive
                                                                             :exception true}})
                                                                surface
                                                                (get-in exceptional-lexeme [:français :français])]
                                                            (log/debug (str "  surface: " surface " => " (strip-refs exceptional-lexeme)))
                                                            {surface
                                                             exceptional-lexeme}))
                                                        exceptional-lexemes)))))
                                           lexemes)))
                               irregular-patterns)]
            result))
   lexicon))

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
