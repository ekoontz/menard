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
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unify)]))

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
  (log/debug (str "analyze: " surface-form))
  (let [retval 
        (concat (if (get lexicon surface-form)
                  (get lexicon surface-form))
                (mapcat
                 (fn [replace-pattern]
                   (let [ ;; regular expression that matches the surface form
                         from (nth (:p replace-pattern) 0)]
                     (log/trace (str "matching replace-pattern:" replace-pattern " against surface-form: " surface-form))
                     (when (re-matches from surface-form)
                       (log/debug (str "matched replace-pattern:" (strip-refs replace-pattern)
                                       " against surface-form: " surface-form))
                       (let [;; expression that is used by string/replace along with the first regexp and the surface form,
                             ;; to create the lexical string
                             to (nth (:p replace-pattern) 1)
                             
                             ;; unifies with the lexical entry to create the inflected form.
                             unify-with (if (:u replace-pattern)
                                          (:u replace-pattern)
                                          :top) ;; default unify-with
                             
                             lex (string/replace surface-form from to)]
                         (map #(-> %
                                   (dissoc-paths [[:français :conjugated]])
                                   (unify {:français {:conjugated true}}))
                              (filter (fn [result] (not (= :fail result)))
                                      (map (fn [lexical-entry]
                                             (unify unify-with lexical-entry))
                                           (get lexicon lex))))))))
                 regular-patterns))]
    (if (not (empty? retval))
      (log/debug (str "analyze '" surface-form "': count=" (count retval) ";" (string/join "," (map strip-refs retval))))
      (log/warn (str " analyze: could not analyze surface-form: " surface-form)))
    retval))

(defn lookup-in [lexicon {spec :spec}]
  (let [infinitive (get-in spec [:français :infinitive])
        lexemes (reduce concat (vals lexicon))]
    (if (= :fail spec)
      (throw (Exception. (str "lookup-in was given spec=fail."))))
    (filter (fn [lexeme]
              (and
               (= infinitive (get-in lexeme [:français :infinitive] ::none))
               
               ;; add other (get-in) constraints here, if necessary:
               ;; ..
               
               ;; (unify) with spec should be last since it is expensive:
               (not (= :fail (unify spec lexeme)))
               
               (or (log/debug (str "lookup-in: matched: " (strip-refs lexeme))) true)))
            lexemes)))

(defn conjugate [infinitive conjugate-with & [if-underspecified]]
  "Conjugate an infinitive into a surface form by taking the first 
   element of regular-patterns where the element's :u unifies successfully with
   conjugate-with."
  (let [underspecified?
        (or 
         (= :top (get-in conjugate-with [:synsem :cat] :top))
         (and
          (= :verb (get-in conjugate-with [:synsem :cat]))
          (= :top (get-in conjugate-with [:synsem :agr] :top))))]
    (cond (nil? infinitive)
          (throw (Exception. (str "conjugate passed null infinitive.")))
          
          ;; default here is return log/warn rather than throwing an exception.
          (and underspecified?
               (not (= :if-underspecified :error)))
          (do
            (log/warn (str "conjugate: underspecified: returning infinitive: " infinitive "; conjugate-with: " (strip-refs conjugate-with)))
            (str infinitive))
          
          underspecified?
          (throw (Exception. (str "conjugate: underspecified: returning infinitive: " infinitive "; conjugate-with: " (strip-refs conjugate-with))))
        
          true
          (let [diag
                (log/debug (str "conjugate: infinitive=" infinitive "; conjugate-with: "
                                (strip-refs conjugate-with)))
                
                lookup-spec (strip-refs
                             (unify
                              (dissoc-paths
                               conjugate-with [[:français :exception]])
                              {:français {:infinitive infinitive
                                          :exception true}}))
                diag
                (log/debug (str "conjugate: lookup-spec: " lookup-spec))
                
                diag
                (if (= :fail lookup-spec)
                  (log/debug (str "fail-path for conjugate-with:"
                                  (strip-refs conjugate-with) "; "
                                  {:français {:infinitive infinitive
                                              :exception true}} ": "
                                  (dag_unify.core/fail-path
                                   conjugate-with
                                   {:français {:infinitive infinitive
                                               :exception true}}))))
                
                error (if (= :fail lookup-spec)
                        (throw (Exception. (str "conjugate was given conjugate-with=:fail; "
                                              "arg1="
                                              (strip-refs (dissoc-paths
                                                           conjugate-with [[:français :exception]])) "; "
                                              "arg2="
                                              {:français {:infinitive infinitive
                                                          :exception true}}))))
                
                exceptional-surface-forms
                (remove nil?
                        (map #(let [{path :path
                                     prefix :prefix
                                     suffix :suffix
                                     unify-with :unify-with} %]
                                (if (not (= :fail (unify unify-with lookup-spec)))
                                  (cond (and (not (nil? prefix))
                                             (not (nil? suffix)))
                                        (str (get-in lookup-spec [:français prefix]) suffix)

                                        (not (nil? path))
                                        (get-in lookup-spec path)
                                        
                                        true (throw (Exception. (str "problem with irregular pattern:" %))))))
                             verbs/irregular-conjugations))
                
                regulars
                (remove nil?
                        (map
                         (fn [replace-pattern]
                           (let [from (nth (:g replace-pattern) 0)
                                 to (nth (:g replace-pattern) 1)]
                             (if (and from to infinitive
                                      (re-matches from infinitive)
                                      (not (= :fail
                                              (let [unify-against (if (:u replace-pattern)
                                                                    (:u replace-pattern)
                                                                    :top)]
                                                (unify unify-against
                                                       conjugate-with)))))
                                              
                               (string/replace infinitive from to))))
                         regular-patterns))]
            (let [results (concat exceptional-surface-forms regulars [infinitive])]
              (first results))))))
  
(defn pre-conjugate [spec]
  "add structure-sharing so that (defn conjugate) can conjugate words correctly."
  (unify spec
         (let [cat (atom :top)
               agr (atom :top)
               infl (atom :top)]
           {:synsem {:cat cat
                     :infl infl
                     :agr agr}
            :français {:cat cat
                       :infl infl
                       :agr agr}})))

;; TODO: separate part-of-speech -related functionality (e.g. the word is a verb) from
;; compositional functionality (e.g. the word has an :a and :b, so combine by concatenation, etc)
;; 
(defn get-string [word & [b]]
  (cond 
        (and (nil? b)
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
              number (get-in word '(:agr :number))]
          (log/debug "get-string: input word: " (strip-refs word))
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
            (do (log/debug (str "exception word:" (strip-refs word)))
                (log/debug (str "exception word1:" (get-in word [:français])))
                (get-in word [:français]))

            (nil? (get-in word [:français]))
            ""

            true
            (let [infinitive (get-in word [:français])
                  debug (log/debug (str "input to conjugate: infinitive: " infinitive "; spec:"
                                        (strip-refs word)))
                  synsemize ;; convert _word_ back into what (defn conjugate) can deal with.
                  (pre-conjugate {:français word})
                  result (conjugate infinitive synsemize)]
              (log/debug (str "result of conjugate: " result))
              result)))))
 
(defn fo [input &
          {:keys [from-language show-notes]
           :or {from-language nil
                show-notes false}}]
  (cond
    (and (map? input)
         (:surface input))
    (:surface input)
    
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
          (unify {:français {:français a-string}}
                 common
                 a-map)
        true
        (unify a-map
               {:français {:français a-string}}
               common))))
