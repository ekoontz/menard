(ns menard.lexiconfn
  (:require
   [clojure.string :as string]
   [menard.exception :refer [exception]]
   #?(:clj [clojure.java.io :as io :refer [resource]])
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [menard.log :as log])
   [dag_unify.serialization :as s :refer [serialize]]
   [dag_unify.core :as u :refer [fail? unify]]
   [dag_unify.diagnostics :as diag]
   [dag_unify.dissoc :as d]))

;; This is used to a convert human-friendly lexicon
;; into a machine-friendly data structure.

;; TODO: consider merging contents of this into morphology.cljc and remove this namespace.

;; developer-mode? must be set to true for
;; lexemes-to-trace and rules-to-trace to run.
(def developer-mode? false)

;; lexemes-to-trace: show more logging for a certain set of lexemes
;; during lexical compilation.
(def lexemes-to-trace #{})

;; rules-to-trace: show more logging for a certain set of rules
;; during processing of the lexemes in lexemes-to-trace.
;; the rules are by convention defined in
;; <language>/lexicon/rules.edn.
(def rules-to-trace #{})

;; examples:
;;(def lexemes-to-trace #{"levantarse"})
;;(def rules-to-trace #{:reflexive})

(defn display-derivation [deriv]
  (->> (seq (zipmap (vals deriv) (keys deriv)))
       (filter (fn [[x y]]
                 (true? (get x :applied?))))
       (map (fn [[x y]]
              (if (get x :ci)
                [(get x :menard.lexiconfn/order) y (get x :ci)]
                [(get x :menard.lexiconfn/order) y])))
       (sort (fn [x y]
               (< (first x) (first y))))
       (map (fn [[x y z]]
              (if z
                [y z]
                [y])))))

(defn derivation-of [lexeme]
  (display-derivation (u/get-in lexeme [:menard.lexiconfn/derivation])))

(defn pprint [lexeme]
  (let [derivation (u/get-in lexeme [:menard.lexiconfn/derivation])
        head-derivation (u/get-in lexeme [:head-derivation])
        comp-derivation (u/get-in lexeme [:comp-derivation])
        exceptions (u/get-in lexeme [:exceptions])]
    (cond
      (map? lexeme)
      (-> lexeme
          (dissoc :menard.lexiconfn/derivation)
          (dissoc :head-derivation)
          (dissoc :comp-derivation)
          ((fn [lexeme]
             (merge
              lexeme
              (cond (and (not (keyword? derivation)) (seq derivation))
                    {:derivation (menard.lexiconfn/display-derivation derivation)}
                    (and (not (keyword? head-derivation)) (seq head-derivation))
                    {:head-derivation (menard.lexiconfn/display-derivation head-derivation)}
                    (and (not (keyword? comp-derivation)) (seq comp-derivation))
                    {:comp-derivation (menard.lexiconfn/display-derivation comp-derivation)})
              (cond (keyword? exceptions)
                    {}
                    (seq exceptions)
                    {:exceptions (vec exceptions)}
                    :else {}))))
          u/pprint)
      :else lexeme)))

(defn add-derivation [rule-name consequent antecedent-index consequent-index]
  {::derivation {rule-name (merge
                            (if (u/get-in consequent [:derivation :sense])
                              {:sense (u/get-in consequent [:derivation :sense])}
                              {})
                            (if consequent-index
                              {:ci consequent-index}
                              {})
                            {:applied? (if consequent true false)
                             ::order antecedent-index})}})

(defn encode-derivation [derivation]
  (if (keyword? derivation) derivation
      (string/join " "
                   (sort (into {}
                               (map (fn [[k v]]
                                      (if (get v :applied?)
                                        [(get v (keyword (str "menard.lexiconfn/order")))
                                         (if (get v :sense) [k (get v :sense)] k)]))
                                    derivation))))))

(defn eval-surface-fns [consequent lexeme]
  (if (seq (:exceptions consequent))
    (merge consequent
           {:exceptions (map (fn [exception]
                               (let [surface (:surface exception)]
                                 (cond
                                   (map? surface)
                                   (merge exception
                                          {:surface (str (u/get-in lexeme (:prefix surface))
                                                               (:suffix surface))})
                                   (string? surface)
                                   exception
                                   :else
                                   (exception "Cannot find surface form given intermediate surface value: " surface))))
                             (:exceptions consequent))})
    consequent))
  
(defn apply-rule-to-lexeme [rule-name lexeme consequent antecedent antecedent-index
                            consequent-index include-derivation?]
  (log/debug (str "rule-name: " rule-name))
  (log/debug (str "consequent: " consequent))
  (log/debug (str "lexeme: " (pprint lexeme)))
  (let [consequent (eval-surface-fns consequent lexeme)
        existing-exceptions (:exceptions lexeme)
        new-exceptions (:exceptions consequent)
        result (unify (dissoc lexeme :exceptions)
                      (dissoc consequent :exceptions)
                      (if (or (seq existing-exceptions)
                              (seq new-exceptions))
                        {:exceptions (concat existing-exceptions
                                             new-exceptions)}
                        :top))]
    (log/debug (str "apply-rule-to-lexeme: "
                    ;;"lexeme: " (u/pprint lexeme)
                    "; consequent: " (u/pprint consequent)
                    "; antecedent:" (u/pprint antecedent)
                    "; consequent-index:" consequent-index
                    "; include-derivation? " include-derivation?
                    ;;"; result: " (u/pprint result)
                    ))
    (cond (= :fail result)
          (let [fail-path (diag/fail-path lexeme consequent)
                error-message (str "rule: " rule-name " failed to unify lexeme: "
                                   "'" (u/get-in lexeme [:canonical]) "'"
                                   (when (u/get-in lexeme [:sense])
                                     (str " sense: " (u/get-in lexeme [:sense])))
                                   "; fail-path was: " fail-path ";"
                                   " lexeme's value for path: " (u/get-in lexeme fail-path)
                                   " consequent's value for path: " (u/get-in consequent fail-path)
                                   (if-let [derivation (u/get-in lexeme [::derivation])]
                                     (str "; lexemes' derivation: "
                                          (vec (display-derivation derivation))))
                                   "; whole lexeme: " (pprint lexeme))]
            (log/error error-message)
            (exception error-message))
          :else
          (do (log/debug (str "apply-rule-to-lexeme: lexeme: " lexeme " with conseq: " consequent "= " (pprint result)))
              (if include-derivation?
                (unify (dissoc result :derivation)
                       (add-derivation rule-name consequent
                                       antecedent-index consequent-index))
                ;; else, remove :derivation to save memory and runtime
                ;; processing:
                (dissoc result :derivation))))))

(defn process-antecedent [m canonical]
   (if (= (type (:canonical m)) java.util.regex.Pattern)
     (if (re-matches (:canonical m) canonical)
       ;; the matches, so remove it from the map since
       ;; we have determined that the rest of the
       ;; antecedent may be checked:
       (dissoc m :canonical)
       ;; if it doesn't match, then don't bother
       ;; checking the rest of the antecent, since
       ;; it's failed the :canonical check:
       :fail)
     m))

(defn apply-rules-to-lexeme
  "given a lexeme and a list of rules, return a list
   of all the possible lexemes following from the
   consequent of each rule in the list."
  [rules lexeme i include-derivation?]
  (cond

    ;; a null rule: commented out with (comment ..):
    (and (seq rules) 
         (nil? (first rules)))
    (apply-rules-to-lexeme (rest rules)
                           lexeme
                           i include-derivation?)

    (seq rules)
    (let [rule (first rules)
          antecedent (:if rule)
          consequents (:then rule)
          canonical (u/get-in lexeme [:canonical])
          log-fn (fn [msg]
                   (if (contains? lexemes-to-trace canonical)
                     (log/info msg)))
          log-rule-fn (fn [msg]
                        (if (and (contains? rules-to-trace (:rule rule))
                                 (contains? lexemes-to-trace canonical))
                          (log/info msg)))]
      (when developer-mode? (log-fn (str "apply-rules-to-lexeme: applying rule: " (:rule rule) " to lexeme: '" canonical "'.")))
      (when developer-mode? (log-rule-fn (str "apply-rules-to-lexeme: rule: " (:rule rule) "'s antecedent: " antecedent)))
      (let [antecedent (process-antecedent antecedent canonical)]
        (if (not (= :fail (unify antecedent lexeme)))
          ;; offset with '1' (i.e. '1' is the first element in the
          ;; derivation consequents
          (do
            (when developer-mode? (log-rule-fn (str "apply-rules-to-lexeme: antecedent matched.")))
            (->> (vec (zipmap (range 1 (+ 1 (count consequents))) consequents))
                 (map (fn [[consequent-index consequent]]
                        (apply-rule-to-lexeme (:rule rule) lexeme consequent antecedent i (if (> (count consequents) 1) consequent-index) include-derivation?)))
                 (mapcat (fn [new-lexeme]
                           (apply-rules-to-lexeme (rest rules) new-lexeme (+ i 1) include-derivation?)))))
          
          ;; else: failed to unify this lexeme antecedent of the rule,
          ;; so the rule doesn't apply to the lexeme.
          (do (when developer-mode? (log-rule-fn (str "apply-rules-to-lexeme: antecedent: " antecedent " did not match lexeme: " (pprint lexeme))))
              (apply-rules-to-lexeme (rest rules)
                                     (let [lexeme (if include-derivation? 
                                                    (unify lexeme (add-derivation (:rule rule) nil i nil))
                                                    lexeme)]
                                       lexeme)
                                     (+ i 1) include-derivation?)))))
    :else
    [lexeme]))

(defn apply-rules-to-lexicon [lexicon rules include-derivation?]
  (into {}
        (for [k (sort (keys lexicon))]
          (let [lexemes (seq (get lexicon k))
                include-derivation? include-derivation?]
            (log/debug (str "apply-rules-to-lexicon: applying rules for: " k))
            (when lexemes ;; keys whose value is nil or null are removed.
              [k (->> lexemes
                      (map (fn [lexeme]
                             (if (nil? lexeme)
                               (exception (str "lexeme is unexpectedly nil; lexicon: " lexicon)))
                             (log/debug (str "apply-rules-to-lexicon: 1st step: for lexeme: " lexeme))
                             (unify lexeme
                                    {:phrasal? false
                                     :canonical (u/get-in lexeme [:canonical] k)})))
                      (mapcat (fn [lexeme]
                                (log/debug (str "apply-rules-to-lexicon: 2nd step: for lexeme: " lexeme))
                                (apply-rules-to-lexeme rules lexeme 0 include-derivation?))))])))))

(defn apply-rules-in-order [lexicon rules include-derivation?]
  (let [include-derivation? include-derivation?]
    (if (empty? rules)
      lexicon
      (-> lexicon
          (apply-rules-to-lexicon rules include-derivation?)))))

(defn apply-to-every-lexeme [lexicon map-fn]
  (if (not (map? lexicon))
    (exception (str "input lexeme is not a map; it is: " (vec lexicon))))
  (let [result 
        (into {}
              (for [[k lexemes-for-k] lexicon]
                (let [lexemes-for-k (remove nil? lexemes-for-k)]
                  (let [surface k
                        log-fn (if (contains? lexemes-to-trace surface)
                                 (fn [msg] (log/info msg)))]
                    (if developer-mode? (log-fn (str "apply-to-every-lexeme: '" surface "'")))
                    [k
                     (do
                       (log/debug (str "K: " k))
                       (log/debug (str "V: " (vec lexemes-for-k)))
                       (map map-fn lexemes-for-k))]))))]
    result))
    
#?(:clj
   (defn read-and-eval [rules-filename]
     (log/info (str "read-and-eval with rules-filename: " rules-filename))
     (-> rules-filename
         ((fn [filename]
            (if (re-find #"^file:///" filename)
              (do
                (log/debug (str "got a file:/// filename: " filename))
                filename)

              ;; else, assume it's a relative path, in which case we
              ;; we have to "cast" the filename to an io/resource,
              ;; which uses the JVM classpath, not the local filesystem, for relative paths
              (do
                (log/debug (str "got a non-file:/// filename: " filename))
                (io/resource filename)))))
         slurp
         read-string
         ((fn [rule]
            (eval rule))))))

;; (read-and-eval) doesn't do anything and should not be called from
;; Clojurescript. This definition is provided to avoid Clojurescript warnings
;; about it being undefined.
#?(:cljs
   (defn read-and-eval [rules-filename]))

(defn matching-lexemes
  "given a surface form _surface_, find all matching lexical entries."
  [surface lexicon morphology]
  (let [log-fn (if (contains? lexemes-to-trace surface)
                 (fn [msg] (log/info msg))
                 (fn [msg] (log/debug msg)))

        debug (log-fn (str "matching-lexemes for surface: '" surface "'"))
        
        ;; Apply morphological rules against surface to find a set of hypotheses
        ;; about the surface form. Each morphological rule has a :p key,
        ;; which we used to turn the surface form in to the canonical form.
        ;; We then use the :u key, also in the rule, to find the agreement and infl
        ;; specificities of this inflected form.
        from-inflected
        ;; Get all the morphological rules. Each
        ;; rule has a :p (parse) key. The value of this is a pair <_from_,_to_>
        ;; and each _from_ is a regexp. If the _from_ matches _surface_, then
        ;; find C, by running string/replace with _surface_, _from_, and _to_:
        (->> morphology

             ;; find all the rules where the "from" rule
             (map (fn [rule]
                    (let [{u :u [from to] :p} rule]
                      (when (re-find from surface)
                        (log-fn (str "matched surface: " surface " with regexp from: " from " and to: " to "; so finally: " (string/replace surface from to) " with derivation: " (-> u :derivation)))
                        {:canonical (string/replace surface from to)
                         :u u}))))
             (filter #(not (nil? %)))

             ((fn [rules]
                (log-fn (str "found: " (count rules) " matching rules."))
                rules))

             ;; Now we have a set of tuples T, each member of which has form: {:u U, :canonical C},
             ;; each of which is a guess about the word, where:
             ;; - C is the canonical form: the base, normalized version of the surface form _surface_.
             ;; - U is what must unify with the lexical forms found, for this guess to be valid.
             ;;
             ;; Furthermore, define L to be the lexemes in the lexicon which have the same
             ;; canonical form (i.e. have {:canonical C} for some member of the set of tuples T.
             ;; First we get this set L:
             (mapcat (fn [tuple]
                       (log/debug (str "looking at tuple: " tuple))
                       (->>
                        (get lexicon (:canonical tuple))

                        ;; remove exceptions:
                        (filter #(= false (u/get-in % [:inflected?] false)))

                        ;; and unify with its respective U:
                        (map #(unify % (:u tuple))))))

             ;; remove all the guesses that failed unification with U:
             (filter #(not (= :fail %)))

             ;; Now we have a set of lexemes each of which has a canonical form which
             ;; is the result of applying a morphological rule to the input _surface_,
             ;; and where the unification with that rule was successful.
             ;; where the _surface_ has been
             ((fn [lexemes]
                (log-fn (str "found: " (count lexemes) " inflections for surface: " surface  "."))
                lexemes))

             (map (fn [lexeme]
                    (log-fn (str "  " surface " -> "
                                   (select-keys (dag_unify.diagnostics/strip-refs lexeme)
                                                [:canonical :sense])))
                    lexeme))

             ;; Add {:surface surface} to the output:
             (map (fn [lexeme]
                    (unify lexeme
                           {:surface surface})))

             (filter #(not (= :fail %))))

        ;; Finally, remove any results which are overgeneralized based on regular morphological
        ;; rules, if the lexeme has any exceptions. Below, the exceptional surface forms
        ;; are used to  cancel these potential overgeneralizations.
        ;; For example, applying the rules for regular verbs in
        ;; English, for infl present and agr 3rd sing,
        ;; the singular form of "be" is "bes", but there is an exceptional form "is" that should
        ;; be used instead. So this filter removes the spurious "bes" from the hypotheses generated
        ;; from _from_inflected_ above.
        filter-against-exceptions
        (filter (fn [analyze-hypothesis]
                  (log/debug (str "== filtering possible inflection: " (u/get-in analyze-hypothesis [:surface]) " -> " (u/get-in analyze-hypothesis [:canonical])))
                  (let [filter-with
                        {:infl (u/get-in analyze-hypothesis [:infl])
                         :agr (u/get-in analyze-hypothesis [:agr])}]
                    (log-fn (str "filtering with: " filter-with))
                    (when (seq (:exceptions analyze-hypothesis))
                      (log-fn (str " count of exceptions found for this guess: " (count (:exceptions analyze-hypothesis)))))
                    (empty? (filter #(not (= :fail (unify % filter-with)))
                                    (:exceptions analyze-hypothesis)))))
                from-inflected)

        debug (log-fn (str "this many inflected forms after filtering against exceptions: "
                             (count filter-against-exceptions)))

        debug (log-fn (str "  the senses (after filtering against exceptions):"
                             (vec (map #(u/get-in % [:sense])
                                       filter-against-exceptions))))
        
        ]
    (let [from-regular-morphology
          (vec (set filter-against-exceptions))
          ;; the lexicon contains both canonical forms and exceptions.
          ;; this complicated filter below is supposed to enforce the following
          ;; contraint:
          ;; 1. Get all forms where there is an inflection resulting in _surface_.
          ;; 2.a. If there are no inflected forms (i.e. 1. is empty), then return any forms from the lexicon
          ;;      that are the same as the input. This is for words that aren't inflected, for example
          ;;      determiners, prepositions, pronouns, etc; in general, closed-class lexemes.
          ;; 2.b. If there are inflected forms, return any forms from the lexicon where the canonical form
          ;;      of the verb is different from the input. This is for the rest of words, (i.e. open-class lexemes).
          exceptions (filter #(and (or (= true (:exception %))
                                       (= true (:inflected? %)))
                                   (or (= ::unspec (:surface % ::unspec))
                                       (= surface (:surface % ::unspec))))
                             (get lexicon surface))]
      (when (and (seq from-regular-morphology)
                 (seq exceptions))
        (log-fn (str "(matching-lexemes '" surface "'): both regular inflections (" (count from-regular-morphology) ") and exceptions (" (count exceptions) ").")))
      (log-fn (str "found: " (count from-regular-morphology) " regular analyzed form"
                      (when (not (= (count from-regular-morphology) 1))
                        "s")
                      " for surface form: " surface "."))
      (log-fn (str "found: " (count exceptions) " exception"
                      (when (not (= count exceptions 1))
                        "s")
                      " for surface form: " surface "."))
      (let [result
            (concat
             from-regular-morphology
             exceptions)]
        (log-fn (str "from-regular-morphology: " (count from-regular-morphology) " analyses for: " surface "."))
        (log-fn (str "   with senses: " (vec (map #(u/get-in % [:sense]) from-regular-morphology))))
        (log-fn (str "exceptions: " (count exceptions) " analyses for: " surface "."))
        (log-fn (str "   with senses: " (vec (map #(u/get-in % [:sense]) exceptions))))
        (log-fn (str "returning: " (count result) " analyses for: " surface "."))
        (log-fn (str "   with senses: " (vec (map #(u/get-in % [:sense]) result))))

        result))))

(defn add-exceptions-to-lexicon
  "augment existing lexicon with new entries for all the exceptions possible for the input lexicon."
  [lexicon]
  (log/debug (str "generating exceptions.."))
  (let [exceptions-for (fn [canonical lexemes]
                         ;; "generate all the exceptions possible for the sequence _lexemes_, each of which 
                         ;;  has _canonical_ as the canonical form for the exception."
                         (log/debug (str "canonical: " canonical))
                         (let [merge-all (fn [args]
                                           (if (seq args)
                                             (reduce (fn [a b] (merge-with concat a b)) args)))]
                           (->> lexemes
                                (mapcat (fn [lexeme]
                                          (let [log-fn (if (contains? lexemes-to-trace canonical)
                                                         (fn [msg] (log/info msg))
                                                         (fn [msg] (log/debug msg)))]
                                            (log-fn (str "add-exceptions-to-lexicon: "
                                                           "canonical: '" canonical "'; " 
                                                           "lexeme: " (pprint lexeme)))
                                            (when (not (map? lexeme))
                                              (exception (str "the lexeme was unexpectedly not a map: " lexeme "; canonical: " canonical)))
                                            (->> (:exceptions lexeme)
                                                 (map (fn [exception]
                                                        (log-fn (str "looking at candidate exception: " (pprint exception)))
                                                        (let [u-result
                                                              (unify
                                                               (d/dissoc-in lexeme [:exceptions])
                                                               exception
                                                               {:exception? true
                                                                :inflected? true
                                                                :canonical canonical})]
                                                          (log-fn (str "new exception: " (pprint u-result)))
                                                          (if (not (= :fail u-result))
                                                            {(:surface exception)
                                                             [u-result]}

                                                            (let [fp
                                                                  (diag/fail-path (d/dissoc-in lexeme [:exceptions]) exception)]
                                                              (log-fn (str "(unify lexeme exception) failed at path: " fp))
                                                              (log-fn (str "   lexeme value:    " (u/get-in (d/dissoc-in lexeme [:exceptions]) fp)))
                                                              (log-fn (str "   exception value: " (u/get-in exception fp))))))))))))
                                merge-all)))]
    (->>
     (cons lexicon
           (map (fn [canonical]
                  (log/debug (str "calling exceptions-for with canonical: " canonical " with: " (vec (get lexicon canonical))))
                  (exceptions-for canonical (get lexicon canonical)))
                (keys lexicon)))
     (reduce (fn [a b] (merge-with concat a b))))))

(defn serialized-value-map [the-map]
  (zipmap
   (keys the-map)
   (map (fn [value-is-a-seq]
          (vec (->> value-is-a-seq
                    (map #(if (map? %) (dissoc % ::derivation) %))
                    (map #(let [serialized (serialize %)]
                            (if (= serialized :dag_unify.serialization/no-sharing)
                              [[[] (dissoc % :dag_unify.serialization/serialized)]]
                              serialized)))
                    (map vec))))
        (vals the-map))))

#?(:clj
   (defn write-compiled-lexicon [lexicon write-to-file]
     (spit write-to-file
           (serialized-value-map lexicon))))

(defn deserialize-lexicon [map-with-serializations]
  (zipmap
   (keys map-with-serializations)
   (map (fn [serializations]
          (vec (map dag_unify.serialization/deserialize
                    serializations)))
        (vals map-with-serializations))))

(defmacro read-compiled-lexicon [filename]
  `~(-> filename
        resource
        slurp
        read-string))
