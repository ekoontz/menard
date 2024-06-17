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

(defn display-derivation [deriv]
  (->> (seq (zipmap (vals deriv) (keys deriv)))
       (map (fn [[x y]]
              {(get x :menard.lexiconfn/order) y}))
       (sort (fn [x y]
               (< (first (first x)) (first (first y)))))))

(defn add-derivation [rule-name consequent i]
  {::derivation {rule-name (merge
                            (if (u/get-in consequent [:derivation :sense])
                              {:sense (u/get-in consequent [:derivation :sense])}
                              {})
                            {:applied? (if consequent true false)
                             ::order i})}})

(defn encode-derivation [derivation]
  (string/join " "
               (sort (into {}
                           (map (fn [[k v]]
                                  (if (get v :applied?)
                                    [(get v (keyword (str "menard.lexiconfn/order")))
                                     (if (get v :sense) [k (get v :sense)] k)])) derivation)))))

(defn eval-surface-fns [consequent lexeme]
  (if (seq (:exceptions consequent))
    (let [retval
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
                                   (:exceptions consequent))})]
      retval)
    consequent))
  
(defn apply-rule-to-lexeme [rule-name lexeme consequent antecedent i include-derivation?]
  (let [consequent (eval-surface-fns consequent lexeme)
        result (unify lexeme consequent)]
    (log/debug (str "apply-rule-to-lexeme: "
                    ;;"lexeme: " (u/pprint lexeme)
                    "; consequent: " (u/pprint consequent)
                    "; antecedent:" (u/pprint antecedent)
                    "; include-derivation? " include-derivation?
                    "; result: " (u/pprint result)))
    (cond (= :fail result)
          (let [fail-path (diag/fail-path lexeme consequent)
                error-message (str "rule: " rule-name " failed to unify lexeme: "
                                   (u/get-in lexeme [:canonical]) " with derivation: "
                                   (display-derivation (u/get-in lexeme [::derivation]))
                                   "; fail-path was: " fail-path ";"
                                   " lexeme's value for path: " (u/get-in lexeme fail-path) ";"
                                   " consequent's value for path: " (u/get-in consequent fail-path))]
            (log/error error-message)
            (exception error-message))
          :else
          (do (log/debug (str "apply-rule-to-lexeme: lexeme: " lexeme " with conseq: " consequent "= " result))
              (if include-derivation?
                (unify (dissoc result :derivation)
                       (add-derivation rule-name consequent i))
                ;; else, remove :derivation to save memory and runtime
                ;; processing:
                (dissoc result :derivation))))))

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
          antecedent (:if rule)]
      (if (not (= :fail (unify antecedent lexeme)))
        (->> (:then rule)
             (map (fn [consequent]
                    (apply-rule-to-lexeme (:rule rule) lexeme consequent antecedent i include-derivation?)))
             (mapcat (fn [new-lexeme]
                       (apply-rules-to-lexeme (rest rules) new-lexeme (+ i 1) include-derivation?))))
        ;; else
        (apply-rules-to-lexeme (rest rules)
                               (let [lexeme (if include-derivation? 
                                              (unify lexeme (add-derivation (:rule rule) nil i))
                                              lexeme)]
                                 lexeme)
                               (+ i 1) include-derivation?)))
    :else
    [lexeme]))

(defn apply-rules-to-lexicon [lexicon rules include-derivation?]
  (into {}
        (for [k (sort (keys lexicon))]
          (let [lexemes (seq (get lexicon k))
                include-derivation? include-derivation?]
            (log/debug (str "applying rules for: " k))
            (when lexemes ;; keys whose value is nil or null are removed.
              [k (->> lexemes
                      (map (fn [lexeme]
                             (unify lexeme
                                    {:phrasal? false
                                     :canonical (u/get-in lexeme [:canonical] k)})))
                      (mapcat (fn [lexeme]
                                (apply-rules-to-lexeme rules lexeme 0 include-derivation?))))])))))

(defn apply-rules-in-order [lexicon rules include-derivation?]
  (let [include-derivation? include-derivation?]
    (if (empty? rules)
      lexicon
      (-> lexicon
          (apply-rules-to-lexicon rules include-derivation?)))))

(defn apply-to-every-lexeme [lexicon map-fn]
  (log/debug (str "apply-to-every-lexeme."))
  (if (not (map? lexicon))
    (exception (str "input lexeme is not a map; it is: " (vec lexicon))))
  (let [result 
        (into {}
              (for [[k lexemes-for-k] lexicon]
                [k
                 (do
                   (log/debug (str "K: " k))
                   (log/debug (str "V: " (vec lexemes-for-k)))
                   (log/debug (str "RESULTS: " (vec (map map-fn lexemes-for-k))))
                   (map map-fn lexemes-for-k))]))]
    result))
    
#?(:clj
   (defn read-and-eval [rules-filename]
     (log/debug (str "read-and-eval with rules-filename: " rules-filename))
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
  (log/info (str "matching-lexemes for surface: '" surface "'"))
  (let [;; Apply morphological rules against surface to find a set of hypotheses
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
                        (log/info (str "FOUND with from: " from))
                        {:canonical (string/replace surface from to)
                         :u u}))))
             (filter #(not (nil? %)))

             ((fn [rules]
                (log/info (str "found: " (count rules) " matching rules."))
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
                (log/info (str "found: " (count lexemes) " inflections for surface: " surface  "."))
                lexemes))

             (map (fn [lexeme]
                    (log/info (str "  " surface " -> "
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
                    (log/debug (str "filtering with: " filter-with))
                    (when (seq (:exceptions analyze-hypothesis))
                      (log/debug (str " count of exceptions found for this guess: " (count (:exceptions analyze-hypothesis)))))
                    (empty? (filter #(not (= :fail (unify % filter-with)))
                                    (:exceptions analyze-hypothesis)))))
                from-inflected)

        debug (log/debug (str "this many inflected forms after filtering against exceptions: "
                              (count filter-against-exceptions)))

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
          exceptions (filter #(or (= true (:exception %))
                                  (= true (:inflected? %)))
                             (get lexicon surface))]
      (when (and (seq from-regular-morphology)
                 (seq exceptions))
        (log/debug (str "(matching-lexemes '" surface "'): both regular inflections (" (count from-regular-morphology) ") and exceptions (" (count exceptions) ").")))
      (log/debug (str "found: " (count from-regular-morphology) " regular analyzed form"
                      (when (not (= (count from-regular-morphology) 1))
                        "s")
                      " for surface form: " surface "."))
      (log/debug (str "found: " (count exceptions) " exception"
                      (when (not (= count exceptions 1))
                        "s")
                      " for surface form: " surface "."))
      (let [result
            (concat
             from-regular-morphology
             exceptions)]
        (log/debug (str "returning: " (count result) " analyses for: " surface "."))
        result))))

(defn merge-with-all
  "having some personal cognitive difficulty in using apply with merge-with,
   so instead using this function as a workaround."
  [merge-with-fn args]
  (when (seq args)
    (merge-with merge-with-fn
                (first args)
                (merge-with-all merge-with-fn (rest args)))))

(defn exceptions-for
  "generate all the exceptions possible for the sequence _lexemes_, each of which 
   has _canonical_ as the canonical form for the exception."
 [canonical lexemes]
 (->> lexemes
      (mapcat (fn [lexeme]
                (map (fn [exception]
                       (let [u-result
                             (reduce unify
                                     [(d/dissoc-in lexeme [:exceptions])
                                      exception
                                      {:exception? true
                                       :inflected? true
                                       :canonical canonical}])
                             result
                             (when (not (= :fail u-result))
                               {(:surface exception)
                                [u-result]})]
                         result))
                     (:exceptions lexeme))))
      (merge-with-all concat)))

(defn add-exceptions-to-lexicon
  "augment existing lexicon with new entries for all the exceptions possible for the input lexicon."
  [lexicon]
  (merge-with-all
   concat
   (cons lexicon
         (map (fn [canonical]
                (exceptions-for canonical (get lexicon canonical)))
              (keys lexicon)))))

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
