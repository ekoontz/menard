(ns menard.lexiconfn
  (:require
   [menard.exception :refer [exception]]
   #?(:clj [clojure.java.io :as io :refer [resource]])
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [dag_unify.serialization :as s :refer [serialize]]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.diagnostics :as diag]
   [dag_unify.dissoc :as d]))

;; This is used to a convert human-friendly lexicon
;; into a machine-friendly data structure.

;; TODO: consider merging contents of this into morphology.cljc and remove this namespace.

;; add debugging information.
;; TODO: use a general-purpose 'debug' flag
;; and set this to true if that flag is on.
(def ^:dynamic include-derivation? true)

(defn apply-rule [rule lexeme consequent antecedent rule-group]
  (let [result (unify lexeme consequent)]
    (log/debug (str "apply-rule: lexeme: " lexeme "; consequent: " consequent "; antecedent:" antecedent
                    "; result: " result))
    (cond (= :fail result)
          (let [error-message (str "rule: " rule " matched antecedent: " antecedent
                                   ", but failed to unify lexeme:" (vec (s/serialize lexeme))
                                   " and consequent: " (vec (s/serialize consequent))
                                   "; fail-path: "
                                   (diag/fail-path lexeme consequent))]
            (log/error error-message)
            (exception error-message))
          true
          (do (log/debug (str "apply-rule: lexeme: " lexeme " with conseq: " consequent "= " result))
              (log/debug (str "include-derivation? set to: " include-derivation?))
              [(if (and include-derivation? rule)
                 (unify result
                        {:derivation {rule-group {rule {:match? true}}}})
                 result)]))))

(defn apply-rules [rules lexeme if-no-rules-matched? rule-group]
  (let [with-rules
         (->> rules
              (filter #(let [{antecedent :if
                              consequents :then} %]
                          (not (= :fail (unify antecedent lexeme)))))
              (mapcat #(let [{rule :rule
                              antecedent :if
                              consequents :then} %]
                         (log/debug (str "processing lexeme: " (u/get-in lexeme [:canonical])
                                        "; rule:" rule))
                         (mapcat (fn [consequent]
                                    (apply-rule rule lexeme consequent antecedent rule-group))
                                consequents))))]
    (cond (not (empty? with-rules))
          with-rules

          (and if-no-rules-matched? include-derivation?)
          [(unify lexeme {:derivation {rule-group {::no-rules-matched? true}}})]
          true
          [lexeme])))

(defn apply-rules-to-lexicon [lexicon rules if-no-rules-matched? rule-group]
  (into {}
        (for [k (sort (keys lexicon))]
          (let [lexemes (get lexicon k)]
            (log/debug (str "applying rules for: " k))
            (if (not (empty? lexemes))
              [k (->> lexemes
                      (mapcat (fn [lexeme]
                                (apply-rules rules lexeme if-no-rules-matched? rule-group)))
                      (mapcat (fn [lexeme]
                                [(unify lexeme
                                        {:phrasal false
                                         :canonical (u/get-in lexeme [:canonical] k)})])))])))))

(defn apply-to-every-lexeme [lexicon map-fn]
  (into {}
        (for [[k lexemes-for-k] lexicon]
          [k
           (map map-fn lexemes-for-k)])))

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

(defn apply-rules-in-order [lexicon rules rule-group]
  (if (empty? rules)
    lexicon
    (-> lexicon
        (apply-rules-in-order (rest rules) rule-group)
        (apply-rules-to-lexicon [(first rules)] false rule-group))))

(def ^:dynamic lexicon)
(def ^:dynamic morphology)

(defn matching-lexemes
  "given a surface form _surface_, find all matching lexical entries."
  [surface]
  (let [;; Apply morphological rules against surface to find a set of hypotheses
        ;; about the surface form. Each morphological rule has a :p key,
        ;; which we used to turn the surface form in to the canonical form.
        ;; We then use the :u key, also in the rule, to find the agreement and infl
        ;; specificities of this inflected form.
        from-inflected
        (->> morphology
             (map (fn [rule]
                    (let [{u :u [from to] :p} rule]
                      (if (re-find from surface)
                        {:canonical (clojure.string/replace surface from to)
                         :u u}))))
             (filter #(not (nil? %)))

             ((fn [rules]
                (log/debug (str "found: " (count rules) " matching rules."))
                rules))

             ;; Now we have a set of tuples T, each member of which has form: {:u U, :canonical C, :p P}, and
             ;; each of which is a guess about the word, where:
             ;; - C is the canonical form: the base, normalized version of the surface form _surface_.
             ;; - U is what must unify with the lexical forms found, for this guess to be valid.
             ;;
             ;; Furthermore, define L to be the lexemes in the lexicon which have the same canonical form (i.e. have {:canonical C} for some member of the set of tuples T.
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

             ((fn [lexemes]
                (log/debug (str "found: " (count lexemes) " inflections."))
                lexemes))
             
             (map (fn [lexeme]
                    (log/debug (str "  " surface " -> " (u/get-in lexeme [:canonical])))
                    lexeme))

             ;; Finally, we add {:surface surface} to the output:
             (map (fn [lexeme]
                    (unify lexeme
                           {:surface surface})))

             (filter #(not (= :fail %))))

        debug (log/debug (str "found: " (count from-inflected) " inflected form"
                             (if (not (= (count from-inflected) 1))
                               "s")
                             " before looking"
                             " at exceptions."))
        ;; Some (or even all) of the hypotheses in _from-inflected_ might be wrong, if the verb has
        ;; any exceptions. Below, the exceptional surface forms are used to  cancel these potential overgeneralizations.
        ;; For example, applying the rules for regular verbs in English, for infl present and agr 3rd sing,
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
                    (if (not (empty? (:exceptions analyze-hypothesis)))
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
      (if (and (not (empty? from-regular-morphology))
               (not (empty? exceptions)))
        (log/debug (str "(matching-lexemes '" surface "'): both regular inflections (" (count from-regular-morphology) ") and exceptions (" (count exceptions) ").")))
      (log/debug (str "found: " (count from-regular-morphology) " regular analyzed form"
                     (if (not (= (count from-regular-morphology) 1))
                       "s")
                     " for surface form: " surface "."))
      (log/debug (str "found: " (count exceptions) " exception"
                     (if (not (= count exceptions 1))
                       "s")
                     " for surface form: " surface "."))
      (let [result
            (concat
             from-regular-morphology
             exceptions)]
        (log/debug (str "returning: " (count result) " analyses for: " surface "."))
        result))))

(defn exceptions
  "generate exceptional lexical entries given a _canonical_ surface form and an input lexeme"
  [canonical lexeme]
  (map (fn [exception]
         (let [u-result
               (reduce unify
                       [(d/dissoc-in lexeme [:exceptions])
                        exception
                        {:exception true
                         :inflected? true
                         :canonical canonical}])
               result
               (if (not (= :fail u-result))
                 {(:surface exception)
                  [u-result]})]
           result))
       (:exceptions lexeme)))

(defn merge-with-all
  "having some personal cognitive difficulty in using apply with merge-with,
   so instead using this function as a workaround."
  [merge-with-fn args]
  (if (not (empty? args))
    (merge-with merge-with-fn
                (first args)
                (merge-with-all merge-with-fn (rest args)))))

(defn exceptions-for
  "generate all the exceptions possible for the sequence _lexemes_, each of which 
   has _canonical_ as the canonical form for the exception."
 [canonical lexemes]
 (->> lexemes
      (mapcat (fn [lexeme]
                (exceptions canonical lexeme)))
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
                    (map #(if (map? %) (dissoc % :derivation) %))
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

