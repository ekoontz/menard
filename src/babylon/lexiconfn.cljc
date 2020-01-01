(ns babylon.lexiconfn
  (:require
   [babylon.exception :refer [exception]]
   #?(:clj [clojure.java.io :as io])
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [dag_unify.serialization :as s :refer [serialize]]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.dissoc :as d]))

;; These functions are used to a convert human-friendly lexicon
;; into a machine-friendly data structure.

(defn apply-rule [rule lexeme consequent antecedent rule-group]
  (let [result (unify lexeme consequent)]
    (log/debug (str "apply-rule: lexeme: " lexeme "; consequent: " consequent "; antecedent:" antecedent
                    "; result: " result))
    (cond (= :fail result)
          (let [error-message (str "rule: " rule " matched antecedent: " antecedent
                                   ", but failed to unify lexeme:" (vec (s/serialize lexeme))
                                   " and consequent: " (vec (s/serialize consequent))
                                   "; fail-path: "
                                   (u/fail-path lexeme consequent))]
            (log/error error-message)
            (throw (Exception. error-message)))
          true
          (do (log/debug (str "apply-rule: lexeme: " lexeme " with conseq: " consequent "= " result))
              [(unify result
                      (if rule
                        {:derivation {rule-group {rule {:match? true}}}}
                        :top))]))))

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

          if-no-rules-matched?
          [(unify lexeme {:derivation {rule-group {::no-rules-matched? true}}})]
          true
          [lexeme])))

(defn apply-rules-to-lexicon [lexicon rules if-no-rules-matched? rule-group]
  (into {}
        (for [[k lexemes] lexicon]
          (if (not (empty? lexemes))
            [k (->> lexemes
                    (mapcat (fn [lexeme]
                               (apply-rules rules lexeme if-no-rules-matched? rule-group)))
                    (mapcat (fn [lexeme]
                               [(unify lexeme
                                       {:phrasal false
                                        :canonical (u/get-in lexeme [:canonical] k)})])))]))))

#?(:clj
   (defn read-rules [rules-filename]
     (-> rules-filename
         io/resource
         slurp
         read-string
         ((fn [rule]
           (eval rule))))))

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
  (let [;; Apply morpologic rules against surface to find a set of hypotheses
        ;; about the surface form. Each morphological rule has a :p key,
        ;; which we used to turn the surface form in to the canonical form.
        ;; We then use the :u key, also in the rule, to find the agreement and infl
        ;; specificities of this inflected form.
        from-inflected
        (let [canonical-forms
              (filter #(not (nil? %))
                      (map (fn [rule]
                             (let [{u :u [from to] :p} rule]
                               (if (re-find from surface)
                                 {:canonical (clojure.string/replace surface from to)
                                  :u u})))
                           morphology))]
          (mapcat (fn [canonical-form]
                    (filter (fn [lexeme]
                              (if (not (= :fail lexeme))
                                (log/debug (str "matching lexeme: " (u/strip-refs lexeme))))
                              (not (= :fail lexeme)))
                            (map (fn [lexeme]
                                   (unify (:u canonical-form)
                                          {:inflected? false}
                                          {:surface surface
                                           :canonical (:canonical canonical-form)}
                                          lexeme))
                                 (get lexicon (:canonical canonical-form)))))
                  canonical-forms))

        debug (log/debug (str "found: " (count from-inflected) " inflected form"
                             (if (not (= (count from-inflected) 1))
                               "s")
                             " before looking"
                             " at exceptions."))
        ;; however, some (or even all) of the hypotheses might be wrong, if the verb has
        ;; any exceptions. Then, the exceptional surface forms should pre-empt and exclude these hypotheses.
        ;; For example, applying the rules for regular verbs in English, for infl present and agr 3rd sing,
        ;; the singular form of "be" is "bes", but there is an exceptional form "is" that should
        ;; be used instead. So this filter removes the spurious "bes" from the hypotheses generated
        ;; from _from_inflected_ above.
        filter-against-exceptions
        (filter (fn [analyze-hypothesis]
                  (log/debug (str "inflection guess: " analyze-hypothesis))
                  (let [filter-with
                        {:infl (u/get-in analyze-hypothesis [:infl])
                         :agr (u/get-in analyze-hypothesis [:agr])}]
                    (log/debug (str "filtering with: " filter-with))
                    (log/debug (str "unify map: " (vec (map #(unify % filter-with)
                                                           (:exceptions analyze-hypothesis)))))
                    (empty? (filter #(not (= :fail (unify % filter-with)))
                                    (:exceptions analyze-hypothesis)))))
                from-inflected)]
    (let [from-regular-morphology
          (vec (set filter-against-exceptions))
          ;; the lexicon contains both canonical forms and exceptions.
          ;; this complicated filter below is supposed to enforce the following
          ;; contraint:
          ;; 1. Get all forms where there is an inflection resulting in _surface_.
          ;; 2.a. If there are no inflected forms (i.e. 1. is empty), then return any forms from the lexicon
          ;;      that are the same as the input. This is for words that aren't inflected, for example
          ;;      determiners, prepositions, pronouns, etc; in general, closed-class lexemes.
          ;; 2.b. If there are inflected forms, return any forms from the lexicom where the canonical form
          ;;      of the verb is different from the input. This is for the
          ;;      rest of words, (i.e. open-class lexemes).
          exceptions (filter #(or (= true (:exception %))
                                  (= true (:inflected? %)))
                             (get lexicon surface))]
      (if (and (not (empty? from-regular-morphology))
               (not (empty? exceptions)))
        (log/warn (str "(matching-lexemes '" surface "'): both regular inflections (" (count from-regular-morphology) ") and exceptions (" (count exceptions) ").")))
      (log/debug (str "found: " (count from-regular-morphology) " analyzed form"
                     (if (not (= (count from-regular-morphology) 1))
                       "s")
                     "."))
      (log/debug (str "found: " (count exceptions) " exception"
                     (if (not (= count exceptions 1))
                         "s")
                     "."))
      (concat
       from-regular-morphology
       exceptions))))

(defn exceptions
  "generate exceptional lexical entries given a _canonical_ surface form and an input lexeme"
  [canonical lexeme]
  (map (fn [exception]
         (let [u-result
               (unify (d/dissoc-in lexeme [:exceptions])
                      exception
                      {:exception true
                       :canonical canonical})
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
          (vec (map serialize value-is-a-seq)))
        (vals the-map))))

(defn write-compiled-lexicon [lexicon write-to-file]
  (spit write-to-file
        (serialized-value-map lexicon)))

