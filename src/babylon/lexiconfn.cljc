(ns babylon.lexiconfn
  (:require
   [babylon.exception :refer [exception]]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [dag_unify.serialization :as s :refer [serialize]]
   [dag_unify.core :as u :refer [unify]]
   [dag_unify.dissoc :as d]))

;; These functions are used to a convert human-friendly lexicon
;; into a machine-friendly data structure.

(defn apply-rule [rule lexeme consequent antecedent]
  (let [result (unify lexeme consequent)]
    (log/debug (str "apply-rule: lexeme: " lexeme "; consequent: " consequent "; antecedent:" antecedent
                   "; result: " result))
    (cond (= :fail result)
          (let [error-message (str "rule: " rule " matched antecedent: " antecedent
                                   ", but failed to unify lexeme:" (vec (s/serialize lexeme))
                                   " and consequent: " (vec (s/serialize consequent)))]
              (log/error error-message)
              (throw (Exception. error-message)))
          true
          (do (log/debug (str "apply-rule: lexeme: " lexeme " with conseq: " consequent "= " result))
              [(unify result
                        (if rule
                          {:rules-matched {rule true}}
                          :top))]))))

(defn apply-rules [rules lexeme if-no-rules-matched?]
  (let [with-rules
         (->> rules
              (filter #(let [{antecedent :if
                              consequents :then} %]
                          (not (= :fail (unify antecedent lexeme)))))
              (mapcat #(let [{rule :rule
                              antecedent :if
                              consequents :then} %]
                        (mapcat (fn [consequent]
                                    (apply-rule rule lexeme consequent antecedent))
                                consequents))))]
    (cond (not (empty? with-rules))
          with-rules
          if-no-rules-matched?
          [(unify lexeme {:rules-matched {::no-rules-matched? true}})]
          true
          [lexeme])))

(defn apply-rules-to-lexicon [lexicon rules if-no-rules-matched?]
  (into {}
        (for [[k lexemes] lexicon]
          (if (not (empty? lexemes))
            [k (->> lexemes
                    (mapcat (fn [lexeme]
                               (apply-rules rules lexeme if-no-rules-matched?)))
                    (mapcat (fn [lexeme]
                               [(unify lexeme
                                       {:phrasal false
                                        :canonical (u/get-in lexeme [:canonical] k)})])))]))))
(defn apply-one-rule [rule lexeme]
  (let [[antecedent consequent] rule]
    (cond
       (= :fail (unify lexeme antecedent))
       []
       (fn? consequent)
       ;; 1. _consequent_ can be a function that
       ;; takes a map and returns a sequence of maps.
       (let [result (consequent lexeme)]
         (filter #(not (u/isomorphic? lexeme %))
                 result))

       true
       ;; 2. ..or (the more frequent use case)
       ;; _consequent_ can be another map that
       ;; we unify against the lexeme.
       (let [result (unify lexeme consequent)]
         (cond
           (= :fail result)
           []
           (u/isomorphic? result lexeme)
           []
           true
           [result])))))

(defn read-rules [rules-filename]
  (-> rules-filename
      io/resource
      slurp
      read-string
      ((fn [rule]
        (eval rule)))))

(defn apply-rules-in-order [lexicon rules]
  (if (empty? rules)
    lexicon
    (-> lexicon
        (apply-rules-in-order (rest rules))
        (apply-rules-to-lexicon [(first rules)] false))))

(defn apply-rules-cyclicly [rules lexemes]
  (let [one-round
        (->> rules
             (mapcat
              (fn [rule]
                (let [[antecedent consequent] rule]
                  (mapcat (fn [lexeme]
                            (apply-one-rule rule lexeme))
                          lexemes)))))]
    (cond (empty? one-round)
          lexemes
          true
          (apply-rules-cyclicly rules one-round))))

(defn process [lexicon rules]
  (into {} (for [[canonical lexemes]
                 lexicon]
             [canonical
              (let [lexemes (if (vector? lexemes) lexemes (vec lexemes))]
                (->> lexemes
                     (map (fn [lexeme]
                            (merge lexeme {:phrasal false
                                           :canonical canonical})))
                     (apply-rules rules)))])))

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
                              (not (= :fail lexeme)))
                            (map (fn [lexeme]
                                   (unify (:u canonical-form)
                                          {:surface surface
                                           :canonical (:canonical canonical-form)}
                                          lexeme))
                                 (get lexicon (:canonical canonical-form)))))
                  canonical-forms))
        ;; however, some (or even all) of the hypotheses might be wrong, if there are
        ;; exceptional surface forms which preclude these hypotheses. For example,
        ;; applying the rules for regular verbs in English, for infl present and agr 3rd sing,
        ;; the singular form of "be" is "bes", but there is an exceptional form "is" that should
        ;; be used instead. So this filter removes the spurious "bes" from the hypotheses generated
        ;; from _from_inflected_ above.
        filter-against-exceptions
        (filter (fn [analyze-hypothesis]
                  (let [filter-with
                        {:infl (u/get-in analyze-hypothesis [:infl])
                         :agr (u/get-in analyze-hypothesis [:agr])}]
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
      (concat
       from-regular-morphology
        exceptions))))

(defn exceptions
  "generate exceptional lexical entries given a _canonical_ surface form and an input lexeme"
  [canonical lexeme]
  (map (fn [exception]
         {(:surface exception)
          [(unify (d/dissoc-in lexeme [:exceptions])
                  exception
                  {:exception true
                   :canonical canonical})]})
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
