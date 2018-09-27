(ns babel.italiano.morphology
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [babel.pos :refer (noun)]
   [babel.italiano.morphology.adjectives :as adjectives]
   [babel.italiano.morphology.determiners :as determiners]
   [babel.italiano.morphology.misc :as misc]
   [babel.italiano.morphology.nouns :as nouns]
   [babel.italiano.morphology.verbs :as verbs]
   [babel.morphology :as language-independent]
   [babel.stringutils :refer (replace-from-list)]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :as u :refer (copy dissoc-paths fail? get-in ref? strip-refs unify)]))

;; analysis-patterns are declarative data that determine how analysis (inflected form ->root form)
;; and conjugation (root form -> inflected form) are performed.
;; TODO: rename to simply 'patterns' since they are used in both directions,
;; not just analysis.
(def analysis-patterns
  (concat
   (-> (str "babel/italiano/morphology/adjectives.edn")
       clojure.java.io/resource
       slurp
       read-string)
   (-> (str "babel/italiano/morphology/determiners.edn")
       clojure.java.io/resource
       slurp
       read-string)
   (-> (str "babel/italiano/morphology/elisions.edn")
       clojure.java.io/resource
       slurp
       read-string)
   (-> (str "babel/italiano/morphology/nouns.edn")
       clojure.java.io/resource
       slurp
       read-string)
   verbs/patterns))

(declare analyze-one-pattern)

(defn analyze-regular [surface-form lexicon]
  (language-independent/analyze surface-form lexicon analysis-patterns))

(def tokenizer #"[ '\n,’».]")

(defn analyze-tokens
  "given a string, generate a list of tokenization hypotheses."
  [string]
  [(string/split string tokenizer)])

(def identity-pattern
  {:agr :top
   :p [#"(.*)" "$1"]})

(defn analyze-regular [surface lexicon]
  (->>
   (cons identity-pattern
         babel.italiano.morphology/analysis-patterns)
   (mapcat #(analyze-one-pattern surface % lexicon))))

(defn analyze-one-pattern
  "do regular (i.e. non-exceptional) morphological analysis to
  determine lexical information for a conjugated surface-form, using
  the (defonce analysis-patterns) defined above."
  [surface pattern lexicon]
  (mapcat (fn [[from to]] 
            (if (re-matches from surface)
              (->> (string/replace surface from to) ;; get root form..
                   (get lexicon) ;; look up root form in lexicon..
                   (map (fn [entry] ;; for each lexical entry, unify against the pattern's :u..
                          (unify (:u pattern :top)
                                 {:synsem {:agr (:agr pattern :top)}}
                                 entry)))
                   (filter #(not (= :fail %)))))) ;; filter out fails.
          (babel.morphology/group-by-two (:p pattern))))

(declare analyze-capitalization-variant)

(defn analyze
  "take the union of: 
      - analyzing _surface-form_ according to the (defonce analysis-patterns) above
      - looking up _surface-form_ in the supplied lexicon."
  [surface-form lexicon]
  (mapcat (fn [each-variant]
            (analyze-capitalization-variant each-variant lexicon))
          (set
           (list
            surface-form
            (string/capitalize surface-form)
            (string/capitalize (string/lower-case surface-form))
            (string/upper-case surface-form)
            (string/lower-case surface-form)
            (string/join " " (map #(if (not (= "e" %))
                                     (string/capitalize %)
                                     %)
                                  (string/split surface-form #"[ ]")))))))

(defn analyze-capitalization-variant [surface-form lexicon]
  "return an array of the maps, each of which represents the lexical information about a surface form."
  (concat
   (analyze-regular surface-form lexicon)

   ;; make canonical input forms fully inflected:
   (map (fn [lexeme]
          (cond (and (= :verb (get-in lexeme [:synsem :cat]))
                     (= :top (get-in lexeme [:synsem :infl])))
                ;; if a verb has no infl, it's :infinitive.
                (unify lexeme
                       {:synsem {:infl :infinitive}})

                (and (= :noun (get-in lexeme [:synsem :cat]))
                     (= :top (get-in lexeme [:synsem :agr :number])))
                ;; if a noun has no number, it's singular.
                (unify lexeme
                       {:synsem {:agr {:number :sing}}})
                true
                lexeme))
        (get lexicon surface-form))))

(def exceptions-rules
  (concat verbs/exceptions-rules
          adjectives/exceptions-rules
          nouns/exceptions-rules))

;; TODO: move this to babel.italiano.lexicon, since it is part of lexicon compilation
(defn exception-generator [lexicon]
  (->>
   (sort (keys lexicon))
   (mapcat (fn [k]
             (let [lexemes (get lexicon k)
                   lexeme-kv [k lexemes]]
               (->> exceptions-rules
                    (mapcat (fn [{path :path
                                  label :label
                                  surface-form :surface-form
                                  merge-fn :merge-fn}]
                              (let [surface-form-fn (or surface-form
                                                        (fn [lexeme]
                                                          (get-in lexeme path :none)))]
                                ;; a lexeme-kv is a pair of a key and value. The key is a string (the word's surface form)
                                ;; and the value is a list of lexemes for that string.
                                (->> lexemes
                                     (mapcat (fn [lexeme]
                                               (if (not (= :none (get-in lexeme path :none)))
                                                 (do (log/debug (str (first lexeme-kv) " generating lexeme exceptional surface form: " (surface-form-fn lexeme)))
                                                     (list {(surface-form-fn lexeme)
                                                            [(reduce
                                                              (fn [a b]
                                                                (cond
                                                                  (or (= a :fail)
                                                                      (= b :fail))
                                                                  :fail
                                                                  true
                                                                  (unify a b)))
                                                              [(dissoc-paths lexeme [[:italiano :italiano]])
                                                               (merge-fn lexeme)
                                                               {:italiano {:infinitive k
                                                                           :exception true}}])]})))))))))))))))
(defn phonize2 [lexicon]
  (into {}
        (for [[k vals] lexicon]
          [k 
           (map (fn [v]
                  (unify v
                         {:italiano {:italiano k}}))
                vals)])))

(def rules
  (reduce concat
          [

           (->> (-> (str "babel/italiano/morphology/nouns.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule)
                            :cat :noun
                            :pronoun false
                            :propernoun false}})))

           (->> (-> (str "babel/italiano/morphology/verbs/conditional.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :conditional}})))

           (->> (-> (str "babel/italiano/morphology/verbs/future.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :future}})))

           (->> (-> (str "babel/italiano/morphology/verbs/gerund.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :gerund}})))

           (->> (-> (str "babel/italiano/morphology/verbs/imperfetto.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :imperfetto}})))

           (->> (-> (str "babel/italiano/morphology/verbs/passato.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule :top)
                            :essere (u/get-in rule [:u :synsem :essere] false)
                            :cat :verb
                            :infl :passato}})))

           (->> (-> (str "babel/italiano/morphology/verbs/present.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :boot-verb (:boot-verb rule false)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :present}})))

           (->> (-> (str "babel/italiano/morphology/verbs/subjunctive.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :boot-verb (:boot-verb rule false)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :subjunctive}})))]))

(defn find-matching-pair [input from-to-pairs]
  (if (not (empty? from-to-pairs))
    (let [[pattern-from pattern-to] from-to-pairs]
      (if (re-matches pattern-from input)
        (cons
         (string/replace input pattern-from pattern-to)
         (find-matching-pair input (rest (rest from-to-pairs))))
        (find-matching-pair input (rest (rest from-to-pairs)))))))

(declare irregular-conditional?)
(declare irregular-conditional)
(declare irregular-future?)
(declare irregular-future)
(declare irregular-gerund?)
(declare irregular-gerund)
(declare irregular-passato?)
(declare irregular-passato)
(declare irregular-present?)
(declare irregular-imperfetto?)
(declare irregular)

(def elision-regexps
  (-> (str "babel/italiano/morphology/elisions.edn")
      clojure.java.io/resource
      slurp
      read-string))

(def generative-elision-regexps
  (mapcat :g elision-regexps))

(defn elisions
  "transform string by doing elisions where needed (e.g. 'a il' -> 'al')"
  [input regex-pairs]
  (if (empty? regex-pairs)
    input
    (elisions
     (let [[pattern-from pattern-to] regex-pairs]
       (if (re-matches pattern-from input)
         (string/replace input pattern-from pattern-to)
         input))
     (rest (rest regex-pairs)))))

(defn elisions-to-fixed-point [input]
  (let [round-one (string/trim (elisions input generative-elision-regexps))]
    (if (= round-one (string/trim input))
      round-one
      (elisions-to-fixed-point round-one))))

(defn morph [structure]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure
        (u/get-in structure [:synsem]) (morph (u/get-in structure [:italiano]))
        
        (and (not (nil? (u/get-in structure [:a])))
             (not (nil? (u/get-in structure [:b]))))
        (->
         (->> 
          [(u/get-in structure [:a])
           (u/get-in structure [:b])]
          (map morph)
          (string/join " "))
         string/trim
         elisions-to-fixed-point)

        (nil? (u/get-in structure [:italiano])) "<empty>"
        
        (and (irregular-conditional? structure)
             (not (= :use-regular (irregular structure :conditional))))
        (irregular structure :conditional)
        
        (and (irregular-future? structure)
             (not (= :use-regular (irregular structure :future))))
        (irregular structure :future)
        
        (irregular-gerund? structure)
        (irregular-gerund structure)
        
        (irregular-passato? structure)
        (irregular-passato structure)
        
        (and (irregular-present? structure)
             (not (= :use-regular (irregular structure :present))))
        (irregular structure :present)
        
        (and (irregular-imperfetto? structure)
             (not (= :use-regular (irregular structure :imperfetto))))
        (irregular structure :imperfetto)
        
        true
        (let [path-to-root
              (cond
                (and
                 (not (nil? (u/get-in structure [:future-stem])))
                 (or (not (= :fail
                             (unify structure
                                    {:cat :verb
                                     :infl :future})))
                     (not (= :fail
                             (unify structure
                                    {:cat :verb
                                     :infl :conditional})))))
                [:future-stem]
                true
                [:italiano])
              regexps
              (concat
               (mapcat :g
                       (filter #(not (= % :fail))
                               (map
                                #(unify %
                                        {:boot-verb (u/get-in structure [:boot-verb] false)
                                         :u structure})
                                rules)))
               [#"(.*)" "$1"])]
          (first (find-matching-pair (u/get-in structure path-to-root)
                                     regexps)))))

(defn irregular-conditional? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :conditional (u/get-in structure [:infl]))
   (map? (u/get-in structure [:conditional]))))

(defn irregular-future? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :future (u/get-in structure [:infl]))
   (map? (u/get-in structure [:future]))))

(defn irregular-present? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :present (u/get-in structure [:infl]))
   (map? (u/get-in structure [:present]))))

(defn irregular-imperfetto? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :imperfetto (u/get-in structure [:infl]))
   (map? (u/get-in structure [:imperfetto]))))

(defn irregular-gerund? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :gerund (u/get-in structure [:infl]))
   (string? (u/get-in structure [:gerund]))))

(defn irregular-gerund [structure]
  (u/get-in structure [:imperfetto]))

(defn irregular-passato? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :passato (u/get-in structure [:infl]))
   (string? (u/get-in structure [:passato]))))

(def essere-passato-regexps
  (-> (str "babel/italiano/morphology/verbs/essere-passato.edn")
      clojure.java.io/resource
      slurp
      read-string))

(defn irregular-passato [structure]
  (first
   (find-matching-pair
    (u/get-in structure [:passato])
    (->>
     essere-passato-regexps
     (filter #(not (= :fail
                      (u/unify structure
                               {:agr (u/get-in % [:agr] :top)
                                :essere (u/get-in % [:u :synsem :essere] :top)}))))
     (mapcat :g)))))

(defn irregular-gerund [structure]
  (u/get-in structure [:gerund]))

(defn irregular [structure infl]
  (let [arg (u/get-in structure [:agr])
        irreg (u/get-in structure [infl])]
    (cond
      (and (not (= :fail (unify arg
                                {:person :1st
                                 :number :sing})))
           (u/get-in irreg [:1sing]))
      (u/get-in irreg [:1sing])

      (and (not (= :fail (unify arg
                                {:person :2nd
                                 :number :sing})))
           (u/get-in irreg [:2sing]))
      (u/get-in irreg [:2sing])
      
      (and (not (= :fail (unify arg
                                {:person :3rd
                                 :number :sing})))
           (u/get-in irreg [:3sing]))
      (u/get-in irreg [:3sing])

      (and (not (= :fail (unify arg
                                {:person :1st
                                 :number :plur})))
           (u/get-in irreg [:1plur]))
      (u/get-in irreg [:1plur])

      (and (not (= :fail (unify arg
                                {:person :2nd
                                 :number :plur})))
           (u/get-in irreg [:2plur]))
      (u/get-in irreg [:2plur])
      
      (and (not (= :fail (unify arg
                                {:person :3rd
                                 :number :plur})))
           (u/get-in irreg [:3plur]))
      (u/get-in irreg [:3plur])

      true :use-regular)))
