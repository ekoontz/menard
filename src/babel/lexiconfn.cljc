(ns babel.lexiconfn ;; TODO consider renaming babel.lexiconfn to babel.lexicon.
  (:refer-clojure :exclude [exists? get-in resolve find])
  (:require
   [babel.encyclopedia :refer [sem-impl]]
   [babel.exception :refer [exception]]
   [babel.pos :refer [agreement-noun common-noun determiner
                      intransitive modal noun
                      subcat0 subcat1
                      transitive-but-object-cat-not-set
                      verb-subjective]]
   [clojure.set :as set]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.string :as string]
   [dag_unify.core :as unify :refer [create-path-in dissoc-paths exists?
                                     fail-path fail? get-in isomorphic?
                                     serialize strip-refs unify]]))

(declare listify)
(declare map-function-on-map-vals)
(declare rules)
(declare transform)

;; TODO: compile-lex should simply be a pipeline rather than an argument-position-sensitive function.
;; The current form is too complex because each argument has a slightly different signature.
;; Instead, it should be a pipeline where each argument is fn(lexicon) => lexicon (i.e. it takes a lexicon, 
;; and a lexicon is returned, where a lexicon is a map<string,vector>.
;; Or, perhaps more conveniently, fn(lexeme) => lexeme, where a lexeme is a vector of maps,
;; or fn(lexeme) => lexeme, where a lexeme is simply a map.
(defn compile-lex [lexicon-source exception-generator phonize-fn & [language-specific-rules]]
  (let [;; take source lexicon (declared above) and compile it.
        ;; 1. canonicalize all lexical entries
        ;; (i.e. vectorize the values of the map).
        lexicon-stage-1 (listify lexicon-source)

        remove-disable (map-function-on-map-vals
                        lexicon-stage-1
                        (fn [k v]
                          (remove #(= :fail %)
                                  (map (fn [lexeme]
                                         (if (= true (get-in lexeme [:disable]))
                                           :fail
                                           lexeme))
                                       v))))

        phon-lexicon (map-function-on-map-vals
                      remove-disable
                      (fn [lexical-string lexical-val]
                        (phonize-fn lexical-val lexical-string)))

        ;; 2. apply grammatical-category and semantic rules to each element in the lexicon
        lexicon-stage-2 (map-function-on-map-vals 
                         phon-lexicon
                         (fn [lexical-string lexeme]
                           (map (fn [lexeme]
                                  (transform lexeme rules))
                                lexeme)))

        ;; 3. apply language-specific grammatical rules to each element in the lexicon
        ;; for an example of a language-specific rule,
        ;; see italiano/morphology.clj:(defn agreement [lexical-entry]).
        lexicon-stage-3 (if language-specific-rules
                          (map-function-on-map-vals
                           lexicon-stage-2
                           (fn [lexical-string lexemes]
                             (map (fn [lexeme]
                                    (transform lexeme language-specific-rules))
                                  lexemes)))
                          ;; no language-specific rules: lexicon-stage-3 == lexicon-stage-2
                          lexicon-stage-2)

        ;; 4. generate exceptions
        ;; problem: merge is overwriting values: use a collator that accumulates values.
        exceptions (listify 
                    (let [tmp (map #(listify %)
                                   (exception-generator lexicon-stage-3))]
                      (if (empty? tmp)
                        nil
                        (reduce #(merge-with concat %1 %2)
                                tmp))))

        lexicon
        (merge-with concat lexicon-stage-3 exceptions)]
    lexicon))

(declare get-fail-path)

(defn encode-where-query [& where]
  "encode a query as a set of index queries."
  where)

(defn italian [lexeme]
  (get (nth lexeme 1) :lexicon))

(defn synsem [lexeme]
  (nth lexeme 1))

(defn english [lexeme]
  (get (nth lexeme 1) :english))

(def firstp
  {:person :1st})
(def secondp
  {:person :2nd})
(def thirdp
  {:person :3rd})
(def sing
  {:number :singular})
(def plural
  {:number :plural})
(def present
  {:cat :verb
   :infl :present})

;; TODO: move to morphology
(defn italian-pluralize [singular gender]
  (cond
   (= gender :masc)
   (string/replace #"([oe])$" "i" singular)
   (= gender :fem)
   (string/replace #"([a])$" "e" singular)))

;; TODO move to morphology
(defn english-pluralize [singular]
  (str (string/replace #"([sxz])$" "$1e" singular) "s"))

(defn listify
  "for every value in the given map, if the value is not a sequence or a vector, wrap it in a vector."
  [m]
  (into {}
        (for [[k v] m]
          [k (cond (map? v)
                   (vec (list v))
                   (seq? v)
                   (vec v)
                   true
                   v)])))

;; http://stackoverflow.com/questions/1676891/mapping-a-function-on-the-values-of-a-map-in-clojure
;; http://stackoverflow.com/a/1677927
(defn map-function-on-map-vals [m f]
  (if (not (map? m))
    (throw (exception (str "Expected map as first input to map-function-on-map-vals, but got an input of type: " (type m)))))
  ;; TODO: add check for uniformity of type of keys
  ;; i.e. check that they are either all strings, or all keywords, or all integers, etc.
  ;; this is to avoid the need to log/debug below.
  (into {} 
        (for [[k v]
              ;; if you need to trace this, probably your input data
              ;; is non-uniform: i.e. the map keys are not all one type
              ;; (e.g. a mixture of strings and keywords rather than
              ;;  purely one or the other)
              ;; your keys or values 
              (do (log/trace (str "sorting array: " m))
                  (sort m))]
          ;; for each <k,v> pair, return a <k,v'>, where v' = f(v).
          [k (f k v)])))

(defn check-lexicon [lexicon]
  (let [check-one (fn [k v]
                    (let [result (fail? v)]
                      (if result 
                        (log/warn (str "fail found for: " k)))
                      (if result
                        (list k))))]
    (mapcat
     #(let [key %
            val (get lexicon %)]
        (if (seq? val) 
          (mapcat (fn [x] 
                    (check-one key x))
                  val)
          (check-one key val)))
     (keys lexicon))))

;; TODO: move this and other functions here to (if-then) statements.
(defn ditransitive-verb-rule [lexical-entry]
  (cond (and (= (get-in lexical-entry [:synsem :cat]) :verb)
             (not (nil? (get-in lexical-entry '(:synsem :sem :iobj))))
             (not (= '() (get-in lexical-entry [:synsem :subcat :3]))))
        (unify
         lexical-entry
         (let [ref (atom :top)]
           {:synsem {:subcat {:3 {:sem ref}}
                     :sem {:iobj ref}}}))
        true
        lexical-entry))

(defn intensifier-agreement [lexical-entry]
  (cond (= (get-in lexical-entry '(:synsem :cat)) :intensifier)
        (unify
         (let [agr (atom :top)]
           {:synsem {:agr agr
                     :subcat {:1 {:agr agr}
                              :2 {:agr agr}}}})
         lexical-entry)

         true lexical-entry))

(defn pronoun-and-propernouns [lexical-entry]
  (cond (= true (get-in lexical-entry '(:synsem :pronoun)))
        (unify lexical-entry
                {:synsem {:propernoun false
                          :cat :noun
                          :subcat '()}})

        (= true (get-in lexical-entry '(:synsem :propernoun)))
        (unify lexical-entry
                {:synsem {:cat :noun
                          :pronoun false
                          :subcat '()}})
        true
        lexical-entry))

;; TODO: language-specific - does not belong here, or else keep here, and continue
;; to add new language like Espanol.
(defn embed-phon [lexical-entry]
  (cond (string? (get-in lexical-entry '(:english)))
        (unify {:english {:english (get-in lexical-entry '(:english))}}
                (embed-phon (dissoc lexical-entry ':english)))

        (and (string? (get-in lexical-entry '(:italiano)))
             (= :verb (get-in lexical-entry '(:synsem :cat))))
        (unify {:italiano {:italiano (get-in lexical-entry '(:italiano))}}
                (embed-phon (dissoc lexical-entry ':italiano)))

        (string? (get-in lexical-entry '(:italiano)))
        (unify {:italiano {:italiano (get-in lexical-entry '(:italiano))}}
                (embed-phon (dissoc lexical-entry ':italiano)))
        true
        lexical-entry))

(defn intransitive-verb-rule [lexical-entry]
  (cond (and (= (get-in lexical-entry '(:synsem :cat))
                :verb)
             (= :none (get-in lexical-entry '(:synsem :sem :obj) :none))
             (= :none (get-in lexical-entry '(:synsem :sem :location) :none))
             (= :none (get-in lexical-entry '(:synsem :subcat :2) :none))
             (not (= true (get-in lexical-entry '(:synsem :aux)))))
        (unify
         lexical-entry
         intransitive)
        true
        lexical-entry))

(defn modality-rule [lexical-entry]
  "prevent ratholes like 'Potere ... potere dormire (To be able...to be able to sleep)'"
  (cond (= true (get-in lexical-entry '(:synsem :modal)))
        (unify
         modal lexical-entry
         {:synsem {:subcat {:2 {:modal false}}}})

        (= :verb (get-in lexical-entry '(:synsem :cat)))
        {:synsem {:modal false}}
        true
        lexical-entry))

(defn noun-arguments-must-be-empty-subcat [lexical-entry]
  "noun-headed arguments of verbs must either be empty subcat (e.g. either a NP such as
    'the dog' in 'sees the dog' and not 'sees dog'), or a mass noun (e.g. 'milk', which will
    have an empty subcat."
  ;; TODO: mass noun part not implemented yet.
  (cond (and (= :verb (get-in lexical-entry '(:synsem :cat)))
             (= :noun (get-in lexical-entry '(:synsem :subcat :2 :cat))))
        (unify lexical-entry
                {:synsem {:subcat {:2 {:subcat '()}}}})

        true
        lexical-entry))

(defn transitive-verb-rule [lexical-entry]
  (cond (and (= (get-in lexical-entry [:synsem :cat]) :verb)
             (not (= false (get-in lexical-entry [:transitivize])))
             (not (nil? (get-in lexical-entry [:synsem :sem :obj])))
             (not (= (get-in lexical-entry [:synsem :sem :obj]) :unspec))

             ;; do not apply rule if (:subcat :2) is explicitly empty.
             (not (= '() (get-in lexical-entry [:synsem :subcat :2])))

             ;; do not apply rule if (:subcat :2) is not noun.
             (= :noun (get-in lexical-entry [:synsem :subcat :2 :cat] :noun))

             ;; do not apply rule if there is :3.
             (= :none (get-in lexical-entry [:synsem :subcat :3] :none)))

        (unify
         lexical-entry
         transitive-but-object-cat-not-set)
        true
        lexical-entry))

  ;; TODO: for here and other rules, allow compiler to emit error message if rule fails like "subject must look like: <verb-subjective>"
(defn verb-rule [lexical-entry]
  "every verb has at least a subject."
  (cond (and (= (get-in lexical-entry [:synsem :cat]) :verb)
             (not (= false (get-in lexical-entry [:intransitivize]))))
        (unify
         lexical-entry
         verb-subjective)
        true
        lexical-entry))

(defn commonnoun [lexical-entry]
  ;; subcat non-empty: pronoun is false
  (cond (and (= (get-in lexical-entry [:synsem :cat]) :noun)
             (or (not (empty? (get-in lexical-entry [:synsem :subcat])))
                 (= :unspecified (get-in lexical-entry [:synsem :subcat] :unspecified)))
             (not (= (get-in lexical-entry [:synsem :pronoun]) true))
             (not (= (get-in lexical-entry [:synsem :propernoun]) true)))
        (let [result 
              (unify lexical-entry
                      (unify agreement-noun
                              common-noun
                              {:synsem {:pronoun false
                                        :subcat {:1 {:cat :det}
                                                 :2 '()}}}))]
          (if (fail? result)
            (throw (exception (str "fail when trying to create common-noun from lexical-entry: " lexical-entry
                                   "subcat: " (get-in lexical-entry [:synsem :subcat])
                                   "subcat emptyness: " (empty? (get-in lexical-entry [:synsem :subcat])))))

            result))
        true
        lexical-entry))

(def do-semantic-implicature? true)

(defn semantic-implicature [lexical-entry]
  (if do-semantic-implicature?
    (cond
       (= (get-in lexical-entry [:synsem :cat]) :noun)
       {:synsem {:sem (sem-impl (get-in lexical-entry [:synsem :sem]))}}

       (and true (= (get-in lexical-entry [:synsem :cat]) :verb))
       {:synsem {:sem (reduce unify
                              (filter #(not (= :does-not-apply %))
                                      [:top ;; necessary to prevent cases where neither :subj nor :obj exists.
                                       (if (not (= :none (get-in lexical-entry [:synsem :sem :subj] :none)))
                                         {:subj (sem-impl (get-in lexical-entry [:synsem :sem :subj]))}
                                         :does-not-apply)
                                       (if (not (= :none (get-in lexical-entry [:synsem :sem :obj] :none)))
                                         {:obj (sem-impl (get-in lexical-entry [:synsem :sem :obj]))}
                                         :does-not-apply)]))}}

       ;; TODO: prepositions
       true
       lexical-entry)

    lexical-entry))

(defn put-a-bird-on-it [lexical-entry]
  "example lexical entry transformer."
  (cond (map? lexical-entry)
        (conj {:bird :thrush}
              lexical-entry)
        true
        lexical-entry))

(defn category-to-subcat [lexical-entry]
  (cond (and (or (= :det (get-in lexical-entry [:synsem :cat]))
                 (= :adverb (get-in lexical-entry [:synsem :cat])))

             ;; if subcat is defined, then go with that rather than trying to unify with subcat0
             (= :none (get-in lexical-entry [:synsem :subcat] :none)))
        (unify
         subcat0
         lexical-entry)

        (and (= (get-in lexical-entry '(:synsem :cat)) :adjective)
             (not (= (get-in lexical-entry '(:synsem :sem :comparative)) true)))
        (unify
         subcat1
         lexical-entry)

        (= (get-in lexical-entry '(:synsem :cat)) :sent-modifier)
        (unify
         {:synsem {:subcat {:1 {:cat :verb
                                :subcat '()}
                            :2 '()}}}
         lexical-entry)

        true
        lexical-entry))

(defn determiner-stuff [lexical-entry]
  (cond (= (get-in lexical-entry '(:synsem :cat)) :det)
        (unify determiner
                lexical-entry)
        true
        lexical-entry))

;; TODO: regenerate :serialized whenever creating a new lexical entry
(defn make-intransitive-variant [lexical-entry]
  (cond

   (and (= (get-in lexical-entry [:synsem :cat]) :verb)
        (exists? lexical-entry [:synsem :subcat :2])
        (not (empty? (get-in lexical-entry [:synsem :subcat :2]))))

   ;; create an intransitive version of this transitive verb by removing the second arg (:synsem :subcat :2), and replacing with nil.
   (list
    (unify (dissoc-paths lexical-entry (list [:synsem :subcat :2]
                                              [:serialized]))
            {:synsem {:subcat {:2 '()}}
             :canary :tweet43}) ;; if the canary tweets, then the runtime is getting updated correctly.

    lexical-entry) ;; the original transitive lexeme.

   true
   (list lexical-entry)))

;; Rules like make-intransitive-variant multiply a single lexeme into zero or more lexemes: 
;; In other words, their function signature is map => seq(map).
(defn apply-multi-rules [lexeme]
  (make-intransitive-variant lexeme))


;; This set of rules is monotonic and deterministic in the sense that
;; iterative application of the set of rules will result in the input
;; lexeme become more and more specific until it reaches a determinate
;; fixed point, no matter what order we apply the rules. Given enough
;; iterations, this same fixed point will be reached no matter which
;; order the rules are applied, as long as all rules are applied at
;; each iteration. This is guaranteed by using these rules below in
;; (transform) so that the rules' outputs are reduced using unify.
(def rules (list category-to-subcat
                 commonnoun
                 determiner-stuff
                 ditransitive-verb-rule
                 intensifier-agreement
                 modality-rule
                 noun-arguments-must-be-empty-subcat
                 pronoun-and-propernouns
                 semantic-implicature
                 transitive-verb-rule
                 verb-rule
))

;; Modifying rules: so-named because they modify the lexical entry in
;; such a way that is non-monotonic and dependent on the order of rule
;; application. Because of these complications, avoid and use
;; unifying-rules instead, where possible. Only to be used where
;; (reduce unify ..) would not work, as with embed-phon, where
;; {:italiano <string>} needs to be turned into {:italiano {:italiano <string>}},
;; but unifying the input and output of the rule would be :fail.
;; These rules are (reduce)d using merge rather than unify.
(def modifying-rules (list embed-phon))

;; TODO: allow transforming rules to emit sequences as well as just the
;; input value. i.e they should take a map and return either: a map, or a sequence of maps.
;; This means we have to check the type of the return value 'result' below.
(defn transform [lexical-entry rules]
  "keep transforming lexical entries until there's no changes. No changes is
   defined as: (isomorphic? input output) => true, where output is one iteration's
   applications of all of the rules."
  (cond (= lexical-entry :fail) :fail
        (fail? lexical-entry)
        (do (log/warn (str "lexical-entry " lexical-entry " was fail before applying any rules."))
            :fail)

        true
        (do
          (log/debug (str "transforming lexical entry: " lexical-entry))
          (let [result (reduce #(if (fail? %1)
                                  (let [message (str "lexical entry fail; entry:" (strip-refs %1) ";")]
                                    (log/error message)
                                    (throw (exception message)))
                                      
                                  (let [result (unify %1 %2)]
                                    (if (fail? result)
                                      (let [fail-path (get-fail-path %1 %2)
                                            message (str "lexical entry reduce fail:(fail-path=" fail-path "): "
                                                         {:f1 (strip-refs (get-in %1 fail-path))
                                                          :f2 (strip-refs (get-in %2 fail-path))})]
                                        (log/error message)
                                        (throw (exception message)))
                                      result)))
                               (map
                                (fn [rule]
                                  ;; check for return value of (apply rule (list lexical-entry)):
                                  ;; if not list, make it a list.
                                  (let [debug (log/debug (str "applying rule: " rule " to lexical entry: " (strip-refs lexical-entry)))
                                        result (rule lexical-entry)]
                                    (if (fail? result)
                                      (let [message
                                            (str "rule: " rule " caused lexical entry: " (strip-refs lexical-entry) " to fail.")]
                                        (log/error message)
                                        (throw (exception message)))
                                      (do
                                        (log/debug (str "rule: " rule " was ok."))
                                        result))))
                                rules))
                result (if (not (fail? result))
                         (reduce unify (map (fn [rule]
                                              (let [result (rule result)]
                                                (if (fail? result)
                                                  (do (log/error (str "merge-type lexical rule: " rule " caused lexical-entry: " lexical-entry 
                                                                      " to fail; fail path was: " (fail-path result)))
                                                      :fail)
                                                  result)))
                                            modifying-rules))
                           :fail)]
            (if (fail? result) 
              (do
                (log/error (str "lexical entry cannot be added: " (strip-refs result) ";fail-path: " (fail-path result)))
                ;; TODO: throw exception here
                :fail)
              (if (isomorphic? result lexical-entry)
                result ;; done

                ;; not done yet: continue.
                (transform result rules)))))))

;; TODO: remove italian-lexical-string, not used and language-specific.
(defn transform-each-lexical-val [italian-lexical-string lexical-val]
  (map (fn [each]
         (transform each rules))
       lexical-val))

;; TODO: remove intransitive-unspecified-obj: too much of a burden to require this per-language - at least make it optional.
;; TODO: in fact remove this altogether: see babel.italiano.lexicon for
;; how to handle lexical compilation without special-purpose functions like this.
(defn intransitivize [lexicon intransitive transitive intransitive-unspecified-obj]
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     (log/debug (str "intransitivize: key: " k))
     (mapcat (fn [val]
               ;; if: 1. the val's :cat is :verb
               ;;     2. :obj is specified.
               ;;     3. there is no :subcat :2 value specified in the input
               (cond (and (= (get-in val [:synsem :cat])
                             :verb)
                          (not (nil? (get-in val
                                             [:synsem :sem :obj]
                                             nil)))

                          ;; don't try to transform into intransitive if verb is reflexive.
                          (not (= true (get-in val [:synsem :sem :reflexive])))

                          (not (= :adjective (get-in val [:synsem :subcat :2 :cat])))
                          (not (= :intensifier (get-in val [:synsem :subcat :2 :cat])))
                          (not (= :prep (get-in val [:synsem :subcat :2 :cat])))
                          (not (= false (get-in val [:intransitivize])))

                          (not (= '() (get-in val [:synsem :subcat :2]))))

                     (do
                       (list (unify val 
                                    transitive) ;; Make a 2-member list. member 1 is the transitive version..

                             ;; .. and the other member of the list being the intransitive version.
                             ;; Turn the singular, transitive form into an intransitive form by
                             ;; doing some surgery on it: (remove the object) and intransitivize it
                             (let [without-object  ;; intransitive version
                                   (unify intransitive-unspecified-obj
                                          (dissoc-paths val
                                                         (list [:serialized]
                                                               [:synsem :sem :obj]
                                                               [:synsem :subcat :2])))]
                               (log/debug (str "without object: " without-object))
                               (log/debug (str "is fail? (without object): " (fail? without-object)))
                               (let [result
                                     (merge
                                      without-object
                                      {:serialized (serialize without-object)})]
                                 (log/debug (str "is fail (w/o object; merged):" (fail? result)))
                                 result))))
                     
                     (and (= (get-in val [:synsem :cat])
                             :verb)
                          (= '() (get-in val [:synsem :subcat :2] '())))
                     (list (unify val intransitive))
                     
                     ;; else just return vals:
                     true
                     (do (if (= :verb (get-in val [:synsem :cat]))
                           (log/trace (str "no modifications apply for val: " val " ; cat: " 
                                          (get-in val [:synsem :cat]) "; subcat: "
                                          (get-in val [:synsem :subcat]))))
                         (list val))))
             vals))))

(defn transitivize [lexicon transitive verb-subjective]
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     (map (fn [val]
            (cond (and (= (get-in val [:synsem :cat])
                          :verb)
                       (not (= :unspec (get-in val [:synsem :sem :obj])))
                       (not (= '() (get-in val [:synsem :subcat :2])))

                       (not (= false (get-in val [:transitivize])))
                       
                       (not (= :adjective (get-in val [:synsem :subcat :2 :cat])))
                       (not (= :intensifier (get-in val [:synsem :subcat :2 :cat])))
                       (not (= :prep (get-in val [:synsem :subcat :2 :cat])))
                       
                       (not (nil? (get-in val [:synsem :sem :obj] nil))))
                  (unify val
                         transitive)
                  
                  (and (= (get-in val [:synsem :cat]) :verb)
                       (not (= false (get-in val [:transitivize]))))
                  (unify val
                         verb-subjective)
                  true
                  val))
          vals))))

(defn infinitives [lexicon]
  "Get all infinitive verbs in the given lexicon: this is the set of the keys each of whose set of values contains a value which is an infinitive verb (infl=:top)"
 (select-keys lexicon
              (for [[k v] lexicon :when (some (fn [each-val]
                                                (and (= :verb (get-in each-val [:synsem :cat]))
                                                     (= :top (get-in each-val [:synsem :infl] :top))))
                                              v)]
                k)))

(defn default [lexicon unify-with]
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     ;; TODO: consider using pmap.
     (map (fn [original-val]
            (let [result
                  (unify original-val unify-with)]
              (if (not (fail? result))
                result
                original-val)))
          vals))))

(defn new-entries [lexicon unify-with
                   modify-with]
  "similar to (default), but add new entries"
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     (concat
      vals
      (remove fail? 
              (map #(modify-with (unify unify-with %))
                   vals))))))

(defn if-has [lexicon path value-at-path unify-with]
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     ;; TODO: consider using pmap.
     (map (fn [original-val]
            (let [unify-against
                  (create-path-in path (unify (get-in original-val path :fail)
                                               value-at-path))
                  result (unify original-val unify-against unify-with)]
              (if (not (fail? result))
                result
                original-val)))
          vals))))

;; TODO: s/unify/unify/ for performance
;; TODO: use (default) rather than this; it's simpler to understand and faster.
(defn if-then [lexicon if-has unify-with]
  (map-function-on-map-vals
   lexicon
   (fn [k vals]
     ;; TODO: use (map .. x) rather than (mapcat ... (list x))
     (mapcat (fn [val]
               (let [result (unify val if-has)]
                 (cond (not (fail? result))
                       (do
                         (log/debug (str val ": matches: if: " if-has " then " unify-with))
                         (list (unify val unify-with)))
                       true
                       (list val))))
             vals))))

(defn constrain-vals-if
  "for each lexeme <k,v>, if v matches if-fn, then unify value with (then-fn v)."
  [lexicon if-fn then-fn]
  (into {}
        (map (fn [k]
               [k
                (map
                 (fn [val]
                   (cond (if-fn val)
                         (do
                           (log/debug (str val ": matches: constrain-vals:"
                                           (if-fn val)))
                           (unify val (then-fn val)))
                         
                         true
                         val))
                 (get lexicon k))])
             (keys lexicon))))


(defn filter-keys [lexicon filter-fn]
  (into {}
        (map (fn [k]
               [k (get lexicon k)])
             (filter filter-fn (keys lexicon)))))

(defn filter-vals [lexicon filter-fn]
  (into {}
        (map (fn [k]
               [k
                (filter
                 (fn [val]
                   (filter-fn val))
                 (get lexicon k))])
             (keys lexicon))))

(defn rewrite-keys [lexicon rewrite-fn]
  (into {}
        (map (fn [k]
               [(rewrite-fn k)
                (get lexicon k)])
             (keys lexicon))))

(defn get-fail-path [map1 map2]
  (if (and (map? map1)
           (map? map2))
    (let [keys1 (keys map1)
          keys2 (keys map2)
          fail-keys (mapcat (fn [key]
                              (if (fail?
                                   (unify (get-in map1 [key] :top)
                                           (get-in map2 [key] :top)))
                                (list key)))
                            (set (concat keys1 keys2)))]
      (let [first-fail-key (first fail-keys)]
        (if (not (empty? fail-keys))
          (cons
           first-fail-key (get-fail-path (get-in map1 [first-fail-key])
                                         (get-in map2 [first-fail-key])))
          (if (not (nil? first-fail-key))
            [first-fail-key]))))))

(defn lexicon-for-generation [lexicon]
  "filter elements of a lexicon that are not intended for generation (:use-for-generation=false)"
  (into {} (map (fn [k] [k (filter #(not (= false (get-in % [:use-for-generation] true)))
                                   (get lexicon k))])
                (keys lexicon))))

(defn verb-pred-defaults [lexicon verb-pred-default-list]
  (if (not (empty? verb-pred-default-list))
    (verb-pred-defaults (default lexicon (first verb-pred-default-list))
                        (rest verb-pred-default-list))
    lexicon))

(defn apply-unify-key [lexicon]
  (into {}
        (for [[k vals] lexicon]
          [k
           (map (fn [v]
                  (cond
                    (map? v)
                    (reduce unify
                            (cons (dissoc v :unify)
                                  (map (fn [each-unify-arg]
                                         (cond (fn? each-unify-arg)
                                               (each-unify-arg)
                                               true each-unify-arg))
                                       (:unify v))))
                    true v))
                vals)])))
