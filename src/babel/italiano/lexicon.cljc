;; TODO: nouns do not need {:essere false}
(ns babel.italiano.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.encyclopedia :refer [sem-impl]]
   [babel.lexicon :refer [universals]]

   ;; TODO: use dag_unify/unifyc instead:
   ;; deprecate lexiconfn/unify.
   [babel.lexiconfn :refer [compile-lex if-then constrain-vals-if
                            filter-vals
                            map-function-on-map-vals
                            rewrite-keys unify]]

   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [babel.italiano.morphology :refer [exception-generator italian-specific-rules phonize]]
   [babel.italiano.pos :refer [adjective
                               agreement-noun
                               agreement-of-obj-of-main-verb
                               agreement-of-subj-of-main-verb
                               cat-of-pronoun
                               common-noun
                               comparative
                               countable-noun
                               determiner
                               essere-aux-subject-agreement
                               gender-and-number-agreement-1
                               intransitive
                               intransitive-unspecified-obj
                               intransitivize
                               masculine-noun
                               non-comparative-adjective
                               pred-is-obj-pred
                               preposition
                               pronoun-acc
                               pronoun-reflexive
                               reflexive
                               reflexive-obj-is-subcat3
                               sentential-adverb
                               subj-obj-humanness
                               transitive
                               transitivize
                               verb-aux
                               verb-subjective]]
   [clojure.edn :as edn]
   [clojure.java.io :refer [reader]]
   [clojure.repl :refer [doc]]
   [dag_unify.core :refer [fail? get-in strip-refs]]))

(def analyze-lexemes false)

;; TODO: promote to babel.lexicon or babel.lexiconfn
;; also rename babel.lexiconfn to babel.lexicon.
(defn edn2lexicon [filename]
  (let [lexicon-hash-from-edn (read-string (slurp filename))
        lexicon-source 
        (zipmap
         (sort (keys lexicon-hash-from-edn))
         (map (fn [k]
                (let [evaluated (eval (get lexicon-hash-from-edn k))
                      vals (cond (or (seq? evaluated) (vector? evaluated))
                                 evaluated
                                 true [evaluated])]
                  (map (fn [v]
                         (reduce unify
                                 (cons
                                  (dissoc v :unify)
                                  (map (fn [each-unify-arg]
                                         (cond (fn? each-unify-arg)
                                               (each-unify-arg)
                                               true each-unify-arg))
                                 (:unify v)))))
                       vals)))
              (sort (keys lexicon-hash-from-edn))))]
    ;; see TODOs in lexiconfn/compile-lex (should be more of a pipeline as
    ;; opposed to a argument-position-sensitive function.
    (-> (compile-lex lexicon-source
                     exception-generator 
                     ;; TODO: rewrite phonize as (constrain-val-if)(one or more)
                     phonize
                     ;; TODO: rewrite italian-specific-rules as (constrain-vals-if)(one or more)
                     italian-specific-rules)
        
        ;; filter out entries with no :cat.
        (filter-vals
         #(or (and (not (= :none (get-in % [:synsem :cat] :none)))
                   (or (log/debug (str "lexical entry has a cat - good : " (strip-refs %)))
                       true))
              (and (log/warn (str "ignoring lexical entry with no :cat: " (strip-refs %)))
                   false)))
        
        (constrain-vals-if
         (fn [val]
           (not (nil? (get universals (get-in val [:synsem :sem :pred])))))
         (fn [val]
           (get universals (get-in val [:synsem :sem :pred]))))
        
        ;; TODO: refactor this; it's very monolithic currently:
        intransitivize
        
        ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
        ;; TODO: refactor this; it's very monolithic currently:
        transitivize

        (constrain-vals-if
         (fn [val]
           (and (= :noun (get-in val [:synsem :cat]))
                (= true (get-in val [:synsem :reflexive]))
                (= true (get-in val [:synsem :pronoun]))))
         (fn [val]
           (unify (let [case (atom :acc)
                        cat (atom :noun)]
                    {:synsem {:cat cat
                              :pronoun true
                              :subcat '()
                              :reflexive true
                              :case case}
                     :italiano {:cat cat
                                :case case}}))))
        (constrain-vals-if
         (fn [val]
           (and (= (get-in val [:synsem :cat]) :noun)
                (or (= (get-in val [:synsem :agr :gender]) :masc)
                    (= (get-in val [:synsem :agr :gender]) :fem))
                (= false (get-in val [:synsem :propernoun] false))
                (= false (get-in val [:synsem :pronoun] false))
                (not (= '() (get-in val [:synsem :subcat] :top))))) ;; some
         ;; some nouns e.g. "casa" may have a sense that requires no determiner (subcat = '())
         ;; in such cases, don't apply agreement-noun.
         
         (fn [val]
           (unify val agreement-noun)))
        
        (constrain-vals-if
         (fn [val]
           (= (get-in val [:synsem :cat])
              :verb))
         (fn [val]
           (unify val {:stage1 :ok})))
                 
        ;; If a verb is not specifically marked as reflexive, it
        ;; is {:reflexive false}, to prevent generation of reflexive
        ;; sentences using nonreflexive verbs.
        ;; TODO: consider moving this to within intransitivize and transitivize:
        ;; that is, within babel.italiano.pos, mark certain parts of speech
        ;; as reflexive=false to accomplish the same thing as we
        ;; are doing here.
        (constrain-vals-if
         (fn [val]
           (and (= (get-in val [:synsem :cat])
                   :verb)
                (= (get-in val [:synsem :aux] false)
                   false)
                (= :none (get-in val [:synsem :sem :reflexive] :none))))
         (fn [val]
           (unify val {:synsem {:sem {:reflexive false}}})))
        
        ;; if object is not specified, then set to :unspec.
        ;; this prevents translations that may have actual objects - e.g. would allow translations like:
        ;; "io mangio" => "I eat the bread" whereas a better translation is simply "I eat".
        (if-then {:top false
                  :synsem {:cat :verb
                           :aux false
                           :sem {:obj :unspec
                                 :reflexive false}}}
                 {:synsem {:sem {:obj :unspec}}})
        
        ;; subject of verbs must have nominative case: prevents wrong things like article-less noun "casa" being the subject of a sentence.
        (if-then {:synsem {:cat :verb}}
                 {:synsem {:subcat {:1 {:case :nom}}}})
        
        (if-then {:synsem {:cat :prep}}
                 preposition)
        
        ;; filters out any verbs without an inflection: infinitive verbs should have inflection ':top', 
        ;; rather than not having any inflection.
        (filter-vals
         #(or (not (= :verb (get-in % [:synsem :cat])))
              (not (= :none (get-in % [:synsem :infl] :none)))))
        
        (rewrite-keys 
         (fn [k]
           (cond
             (= false analyze-lexemes)
             k
             
             ;; TODO: only an example provided here:
             ;; if analyze-lexemes is true, replace this example with use of
             ;; italiano.morphology/preposition-plus-article regexp pairs
             (= k "alla prossima")
             "a la prossima"
             
             true k))))))

