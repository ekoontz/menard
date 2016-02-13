(ns babel.italiano.grammar
  (:refer-clojure :exclude [get-in merge resolve])
  (:require
   [babel.cache :refer [create-index]]
   [babel.enrich :refer [enrich]]
   [babel.italiano.lexicon :refer [lexicon]]
   [babel.italiano.morphology :refer [analyze fo]]
   [babel.parse :as parse]
   [babel.ug :refer [comp-modifies-head head-principle subcat-1-principle subcat-1-1-principle subcat-2-principle
                     comp-specs-head root-is-head-root
                     subcat-1-1-principle-comp-subcat-1 root-is-comp
                     subcat-2-2-principle root-is-head
                     subcat-5-principle
                     ]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer (fail? get-in merge remove-matching-keys unifyc)]))

(def hc-agreement
  (let [agr (atom :top)]
    {:synsem {:agr agr}
     :head {:synsem {:agr agr}}
     :comp {:italiano {:agr agr}
            :synsem {:agr agr}}}))

(def head-first
  (let [head-italian (atom :top)
        comp-italian (atom :top)]
    (unifyc
     {:comp {:italiano {:initial false}}
      :head {:italiano {:initial true}}}
     {:head {:italiano head-italian}
      :comp {:italiano comp-italian}
      :italiano {:a head-italian
                 :b comp-italian}})))

(def head-last
  (let [head-italian (atom :top)
        comp-italian (atom :top)]
    (unifyc
     {:comp {:italiano {:initial true}}
      :head {:italiano {:initial false}}}
     {:head {:italiano head-italian}
      :comp {:italiano comp-italian}
      :italiano {:a comp-italian
                 :b head-italian}})))

;; -- BEGIN SCHEMA DEFINITIONS
;; <TODO: move to ug>
(def schema-10
  (unifyc
   subcat-1-principle
   head-principle
   {:first :comp
    :comp {:synsem {:subcat '()}}}))

(def c10
  (unifyc
   schema-10
   head-last
   {:comment "c10"
    ;; TODO: using :schema-symbol below - cannot use :schema for some reason; need to figure out why.
    ;; if you try to use :schema, I get:
    ;; java.util.concurrent.ExecutionException: java.lang.RuntimeException:
    ;; Can't embed object in code, maybe print-dup not defined: clojure.lang.Ref@11819f3c
    :schema-symbol 'c10 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comp {:synsem {:subcat '()}}}))

(def c21
  (unifyc
   subcat-2-principle
   head-principle
   head-last
   {:comp {:synsem {:subcat '()
                    :pronoun true}} ;; should not set :pronoun here: should be done
    ;; by usages of c21, such as vp-pronoun (below).
    :schema-symbol 'c21 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comment "c21"}))

(def h11
  (unifyc
   subcat-1-1-principle
   hc-agreement
   head-principle
   comp-modifies-head
   head-first
   {
    :schema-symbol 'h11 ;; used by over-each-parent to know where to put children.
    :first :head
    :comment "h11"}))


(def h11-comp-subcat-1
  (let [subcat (atom :top)]
    (unifyc
     {:head {:synsem {:subcat {:1 subcat}}}
      :comp {:synsem {:subcat {:1 subcat}}}}
     subcat-1-1-principle-comp-subcat-1
     hc-agreement
     head-principle
     comp-modifies-head
     head-first
     {:schema-symbol 'h11-comp-subcat-1
      :first :head
      :comment "h11-comp-subcat-1"})))

(def h10
  (unifyc
   subcat-1-principle
   head-principle
   head-first
   {:comment "h10"
    :schema-symbol 'h10 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def h21
  (unifyc
   subcat-2-principle
   head-principle
   head-first
   {:comment "h21"
    :schema-symbol 'h21
    :first :head}))

;; h21a is a specialization of h21. it's used for vp-aux to prevent over-generation.
(def h21a
  (merge
   (unifyc
    h21
    {:head {:synsem {:subcat {:2 {:subcat {:2 '()}}}}}})
   {:comment "h21a"
    :schema-symbol 'h21a}))

(def h22
  (unifyc
   subcat-2-2-principle
   head-principle
   head-first
   {:comment "h22"
    :schema-symbol 'h22 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def h32
  (unifyc
   subcat-5-principle
   head-principle
   head-first
   {:comment "h32"
    :schema-symbol 'h32 ;; used by over-each-parent to know where to put children.
    :first :head}))

;; </TODO: move to ug>
;; -- END SCHEMA DEFINITIONS

(def grammar (list (unifyc h21
                           {:rule "adjective-phrase"
                            :synsem {:cat :adjective}})

                   (unifyc h21
                           (let [head-synsem {:cat :intensifier
                                              :modified true}] ;; TODO: document what purpose :modified serves (if any: if none, remove).
                             {:rule "intensifier-phrase"
                              :synsem head-synsem}))

                   (unifyc h11-comp-subcat-1
                           (let [head-synsem {:cat :noun
                                              :modified true}]
                             {:comp {:phrasal false ;; rathole prevention ;; TODO: see if this can be removed.
                                     :synsem {:cat :adjective
                                              :mod head-synsem}}
                              :head {:phrasal false
                                     :synsem {:modified false}} ;; TODO: document what purpose :modified serves (if any: if none, remove).
                              :rule "nbar"
                              :synsem head-synsem}))

                   (unifyc c10
                           comp-specs-head
                           (let [number-agreement (atom :top)]
                             {:rule "noun-phrase1"
                              :aliases (list "np1")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :sem {:number number-agreement
                                             :mod '()}}
                              :head {:phrasal false}
                              :comp {:phrasal false}})) ;; rathole prevention ;; TODO: see if this can be removed.

                   (unifyc c10
                           comp-specs-head
                           (let [number-agreement (atom :top)]
                             {:rule "noun-phrase2"
                              :aliases (list "np2")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :sem {:number number-agreement}}
                              :head {:phrasal true}
                              :comp {:phrasal false}})) ;; rathole prevention ;; TODO: see if this can be removed.

                   (unifyc h10
                           {:rule "prepositional-phrase"
                            :synsem {:cat :prep}})

                   (unifyc c10
                           root-is-head-root
                           {:head {:phrasal true ;; only a vp-aux may be the head child, not simply a lexical auxiliary verb.
                                   :synsem {:aux true}}
                            :rule "s-aux"
                            :synsem {:infl :present
                                     :cat :verb
                                     :sem {:aspect :perfect
                                           :tense :past}}})
                   (unifyc c10
                           root-is-head-root
                           {:rule "s-future-phrasal"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :future
                                     :cat :verb
                                     :sem {:tense :future}}})
                   (unifyc c10
                           root-is-head
                           {:rule "s-future-nonphrasal"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :future
                                     :cat :verb
                                     :sem {:tense :future}}})
                   (unifyc c10
                           root-is-head-root
                           {:rule "s-conditional-phrasal"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :conditional
                                     :cat :verb
                                     :sem {:tense :conditional}}})
                   (unifyc c10
                           root-is-head
                           {:rule "s-conditional-nonphrasal"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :conditional
                                     :cat :verb
                                     :sem {:tense :conditional}}})
                   (unifyc c10
                           root-is-head-root
                           {:rule "s-imperfect-phrasal"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :imperfect
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :past}}})
                   (unifyc c10
                           root-is-head
                           {:rule "s-imperfect-nonphrasal"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :imperfect
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :past}}})
                   (unifyc c10
                           root-is-head
                           {:rule "s-present-nonphrasal"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :present
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :present}}})
                   (unifyc c10
                           root-is-head-root
                           {:rule "s-present-phrasal"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :present
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :present}}})
                   (unifyc h21
                           root-is-head
                           {:rule "vp-infinitive"
                            :synsem {:aux false
                                     :infl :infinitive
                                     :cat :verb}})
                   (unifyc h21a
                           root-is-comp
                           {:rule "vp-aux"
                            :head {:phrasal false}
                            :synsem {:aux true
                                     :infl :present
                                     :sem {:tense :past}
                                     :cat :verb}})

                   ;; this rule is kind of complicated and made more so by
                   ;; dependence on auxilary sense of "avere" which supplies the
                   ;; obj-agr agreement between the object and the main (non-auxilary) verb.
                   ;; Note use of :reflexive below.
                   (unifyc h22
                           root-is-comp
                           (let [obj-agr (atom :top)]
                             {:head {:phrasal false}
                              :rule "vp-aux-22"
                              :synsem {:aux true
                                       :cat :verb
                                       :infl :present
                                       :sem {:reflexive true
                                             :tense :past}
                                       :subcat {:2 {:agr obj-agr}}}
                              :italiano {:b {:obj-agr obj-agr}}}))

                   (unifyc h21
                           root-is-head
                           {:rule "vp-future"
                            :synsem {:aux false
                                     :infl :future
                                     :cat :verb}})
                   (unifyc h21
                           root-is-head
                           {:rule "vp-imperfect"
                            :synsem {:aux false
                                     :infl :imperfect
                                     :cat :verb}})
                   (unifyc h21
                           root-is-head
                           {:rule "vp-past"
                            :synsem {:aux false
                                     :infl :past
                                     :cat :verb}})
                   (unifyc h21
                           root-is-head
                           {:rule "vp-present"
                            :synsem {:aux false
                                     :infl :present
                                     :sem {:tense :present}
                                     :cat :verb}})
                   (unifyc c21
                           root-is-head-root
                           {:head {:phrasal true}
                            :comp {:synsem {:cat :noun
                                            :pronoun true}}
                            :rule "vp-pronoun-phrasal"
                            :synsem {:cat :verb
                                     :infl {:not :past}}})

                   ;; Note {:reflexive true} below: for now, we are trying to
                   ;; only generate reflexive sentences (e.g. "io mi alzo") and not transitive
                   ;; sentences with pronouns (e.g. "io lo vedo").
                   ;; We have a problem with generating transitive sentences otherwise,
                   ;; which is not always desired in a given educational context (if we want to concentrate
                   ;; on intransitive verbs plus reflexives).
                   ;; Later, when we do want to generate transitive sentences with pronouns, we might need
                   ;; to split this rule into two: one for reflexives, one for transitives.
                   (unifyc c21
                           root-is-head
                           {:head {:phrasal false
                                   :synsem {:sem {:reflexive true}}}
                            :comp {:synsem {:cat :noun
                                            :reflexive true
                                            :pronoun true}}
                            :rule "vp-pronoun-nonphrasal"
                            :synsem {:cat :verb
                                     :infl {:not :past}}})
                   (unifyc h10
                           root-is-comp
                           {:head {:phrasal false
                                   :synsem {:cat :sent-modifier}}
                            :rule "s-modifier"})))

(defn aux-is-head-feature [phrase]
  (cond (= :verb (get-in phrase '(:synsem :cat)))
        (unifyc phrase
                (let [ref (atom :top)]
                  {:synsem {:aux ref}
                   :head {:synsem {:aux ref}}}))
        true phrase))

(defn modal-is-head-feature [phrase]
  (cond (= :verb (get-in phrase '(:synsem :cat)))
        (unifyc phrase
                (let [ref (atom :top)]
                  {:synsem {:modal ref}
                   :head {:synsem {:modal ref}}}))
        true phrase))

;; TODO: warn about failures.
(def grammar
  (map (fn [phrase]
         (modal-is-head-feature
          (aux-is-head-feature phrase)))
       (filter #(not (fail? %))
               grammar)))

;; TODO: move to italiano/morphology or higher (language-universal)
(defn morph-walk-tree [tree]
  (log/debug (str "morph-walk-tree: " (fo tree)))
  (merge
   {:surface (fo tree)}
   (if (get-in tree [:comp])
     {:comp (morph-walk-tree (get-in tree [:comp]))}
     {})
   (if (get-in tree [:head])
     {:head (morph-walk-tree (get-in tree [:head]))})))

(def small
  (let [grammar
        (filter #(or (= (:rule %) "s-conditional-phrasal")
                     (= (:rule %) "s-conditional-nonphrasal")
                     (= (:rule %) "s-present-phrasal")
                     (= (:rule %) "s-present-nonphrasal")
                     (= (:rule %) "s-future-phrasal")
                     (= (:rule %) "s-future-nonphrasal")
                     (= (:rule %) "s-imperfect-phrasal")
                     (= (:rule %) "s-imperfect-nonphrasal")
                     (= (:rule %) "s-aux")
                     (= (:rule %) "vp-aux")
                     (= (:rule %) "vp-aux-22")
                     (= (:rule %) "vp-pronoun-nonphrasal")
                     (= (:rule %) "vp-pronoun-phrasal"))
                grammar)
        lexicon
        (into {}
              (for [[k v] lexicon]
                (let [filtered-v
                      (filter #(or (= (get-in % [:synsem :cat]) :verb)
                                   (= (get-in % [:synsem :propernoun]) true)
                                   (= (get-in % [:synsem :pronoun]) true))
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))]
    {:name "small"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "it"
     :language-keyword :italiano
     :morph fo
     :lookup (fn [arg]
               (analyze arg lexicon))
     :enrich enrich
     :grammar grammar
     :lexicon lexicon
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))

(def medium
  (let [lexicon
        (into {}
              (for [[k v] lexicon]
                (let [filtered-v v]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        rules (map #(keyword (get-in % [:rule])) grammar)]
    {:name "medium"
     :language "it"
     :language-keyword :italiano
     :morph fo
     :lookup (fn [arg]
               (analyze arg lexicon))
     :enrich enrich
     :grammar grammar
     :rules rules
     :rule-map (zipmap rules
                       grammar)
     :lexicon lexicon
     :index (create-index grammar (flatten (vals lexicon)) head-principle)
     }))

(def np-grammar
  (let [grammar
        (filter #(or (= (:rule %) "noun-phrase2")
                     (= (:rule %) "nbar"))
                grammar)
        lexicon
        (into {}
              (for [[k v] lexicon]
                (let [filtered-v
                      (filter #(or (= (get-in % [:synsem :cat] :adjective) :adjective)
                                   (= (get-in % [:synsem :cat] :det) :det)
                                   (and (= (get-in % [:synsem :cat] :noun) :noun)
                                        (not (= (get-in % [:synsem :propernoun] false) true))
                                        (not (= (get-in % [:synsem :pronoun] false) true))))
                              v)
                      remove-semantic-features
                      (map (fn [lexeme]
                             (remove-matching-keys lexeme
                                                   #(or
                                                     (= % :activity)        (= % :animate)  (= % :artifact)
                                                     (= % :buyable)         (= % :child)    (= % :clothing)
                                                     (= % :consumable)
                                                     (= % :drinkable)       (= % :edible)   (= % :furniture)
                                                     (= % :human)           (= % :legible)  (= % :part-of-human-body)
                                                     (= % :pet)
                                                     (= % :physical-object) (= % :place)    (= % :speakable))))
                           filtered-v)]
                  (if (not (empty? remove-semantic-features))
                    [k remove-semantic-features]))))]
    {:name "np-grammar"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "it"
     :language-keyword :italiano
     :morph fo
     :lookup (fn [arg]
               (analyze arg lexicon))
     :enrich enrich
     :grammar grammar
     :lexicon lexicon
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))

(log/info "Italiano grammars defined (small, medium).")
