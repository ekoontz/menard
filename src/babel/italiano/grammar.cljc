(ns babel.italiano.grammar
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [babel.index :refer [create-indices intersection-with-identity lookup-spec map-subset-by-path]]
   [babel.italiano.lexicon :refer [deliver-lexicon lexicon]]
   [babel.italiano.morphology :refer [analyze fo]]
   [babel.parse :as parse]
   [babel.ug :refer [apply-default-if comp-modifies-head comp-specs-head
                     head-principle
                     root-is-comp root-is-comp-root
                     root-is-head root-is-head-root
                     subcat-1-principle
                     subcat-1-1-principle subcat-2-principle
                     subcat-1-1-principle-comp-subcat-1 
                     subcat-2-2-principle
                     subcat-5-principle verb-default?]
    :as ug]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.core.cache :as cache]
   [clojure.repl :refer (doc)]
   [dag_unify.core :refer (fail? get-in remove-matching-keys strip-refs unify)]))

(def index-lexicon-on-paths
  [
   [:italiano :italiano]
   [:synsem :aux]
   [:synsem :cat]
   [:synsem :essere]
   [:synsem :infl]
   [:synsem :sem :pred]
   ])

(def tenses
  {"present simple" {:synsem {:sem {:tense :present
                                    :aspect :simple}}}
   "present progressive" {:synsem {:sem {:tense :present
                                    :aspect :progressive}}}
   "conditional" {:synsem {:sem {:tense :conditional}}}
   "future" {:synsem {:sem {:tense :future}}}
   "imperfetto" {:synsem {:sem {:aspect :progressive
                                :tense :past}}}
   "passato" {:synsem {:sem {:aspect :perfect
                             :tense :present}}}
   "trapassato" {:synsem {:sem {:aspect :pluperfect
                                :tense :past}}}})

(defn fo-ps [expr]
  (parse/fo-ps expr fo))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. (str ": " error-string))))
  #?(:cljs
     (throw (js/Error. error-string))))

(defn unify-check [ & vals]
  (let [result (apply unify vals)]
    (if (fail? result)
      (exception (str "failed to unify grammar rule with values: " vals))
      result)))

(def hc-agreement
  (let [agr (atom :top)]
    {:synsem {:agr agr}
     :head {:synsem {:agr agr}}
     :comp {:italiano {:agr agr}
            :synsem {:agr agr}}}))

(def cat-sharing
  (let [head-cat (atom :top)
        comp-cat (atom :top)]
    {:comp {:synsem {:cat comp-cat}
            :italiano {:cat comp-cat}}
     :head {:synsem {:cat head-cat}
            :italiano {:cat head-cat}}}))

(def head-first
  (let [head-italian (atom :top)
        comp-italian (atom :top)]
    (unify
     cat-sharing
     {:comp {:italiano {:initial false}}
      :head {:italiano {:initial true}}}
     {:head {:italiano head-italian}
      :comp {:italiano comp-italian}
      :italiano {:a head-italian
                 :b comp-italian}})))
(def head-last
  (let [head-italian (atom :top)
        comp-italian (atom :top)
        head-cat (atom :top)
        comp-cat (atom :top)]
    (unify
     cat-sharing
     {:comp {:italiano {:initial true}}
      :head {:italiano {:initial false}}}
     {:head {:italiano head-italian}
      :comp {:italiano comp-italian}
      :italiano {:a comp-italian
                 :b head-italian}})))

;; -- BEGIN SCHEMA DEFINITIONS
(def c10
  (unify
   ug/c10
   head-last))

(def c21
  (unify
   ug/c21 head-last))

(def h11
  (unify
   ug/h11
   head-first))

;; <TODO: move most of the content to babel.ug as the above examples (c10,c21,h11) do.>
(def c11-comp-subcat-1
  (let [subcat (atom :top)]
    (unify
     {:head {:synsem {:subcat {:1 subcat}}}
      :comp {:synsem {:subcat {:1 subcat}}}}
     subcat-1-1-principle-comp-subcat-1
     hc-agreement
     head-principle
     comp-modifies-head
     head-last
     {:schema-symbol 'c11-comp-subcat-1
      :first :head
      :comment "c11-comp-subcat-1"})))

(def h11-comp-subcat-1
  (let [subcat (atom :top)]
    (unify
     {:head {:synsem {:subcat {:1 subcat}}}
      :comp {:synsem {:subcat {:1 subcat}}}}
     subcat-1-1-principle-comp-subcat-1
     hc-agreement
     head-principle
     comp-modifies-head
     head-first
     {:schema-symbol 'h11-comp-subcat-1
      :first :comp
      :comment "h11-comp-subcat-1"})))

(def h10
  (unify
   subcat-1-principle
   head-principle
   head-first
   {:comment "h10"
    :schema-symbol 'h10 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def h21
  (unify
   subcat-2-principle
   head-principle
   head-first
   {:comment "h21"
    :schema-symbol 'h21
    :first :head}))

;; h21a is a specialization of h21. it's used for vp-aux to prevent over-generation.
(def h21a
  (merge
   (unify
    h21
    {:head {:synsem {:subcat {:2 {:subcat {:2 '()}}}}}})
   {:comment "h21a"
    :schema-symbol 'h21a}))

(def h22
  (unify
   subcat-2-2-principle
   head-principle
   head-first
   {:comment "h22"
    :schema-symbol 'h22 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def h32
  (unify
   subcat-5-principle
   head-principle
   head-first
   {:comment "h32"
    :schema-symbol 'h32 ;; used by over-each-parent to know where to put children.
    :first :head}))

(def c00
  (unify-check
   head-last
   {:comment "c00"
    :schema-symbol 'c00 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comp {:synsem {:subcat '()}}
    :head {:synsem {:subcat '()}}}))

(def h00
  (unify-check
   head-first
   {:comment "h00"
    :schema-symbol 'h00 ;; used by over-each-parent to know where to put children.
    :first :head
    :comp {:synsem {:subcat '()}}
    :head {:synsem {:subcat '()}}}))

;; </TODO: move most of the content to babel.ug>
;; -- END SCHEMA DEFINITIONS

(def vp-non-pronoun
  {:comp {:synsem {:pronoun false}}})

(def modified {:modified true})
(def unmodified {:modified false})

(def grammar (list (unify h21
                           {:rule "adjective-phrase"
                            :synsem {:cat :adjective}})

                   (unify h21
                           (let [head-synsem {:cat :intensifier
                                              :modified true}] ;; TODO: document what purpose :modified serves (if any: if none, remove).
                             {:rule "intensifier-phrase"
                              :synsem head-synsem}))

;                   (let [parent-subcat (atom :top)
;                         sem (atom :top)
;                         infl (atom :top)
;                         comp-synsem (atom {:sem sem
;                                            :infl infl
;                                            :subcat {:1 parent-subcat}})]
;                     (unify
;                      head-first
;                      {:head {:synsem {:cat :adverb
;                                       :sem sem
;                                       :subcat {:1 comp-synsem}}}
;                       :first :head
;                       :comp {:synsem comp-synsem}
;                       :synsem {:cat :verb
;                                :infl infl
;                                :aux false
;                                :sem sem
;                                :subcat parent-subcat}
;                       :schema-symbol 'h11
;                       :rule "adverb-phrase"}))
                   
                   ;; nbar where head (noun) is first ('h' in h11)
                   (unify h11-comp-subcat-1
                           (let [is-propernoun? (atom :top)]
                             {:comp {:phrasal false ;; rathole prevention ;; TODO: see if this can be removed.
                                     :synsem {:cat :adjective}}
                              :head {:phrasal false
                                     :synsem {:modified false ;; TODO: document what purpose :modified serves (if any: if none, remove).
                                              :propernoun is-propernoun?}}
                              :rule "nbar1"
                              :synsem {:cat :noun
                                       :modified true
                                       :propernoun is-propernoun?}}))

                   ;; nbar where complement (adjective) is first ('c' in c11)
                   (unify c11-comp-subcat-1
                           (let [is-propernoun? (atom :top)]
                             {:comp {:phrasal false ;; rathole prevention ;; TODO: see if this can be removed.
                                     :synsem {:cat :adjective}}
                              :head {:phrasal false
                                     :synsem {:modified false ;; rathole prevention: as above, try to remove if possible
                                              :propernoun is-propernoun?}}
                              :rule "nbar2"
                              :synsem {:cat :noun
                                       :modified true
                                       :propernoun is-propernoun?}}))

                   (unify c10
                           comp-specs-head
                           (let [number-agreement (atom :top)
                                 is-propernoun? (atom :top)]
                             {:rule "noun-phrase1"
                              :aliases (list "np1")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :propernoun is-propernoun?
                                       :sem {:number number-agreement
                                             :mod '()}}
                              :head {:phrasal false
                                     :synsem {:propernoun is-propernoun?}}
                              :comp {:phrasal false}})) ;; rathole prevention ;; TODO: see if this can be removed.

                   (unify h10
                           {:rule "np-to-n-pp"
                            :synsem {:cat :noun}
                            :head {:phrasal false}
                            :comp {:synsem {:cat :prep
                                            :sem {:pred :di}}}})
                   (unify c10
                           comp-specs-head
                           (let [number-agreement (atom :top)
                                 is-propernoun? (atom :top)]
                             {:rule "noun-phrase2"
                              :aliases (list "np2")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :sem {:number number-agreement}
                                       :propernoun is-propernoun?}
                              :head {:phrasal true
                                     :synsem {:propernoun is-propernoun?}}
                              :comp {:phrasal false}})) ;; rathole prevention ;; TODO: see if this can be removed.

                   (let [reflexive (atom :top)
                         sem (atom :top)]
                     (unify-check h10
                                  subcat-1-principle
                                  {:rule "prepositional-phrase"
                                   :synsem {:cat :prep
                                            :reflexive reflexive
                                            :sem sem}
                                   :head {:synsem {:sem sem
                                                   :subcat {:1 {:reflexive reflexive}}}}}))

;                   (unify h21
;                           {:rule "adjunct-prepositional-phrase"
;                            :synsem {:cat :prep}})
                   
                   (unify c10
                          root-is-head-root
                          {:head {:phrasal true ;; only a vp-aux may be the head child, not simply a lexical auxiliary verb.
                                  :synsem {:aux true}}
                             :rule "s-aux"
                             :synsem {:cat :verb}})

                   ;; TODO: consolidate s-future-(non)phrasal,s-conditional-(non)phrasal,etc
                   ;; into fewer rules and use a default rule to choose among them.
                   ;; (see babel.english.grammar/default-fn)
                   (unify c10
                           root-is-head-root
                           {:rule "s-future-phrasal"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :future
                                     :cat :verb
                                     :sem {:tense :future}}})
                   (unify c10
                           root-is-head
                           {:rule "s-future-nonphrasal"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :future
                                     :cat :verb
                                     :sem {:tense :future}}})
                   (unify c10
                           root-is-head-root
                           {:rule "s-conditional-phrasal"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :conditional
                                     :cat :verb
                                     :sem {:tense :conditional}}})
                   (unify c10
                           root-is-head
                           {:rule "s-conditional-nonphrasal"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :conditional
                                     :cat :verb
                                     :sem {:tense :conditional}}})
                   (unify c10
                           root-is-head-root
                           {:rule "s-imperfect-phrasal"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :imperfect
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :past}}})
                   (unify c10
                           root-is-head
                           {:rule "s-imperfect-nonphrasal"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :imperfect
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :past}}})
                   (unify c10
                           root-is-head
                           {:rule "s-present-nonphrasal"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :present
                                     :cat :verb
                                     :sem {:aspect :simple
                                           :tense :present}}})
                   (unify c10
                           root-is-head-root
                           {:rule "s-present-phrasal"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :present
                                     :cat :verb
                                     :sem {:aspect :simple
                                           :tense :present}}})
                   (unify h21
                           root-is-head
                           {:rule "vp-infinitive"
                            :synsem {:aux false
                                     :infl :infinitive
                                     :cat :verb}})
                   (unify h21a
                          root-is-comp-root
                          {:rule "vp-aux-phrasal-complement"
                           :head {:phrasal false
                                  :synsem {:aux true}}
                           :comp {:phrasal true}
                           :synsem {:aux true
                                    :cat :verb}})
                   (unify h21a
                          root-is-comp
                          {:rule "vp-aux-nonphrasal-complement"
                           :head {:phrasal false
                                  :synsem {:aux true}}
                           :comp {:phrasal false}
                           :synsem {:aux true
                                    :cat :verb}})
                   
                   ;; this rule is kind of complicated and made more so by
                   ;; dependence on auxilary sense of "avere" which supplies the
                   ;; obj-agr agreement between the object and the main (non-auxilary) verb.
                   ;; Note use of :reflexive below.
                   (unify h22
                           root-is-comp
                           vp-non-pronoun
                           (let [obj-agr (atom :top)]
                             {:head {:phrasal false
                                     :synsem {:aux true}}
                              :rule "vp-aux-22"
                              :synsem {:aux true
                                       :cat :verb
                                       :sem {:reflexive true}
                                       :subcat {:2 {:agr obj-agr}}}
                              :italiano {:b {:obj-agr obj-agr}}}))

                   (unify h21
                           root-is-head
                           {:rule "vp-future"
                            :synsem {:aux false
                                     :infl :future
                                     :cat :verb}}
                           vp-non-pronoun)

                   (unify h21
                           root-is-head
                           {:rule "vp-imperfect"
                            :synsem {:aux false
                                     :infl :imperfect
                                     :cat :verb}}
                           vp-non-pronoun)
                   
                   (unify h21
                           root-is-head
                           {:rule "vp-past"
                            :synsem {:aux false
                                     :infl :past
                                     :cat :verb}}
                           vp-non-pronoun)
                   
                   (unify h21
                           root-is-head
                           {:rule "vp-present"
                            :synsem {:aux false
                                     :infl :present
                                     :sem {:tense :present
                                           :aspect :simple}
                                     :cat :verb}}
                           vp-non-pronoun)
                   
                   (unify c21
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
                   (unify c21
                           root-is-head
                           {:head {:phrasal false
                                   :synsem {:sem {:reflexive true}}}
                            :comp {:synsem {:cat :noun
                                            :reflexive true
                                            :pronoun true}}
                            :rule "vp-pronoun-nonphrasal"
                            :synsem {:cat :verb
                                     :infl {:not :past}}})

                   ;; e.g. used as: "io mi chiamo Luisa" -
                   ;; [s-present-phrasal 'io' [vp-pronoun-phrasal 'mi' [vp-32 'chiamo' 'Luisa']]]
                   (unify h32
                           root-is-head
                           {:rule "vp-32"
                            :head {:phrasal false
                                   :synsem {:aux false}}
                            :synsem {:aux false
                                     :infl {:not :past}
                                     :cat :verb}})
                   (unify h10
                           root-is-comp
                           {:head {:phrasal false
                                   :synsem {:cat :sent-modifier}}
                            :rule "s-modifier"})

                   (unify-check c00
                                comp-modifies-head
                                modified
                                (let [sem (atom :top)
                                      essere (atom :top)] ;; TODO: use (default) to set this for all {:cat :verb} rules.
                                  {:synsem {:subcat '()
                                            :essere essere
                                            :cat :verb
                                            :sem sem}
                                   :comp {:synsem {:cat :prep
                                                   :subcat '()}}
                                   :head {:modified false
                                          :essere essere
                                          :synsem {:cat :verb
                                                   :sem sem
                                                   :subcat '()}}
                                   :rule "s-modified-modifier-first"}))

                   (unify-check h00
                                comp-modifies-head
                                modified
                                (let [sem (atom :top)
                                      essere (atom :top)]  ;; TODO: use (default) to set this for all {:cat :verb} rules.
                                  {:synsem {:subcat '()
                                            :cat :verb
                                            :essere essere 
                                            :sem sem}
                                   :comp {:synsem {:cat :prep
                                                   :subcat '()}}
                                   :head {:modified false
                                          :synsem {:cat :verb
                                                   :essere essere
                                                   :sem sem
                                                   :subcat '()}}
                                   :rule "s-modified-modifier-last"}))))

(defn aux-is-head-feature [phrase]
  (cond (= :verb (get-in phrase '(:synsem :cat)))
        (unify phrase
                (let [ref (atom :top)]
                  {:synsem {:aux ref}
                   :head {:synsem {:aux ref}}}))
        true phrase))

(defn modal-is-head-feature [phrase]
  (cond (= :verb (get-in phrase '(:synsem :cat)))
        (unify phrase
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

(defn lexicon-for-generation [lexicon]
  (into {}
        (for [[k v] lexicon]
          (let [filtered-v
                (filter #(not (= false (get-in % [:generate-with] true))) v)]
            (if (not (empty? filtered-v))
              [k filtered-v])))))

(defn model [ & [name lexicon-filter-fn grammar-filter-fn]]
  "create a model using a lexicon derived from the supplied filtering function."
  (deliver-lexicon)
  (let [lexicon-filter-fn (or lexicon-filter-fn (fn [x] x))
        grammar-filter-fn (or grammar-filter-fn (fn [x] x))
        lexicon @lexicon
        lexicon
        (into {}
              (for [[k v] lexicon]
                (let [filtered-v
                      (filter lexicon-filter-fn v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        grammar (filter grammar-filter-fn grammar)]
    {:name name
     :language "it"
     :language-keyword :italiano
     :morph fo
     :morph-ps fo-ps
     :lookup (fn [arg]
               (analyze arg lexicon))
     :generate {:lexicon lexicon}
     :grammar grammar
     :grammar-map (zipmap
                   (map #(keyword (get-in % [:rule])) grammar)
                   grammar)
     :lexicon lexicon
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))}))

(defn small []
  (deliver-lexicon)
  (let [lexicon @lexicon
        grammar
        (filter #(or (= (:rule %) "s-conditional-phrasal")
                     (= (:rule %) "s-conditional-nonphrasal")
                     (= (:rule %) "s-present-phrasal")
                     (= (:rule %) "s-present-nonphrasal")
                     (= (:rule %) "s-future-phrasal")
                     (= (:rule %) "s-future-nonphrasal")
                     (= (:rule %) "s-imperfect-phrasal")
                     (= (:rule %) "s-imperfect-nonphrasal")
                     (= (:rule %) "s-aux")
                     (= (:rule %) "vp-32")
                     (= (:rule %) "vp-aux-phrasal-complement")
                     (= (:rule %) "vp-aux-22")
                     (= (:rule %) "vp-aux-nonphrasal-complement")
                     (= (:rule %) "vp-present")
                     (= (:rule %) "vp-pronoun-nonphrasal")
                     (= (:rule %) "vp-pronoun-phrasal")
                     (= (:rule %) "np-to-n-plus-di"))
                grammar)

          lexicon  ;; create a subset of the lexicon tailored to this small grammar.
          (into {}
                (for [[k v] lexicon]
                  (let [filtered-v
                        (filter #(and
                                  (not (= true (get-in % [:top]))) ;; exclude the ":top" wildcard lexeme. actually
                                  ;; babel.italiano.lexicon/edn2lexicon already excludes the wildcard lexeme,
                                  ;; so this filtering rule will not find anything to exclude.
                                  
                                  (or (and
                                       (= (get-in % [:synsem :cat]) :verb)

                                       ;; disabled with (or true) but shows how to filter by infinitive.
                                       (or true (= (get-in % [:italiano :italiano]) "sedersi")
                                           (= (get-in % [:synsem :aux]) true))
                                       
                                       (or (= (get-in % [:synsem :sem :obj] :unspec) :unspec) ;; exclude transitive verbs..
                                           ;; ..but allow reflexive verbs.
                                           (= (get-in % [:synsem :sem :reflexive] false) true)

                                           ;; ..or "avere" + bisogna di
                                           (and (= (get-in % [:synsem :sem :pred]) :need)
                                                (= (get-in % [:italiano :infinitive]) "avere")))
                                       
                                       ;; exclude verbs that take an adverb as the third argument.
                                       (not (= (get-in % [:synsem :subcat :3 :cat]) :adverb))) 
                                       
                                      ;; exclude cities from the small grammar.
                                      (and (= (get-in % [:synsem :propernoun]) true)
                                           (= (get-in % [:synsem :sem :city] false) false)) 
          
                                      (= (get-in % [:synsem :pronoun]) true)

                                      (= (get-in % [:italiano :italiano]) "bisogno")
                                      (= (get-in % [:italiano :italiano]) "di")
                                      ))
                                      
                                v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        lexicon-for-generation (lexicon-for-generation lexicon)

        indices (create-indices lexicon-for-generation index-lexicon-on-paths)]
        
    {:index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))

     :name "small"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "it"
     :language-keyword :italiano
     :morph (fn [expression & {:keys [from-language show-notes]}] (fo expression))
     :morph-ps fo-ps
     :generate {:lexicon lexicon-for-generation}
     :grammar grammar
     :lexicon lexicon
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lookup (fn [arg]
               (analyze arg lexicon))

     :rule-map (zipmap (map #(keyword (get-in % [:rule])) grammar)
                       grammar)}))

;; TODO: promote to babel.writer
(defn create-model-for-spec [spec]
  (let [root (get-in spec [:root :italiano :italiano])
        pred (get-in spec [:synsem :sem :pred])
        small (small)
        micro-lexicon
        (into {}
              (for [[k v] (:lexicon small)]
                (let [filtered-v
                      (filter #(and
                                (not (= true (get-in % [:top]))) ;; exclude the ":top" wildcard lexeme. actually
                                ;; some languages (e.g. Italian in babel.italiano.lexicon/edn2lexicon)
                                ;; may already exclude the wildcard lexeme,
                                ;; so this filtering rule will not find anything to exclude.
                                  
                                ;; include all aux verbs:
                                (or (not (= (get-in % [:synsem :cat]) :verb))
                                    (= (get-in % [:synsem :aux]) true)
                                    (= (get-in % [:italiano :italiano]) root) ;; TODO: support other languages besides Italian.
                                    (= (get-in % [:synsem :sem :pred]) pred))

                                ;; only allow intransitive and reflexive verbs:
                                (or (not (= (get-in % [:synsem :cat]) :verb))
                                    (= (get-in % [:synsem :sem :obj] :unspec) :unspec) ;; exclude transitive verbs (intransitive verbs will have :obj=:unspec)
                                    (= (get-in % [:synsem :sem :reflexive] false) true))  ;; ..but allow reflexive verbs.

                                ;; exclude verbs that take an adverb as the third argument.
                                (or (not (= (get-in % [:synsem :cat]) :verb))
                                    (not (= (get-in % [:synsem :subcat :3 :cat]) :adverb)))
                                       
                                ;; exclude cities from the small grammar.
                                (or (not (= (get-in % [:synsem :propernoun]) true))
                                    (= (get-in % [:synsem :sem :city] false) false)))
                              
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))]
    (log/debug (str "micro lexicon size:" (count (keys micro-lexicon))))
    (clojure.core/merge small
                        {:lexicon micro-lexicon})))

(defn default-fn [tree]
  (log/debug (str "Italiano: do-defaults (pre) on tree: " (parse/fo-ps tree fo)))
  (log/debug (str "aspect (pre): " (strip-refs (get-in tree
                                                       [:synsem :sem :aspect]
                                                       ::unset))))
  (log/debug (str "infl   (pre): " (strip-refs (get-in tree
                                                       [:synsem :infl]
                                                       ::unset))))  
  (log/debug (str "tense  (pre): " (strip-refs (get-in tree
                                                       [:synsem :sem :tense]
                                                       ::unset))))
  (let [result
        (-> tree
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :present
                             :aspect :simple}
                       :infl :present}})
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :present
                             :aspect :progressive}
                       :infl :present-progressive}})
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :future}
                       :infl :future}})
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :conditional}
                       :infl :conditional}}))]
    result))

(defn medium []
  (deliver-lexicon)
  ;; TODO: remove parse-lexicon.
  ;; Should not need a separate parse-lexicon here: for debugging,
  ;; instead add to the lexicon and entry like "_" (as with english/lexicon.clj).
  (let [lexicon @lexicon
        parse-lexicon
        (into {}
              (for [[k v] lexicon]
                (let [filtered-v 
                      (filter (fn [x] true)
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        lexicon
        (into {}
              (for [[k v] lexicon]
                (let [filtered-v
                      (filter #(not (= true (get-in % [:top])))
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        lexicon-for-generation (lexicon-for-generation lexicon)
        rules (map #(keyword (get-in % [:rule])) grammar)

        ;; indices from paths to subsets of the lexicon
        indices (create-indices lexicon-for-generation index-lexicon-on-paths)]
        
    {:index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))
     :default-fn default-fn
     :name "medium"
     :generate {:lexicon lexicon-for-generation}
     :grammar grammar
     :language "it"
     :language-keyword :italiano
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lexicon lexicon
     :lookup (fn [arg]
               (analyze arg parse-lexicon))
     :morph fo
     :morph-ps fo-ps

     :rules rules
     :rule-map (zipmap rules
                       grammar)
     }))

(defn np-grammar []
  (deliver-lexicon)
  (let [lexicon @lexicon
        grammar
        (filter #(or (= (:rule %) "noun-phrase1")
                     (= (:rule %) "noun-phrase2")
                     (= (:rule %) "nbar1")
                     (= (:rule %) "nbar2"))
                grammar)
        rules (map #(keyword (get-in % [:rule])) grammar)
        lexicon
        (into {}
              (for [[k v] lexicon]
                (let [filtered-v
                      (filter #(and (not (= true (get-in % [:top])))
                                    (or (= (get-in % [:synsem :cat] :adjective) :adjective)
                                        (= (get-in % [:synsem :cat] :det) :det)
                                        (and (= (get-in % [:synsem :cat] :noun) :noun)
                                             (not (= (get-in % [:synsem :propernoun] false) true))
                                             (not (= (get-in % [:synsem :pronoun] false) true)))))
                              v)
                      ;; TODO: remove this removal:
                      ;; don't remove this semantic info. It's an interesting example of how to
                      ;; process a lexicon, though, so perhaps find a reason to use it in some way.
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
                    [k remove-semantic-features]))))
        lexicon-for-generation (lexicon-for-generation lexicon)
        ;; indices from paths to subsets of the lexicon
        indices (create-indices lexicon-for-generation index-lexicon-on-paths)]
    {:index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))
     :name "np-grammar"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "it"
     :language-keyword :italiano
     :morph-ps fo-ps
     :morph fo
     :lookup (fn [arg]
               (analyze arg lexicon))
     :generate {:lexicon lexicon-for-generation}
     :grammar grammar
     :lexicon lexicon
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :rules rules
     :rule-map (zipmap rules grammar)}))

