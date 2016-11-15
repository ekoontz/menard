(ns babel.english.grammar
  (:refer-clojure :exclude [get-in])
  (:require 
   [babel.english.lexicon :refer [lexicon]]
   [babel.english.morphology :refer (analyze fo)]
   [babel.index :refer [create-indices lookup-spec]]
   [babel.lexiconfn :refer [apply-default]]
   [babel.over :refer (over)]
   [babel.parse :as parse]
   [babel.ug :as ug
    :refer [comp-modifies-head
            comp-specs-head
            head-principle
            root-is-comp
            root-is-head root-is-head-root
            subcat-1-principle
            subcat-1-1-principle
            subcat-1-1-principle-comp-subcat-1
            subcat-2-principle
            subcat-2-2-principle
            subcat-5-principle
            unify-check
            ]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.core.cache :as cache]
   [dag_unify.core :refer [fail? get-in remove-matching-keys unify]]))

(def index-lexicon-on-paths
  [[:synsem :agr :gender]
   [:synsem :agr :number]
   [:synsem :agr :person]
   [:synsem :aux]
   [:synsem :cat]
   [:synsem :pronoun]
   [:synsem :sem :pred]
   [:synsem :sem :human]])

(defn apply-default-if [tree
                        test
                        to-apply]
  (if (test tree)
    (apply-default tree to-apply)
    tree))

(defn verb-default? [tree]
  (and (= :verb (get-in tree [:synsem :cat]))
       (or (= :top (get-in tree [:synsem :sem :tense] :top))
           (= :top (get-in tree [:synsem :infl] :top)))))

(defn default-fn [tree]
  (log/debug (str "English: do-defaults (pre) on tree: " (fo tree)))
  (let [result
        (-> tree
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :present
                             :aspect :progressive}
                       :infl :present}})
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :future}
                       :infl :future}})
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :conditional}
                       :infl :conditional}})
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:aspect :progressive
                             :tense :past}
                       :infl :imperfect}}))]
    (log/debug (str "English: do-defaults (post) on tree: " (fo result)))
    result))

(declare cache)

(defn fo-ps [expr]
  (parse/fo-ps expr fo))

(def hc-agreement
  (let [agr (atom :top)]
    {:synsem {:agr agr}
     :head {:synsem {:agr agr}}
     :comp {:english {:agr agr}
            :synsem {:agr agr}}}))

(def head-first
  (let [agr (atom :top)
        head-english (atom {:agr agr})
        comp-english (atom :top)]
    {:head {:english head-english}
     :comp {:english comp-english}
     :english {:a head-english
               :agr agr
               :b comp-english}
     :first :head}))
               
(def head-last
  (let [agr (atom :top)
        head-english (atom {:agr agr})
        comp-english (atom :top)]

    {:head {:english head-english}
     :comp {:english comp-english}
     :english {:a comp-english
               :agr agr
               :b head-english}}))

;; -- BEGIN SCHEMA DEFINITIONS
(def c10
  (unify-check
   ug/c10
   head-last))

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
    :comp {:synsem {:subcat '()}}
    :head {:synsem {:subcat '()}}}))

(def c11
  (unify-check
   subcat-1-1-principle
   hc-agreement
   head-principle
   comp-modifies-head
   head-last
   {
    :schema-symbol 'c11 ;; used by over-each-parent to know where to put children.
    :first :head
    :comment "c11"}))

;; TODO: names like "c11-comp-subcat-1" have no human-discernible meaning:
;; sound like droid names or something..
(def c11-comp-subcat-1
  (let [subcat (atom :top)]
    (unify-check
     {:head {:synsem {:subcat {:1 subcat}}}
      :comp {:synsem {:subcat {:1 subcat}}}}
     subcat-1-1-principle-comp-subcat-1
     hc-agreement
     head-principle
     head-last
     {:schema-symbol 'c11-comp-subcat-1
      :first :comp
      :comment "c11-comp-subcat-1"})))

(def h10
  (unify-check
   subcat-1-principle
   head-principle
   head-first
   {:comment "h10"
    :schema-symbol 'h10 ;; used by over-each-parent to know where to put children.
    }))

(def h21
  (unify-check
   subcat-2-principle
   head-principle
   head-first
   {:comment "h21"
    :schema-symbol 'h21})) ;; used by over-each-parent to know where to put children.

(def h22
  (unify-check
   subcat-2-2-principle
   head-principle
   head-first
   {:comment "h22"
    :schema-symbol 'h22 ;; used by over-each-parent to know where to put children.
    }))

(def h32
  (unify-check
   subcat-5-principle
   head-principle
   head-first
   {:comment "h32"
    :schema-symbol 'h32 ;; used by over-each-parent to know where to put children.
    }))

;; </TODO: move to ug>
;; -- END SCHEMA DEFINITIONS

(def modified {:modified true})
(def unmodified {:modified false})

(def grammar (list (unify-check h21
                                {:rule "adjective-phrase"
                                 :synsem {:cat :adjective}})
                   
                   (unify-check h21
                                (let [head-synsem {:cat :intensifier}]
                                  {:rule "intensifier-phrase"
                                   :synsem head-synsem}))
                   
                   (unify-check h10
                                (let [semantics (atom :top)
                                      comp-type (atom :top)]
                                  {:rule "complementizer-phrase"
                                   :synsem {:cat :comp
                                            :comp-type comp-type
                                            :sem semantics}
                                   :head {:synsem {:comp-type comp-type
                                                   :sem semantics}}}))
                   (unify-check c10
                           {:rule "determiner-phrase"
                            :synsem {:cat :det}})

                   (unify-check c11-comp-subcat-1
                           (let [propernoun (atom :top)
                                 head-sem (atom :top)]
                             {:modified true
                              :synsem {:reflexive false
                                       :propernoun propernoun}
                              :comp {:synsem {:cat :adjective
                                              :sem head-sem}}
                              :head {:modified false
                                     :synsem {:cat :noun
                                              :propernoun propernoun
                                              :sem head-sem}}
                              :rule "nbar"}))

                   ;; TODO: do we need noun-phrase1? its only distinction is its' [:sem :mod] = '()
                   ;; and I'm not sure why this distinction is necessary.
                   (unify-check c10
                           comp-specs-head
                           (let [number-agreement (atom :top)
                                 propernoun (atom :top)]
                             {:rule "noun-phrase1"
                              :aliases (list "np1")
                              :synsem {:agr {:number number-agreement}
                                       :reflexive false
                                       :cat :noun
                                       :propernoun propernoun
                                       :sem {:number number-agreement
                                             :mod '()}}
                              :head {:phrasal false
                                     :synsem {:propernoun propernoun}}}))
                   (unify-check c10
                           comp-specs-head
                           (let [number-agreement (atom :top)
                                 propernoun (atom :top)]
                             {:rule "noun-phrase2"
                              :aliases (list "np2")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :reflexive false
                                       :propernoun propernoun
                                       :sem {:mod {:pred :top}
                                             :number number-agreement}}
                              :head {:phrasal true
                                     :synsem {:propernoun propernoun}}}))

                   (let [sem (atom :top)
                         agr (atom :top)
                         reflexive (atom :top)]
                     (unify-check h10
                      subcat-1-principle
                      head-first
                      {:comment "h10"
                       :schema-symbol 'h10
                       :first :head
                       :rule "prepositional-phrase"
                       :synsem {:agr agr
                                :cat :prep
                                :reflexive reflexive
                                :sem sem}
                       :head {:synsem {:sem sem
                                       :subcat {:1 {:agr agr
                                                    :reflexive reflexive}}}}}))
                   (unify-check c10
                                unmodified
                                root-is-head
                                {:rule "sentence-nonphrasal-head"
                                 :synsem {:cat :verb}})

                   (unify-check c10
                                unmodified
                                root-is-head-root
                                {:head {:phrasal true}
                                 :rule "sentence-phrasal-head"
                                 :synsem {:cat :verb}})

                   (unify-check h21
                                root-is-head
                                {:rule "transitive-vp-nonphrasal-head"
                                 :synsem {:aux false
                                          :cat :verb}})
                   (unify-check h21
                                root-is-head-root
                                {:rule "transitive-vp-phrasal-head"
                                 :head {:phrasal true}
                                 :synsem {:aux false
                                          :cat :verb}})
                   (unify-check h32
                           root-is-head-root
                           {:rule "vp32"
                            :head {:phrasal-verb true}
                            :synsem {:aux false
                                     :cat :verb}})

                   (unify-check h10
                           {:head {:phrasal false
                                   :synsem {:cat :sent-modifier}}
                            :rule "s-modifier"})))

(defn aux-is-head-feature [phrase]
  (cond (= :verb (get-in phrase '(:synsem :cat)))
        (unify-check phrase
                (let [ref (atom :top)]
                  {:synsem {:aux ref}
                   :head {:synsem {:aux ref}}}))
        true phrase))

(defn modal-is-head-feature [phrase]
  (cond (= :verb (get-in phrase '(:synsem :cat)))
        (unify-check phrase
                (let [ref (atom :top)]
                  {:synsem {:modal ref}
                   :head {:synsem {:modal ref}}}))
        true phrase))

;; TODO: rewrite this and and above 2 functions into
;; a single threaded (->) function.
(def grammar
  (map (fn [phrase]
         (modal-is-head-feature
          (aux-is-head-feature phrase)))
       grammar))

(defn morph-walk-tree [tree]
  (log/debug (str "morph-walk-tree: " (fo tree)))
  (merge
   {:surface (fo (get-in tree [:english]))}
   (if (get-in tree [:comp])
     {:comp (morph-walk-tree (get-in tree [:comp]))}
     {})
   (if (get-in tree [:head])
     {:head (morph-walk-tree (get-in tree [:head]))})))

(defn very-small []
  (let [grammar
        (filter #(or (= (:rule %) "s-conditional-nonphrasal-head")
                     (= (:rule %) "s-present-nonphrasal-head")
                     (= (:rule %) "s-future-nonphrasal-head")
                     (= (:rule %) "s-imperfect-nonphrasal-head")
                     (= (:rule %) "s-past-nonphrasal-head")
                     (= (:rule %) "s-aux"))
                grammar)

        lexicon
        (into {}
              (for [[k v] @lexicon]
                (let [filtered-v
                      (filter #(or (= (get-in % [:synsem :cat]) :verb)
                                   (= (get-in % [:synsem :propernoun]) true)
                                   (= (get-in % [:synsem :pronoun]) true))
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        ]
    {:name "very-small"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "en"
     :morph fo
     :morph-ps fo-ps
     :grammar grammar
     :lookup (fn [arg]
               (analyze arg lexicon))
     :lexicon lexicon
     :for {:es ;; a lexicon specific to when we want to use Español as a target.
           (into {}
                 (for [[k v] lexicon]
                   (let [filtered-v
                         (filter #(or (= :unset (get-in % [:target]))
                                      (= :es (get-in % [:target])))
                                 v)]
                     (if (not (empty? filtered-v))
                       [k filtered-v]))))
           
           :it  ;; a lexicon specific to when we want to use Italiano as a target.
           (into {}
                 (for [[k v] lexicon]
                   (let [filtered-v
                         (filter #(or (= :unset (get-in % [:target]))
                                      (= :it (get-in % [:target])))
                                 v)]
                     (if (not (empty? filtered-v))
                       [k filtered-v]))))}}))

(defn small-plus-vp-pronoun []
  (let [grammar
        (filter #(or (= (:rule %) "s-conditional-nonphrasal-head")
                     (= (:rule %) "s-conditional-phrasal-head")
                     (= (:rule %) "s-present-nonphrasal-head")
                     (= (:rule %) "s-present-phrasal-head")
                     (= (:rule %) "s-future-nonphrasal-head")
                     (= (:rule %) "s-future-phrasal-head")
                     (= (:rule %) "s-imperfect-nonphrasal-head")
                     (= (:rule %) "s-imperfect-phrasal-head")
                     (= (:rule %) "s-past-nonphrasal-head")
                     (= (:rule %) "s-past-phrasal-head")
                     (= (:rule %) "s-aux")
                     (= (:rule %) "vp-past")
                     (= (:rule %) "vp-pronoun"))
                grammar)
        lexicon
        (into {}
              (for [[k v] @lexicon]
                (let [filtered-v
                      (filter #(or (= (get-in % [:synsem :cat]) :verb)
                                   (= (get-in % [:synsem :propernoun]) true)
                                   (= (get-in % [:synsem :pronoun]) true))
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        ]
    {:name "small-plus-vp-pronoun"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "en"
     :lookup (fn [arg]
               (analyze arg lexicon))
     :morph fo
     :morph-ps fo-ps
     :grammar grammar
     :lexicon lexicon
     :for {:es ;; a lexicon specific to when we want to use Español as a target.
           (into {}
                 (for [[k v] lexicon]
                   (let [filtered-v
                         (filter #(or (= :unset (get-in % [:target]))
                                      (= :es (get-in % [:target])))
                                 v)]
                     (if (not (empty? filtered-v))
                       [k filtered-v]))))
           
           :it  ;; a lexicon specific to when we want to use Italiano as a target.
           (into {}
                 (for [[k v] lexicon]
                   (let [filtered-v
                         (filter #(or (= :unset (get-in % [:target]))
                                      (= :it (get-in % [:target])))
                                 v)]
                     (if (not (empty? filtered-v))
                       [k filtered-v]))))}}))

(defn small-plus-plus-np []
  (let [grammar
        (filter #(or (= (:rule %) "s-conditional-nonphrasal-head")
                     (= (:rule %) "s-conditional-phrasal-head")
                     (= (:rule %) "s-present-nonphrasal-head")
                     (= (:rule %) "s-present-phrasal-head")
                     (= (:rule %) "s-future-nonphrasal-head")
                     (= (:rule %) "s-future-phrasal-head")
                     (= (:rule %) "s-imperfect-nonphrasal-head")
                     (= (:rule %) "s-imperfect-phrasal-head")
                     (= (:rule %) "s-past-nonphrasal-head")
                     (= (:rule %) "s-past-phrasal-head")
                     (= (:rule %) "s-aux")
                     (= (:rule %) "vp-past")
                     (= (:rule %) "vp-present")
                     (= (:rule %) "vp-pronoun")
                     (= (:rule %) "noun-phrase1"))
                grammar)

        lexicon
        (into {}
              (for [[k v] @lexicon]
                (let [filtered-v
                      (filter #(or (= (get-in % [:synsem :cat]) :verb)
                                   (= (get-in % [:synsem :sem :propernoun]) true)
                                   (= (get-in % [:synsem :cat]) :noun)
                                   (= (get-in % [:synsem :pronoun]) true)
                                   (= (get-in % [:synsem :cat]) :det))
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        ]
    {:name "small-plus-plus-np"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "en"
     :morph fo
     :morph-ps fo-ps
     :grammar grammar
     :lookup (fn [arg]
               (analyze arg lexicon))

     ;; Will throw exception if more than 1 rule has the same :rule value:
     :grammar-map (zipmap
                   (map #(keyword (get-in % [:rule])) grammar)
                   grammar)

     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lexicon lexicon
     :for {:es ;; a lexicon specific to when we want to use Español as a target.
           (into {}
                 (for [[k v] lexicon]
                   (let [filtered-v
                         (filter #(or (= :unset (get-in % [:target]))
                                      (= :es (get-in % [:target])))
                                 v)]
                     (if (not (empty? filtered-v))
                       [k filtered-v]))))

           :it  ;; a lexicon specific to when we want to use Italiano as a target.
           (into {}
                 (for [[k v] lexicon]
                   (let [filtered-v
                         (filter #(or (= :unset (get-in % [:target]))
                                      (= :it (get-in % [:target])))
                                 v)]
                     (if (not (empty? filtered-v))
                       [k filtered-v]))))}}))

(defn small []
  (let [grammar
        (filter #(or (= (:rule %) "s-conditional-nonphrasal-head")
                     (= (:rule %) "s-conditional-phrasal-head")
                     (= (:rule %) "s-present-nonphrasal-head")
                     (= (:rule %) "s-present-phrasal-head")
                     (= (:rule %) "s-future-nonphrasal-head")
                     (= (:rule %) "s-future-phrasal-head")
                     (= (:rule %) "s-imperfect-nonphrasal-head")
                     (= (:rule %) "s-imperfect-phrasal-head")
                     (= (:rule %) "s-past-nonphrasal-head")
                     (= (:rule %) "s-past-phrasal-head")
                     (= (:rule %) "s-aux")
                     (= (:rule %) "vp-conditional")
                     (= (:rule %) "vp-future")
                     (= (:rule %) "vp-imperfect")
                     (= (:rule %) "vp-past")
                     (= (:rule %) "vp-present")
                     (= (:rule %) "vp-pronoun")
                     (= (:rule %) "noun-phrase1"))
                grammar)

        lexicon
        (into {}
              (for [[k v] @lexicon]
                (let [filtered-v
                      (filter #(or (and (= (get-in % [:synsem :cat]) :verb)
                                        (not (= :verb (get-in % [:synsem :subcat :2 :cat])))) ;; exclude modal verbs e.g. "I want to .."
                                   (= (get-in % [:synsem :sem :propernoun]) true)
                                   (= (get-in % [:synsem :cat]) :noun)
                                   (= (get-in % [:synsem :pronoun]) true)
                                   (= (get-in % [:synsem :cat]) :det))
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        indices (create-indices lexicon index-lexicon-on-paths)]
    {:name "small"
     :index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "en"
     :morph fo
     :morph-ps fo-ps
     :grammar grammar
     :lookup (fn [arg]
               (analyze arg lexicon))

     ;; Will throw exception if more than 1 rule has the same :rule value:
     :grammar-map (zipmap
                   (map #(keyword (get-in % [:rule])) grammar)
                   grammar)

     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lexicon lexicon
     :for {:es ;; a lexicon specific to when we want to use Español as a target.
           (into {}
                 (for [[k v] lexicon]
                   (let [filtered-v
                         (filter #(or (= :unset (get-in % [:target]))
                                      (= :es (get-in % [:target])))
                                 v)]
                     (if (not (empty? filtered-v))
                       [k filtered-v]))))

           :it  ;; a lexicon specific to when we want to use Italiano as a target.
           (into {}
                 (for [[k v] lexicon]
                   (let [filtered-v
                         (filter #(or (= :unset (get-in % [:target]))
                                      (= :it (get-in % [:target])))
                                 v)]
                     (if (not (empty? filtered-v))
                       [k filtered-v]))))}}))
(defn medium []
  (let [lexicon
        (into {}
              (for [[k v] @lexicon]
                (let [filtered-v v]
                  (if (not (empty? filtered-v))  ;; TODO: this empty-filtering should be done in lexicon.cljc, not here.
                    [k filtered-v]))))
        indices (create-indices lexicon index-lexicon-on-paths)
        morph fo]
    {:name "medium"
     :default-fn default-fn
     :index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))
     ;; Will throw a clojure/core-level exception if more than 1 rule has the same :rule value:
     :grammar-map (zipmap
                   (map #(keyword (get-in % [:rule])) grammar)
                   grammar)

     :language-keyword :english

     :grammar grammar
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lexicon lexicon
     :lookup (fn [arg]
               (analyze arg lexicon))
     :morph fo
     :morph-ps fo-ps
     ;; TODO: remove: not used
     :morph-walk-tree (fn [tree]
                        (merge tree
                               (morph-walk-tree tree)))}))
    
(defn np-grammar []
  (let [grammar
        (filter #(or (= (:rule %) "noun-phrase2")
                     (= (:rule %) "nbar"))
                grammar)
        lexicon
        (into {}
              (for [[k v] @lexicon]
                 (let [filtered-v
                      (filter #(or (= (get-in % [:synsem :cat]) :adjective)
                                   (= (get-in % [:synsem :cat]) :det)
                                   (and (= (get-in % [:synsem :cat]) :noun)
                                        (not (= (get-in % [:synsem :propernoun] false) true))
                                        (not (= (get-in % [:synsem :pronoun] false) true))))
                              v)
                      remove-semantic-features
                      (map (fn [lexeme]
                             (remove-matching-keys lexeme
                                                   #(or
                                                     (= % :activity)        (= % :animate)  (= % :artifact)
                                                     (= % :buyable)         (= % :clothing) (= % :consumable)
                                                     (= % :drinkable)       (= % :edible)   (= % :furniture)
                                                     (= % :human)           (= % :legible)  (= % :part-of-human-body)
                                                     (= % :physical-object) (= % :place)    (= % :speakable))))
                           filtered-v)]
                  (if (not (empty? remove-semantic-features))
                    [k remove-semantic-features]))))]
    {:name "np-grammar"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "en"
     :language-keyword :english
     :morph fo
     :morph-ps fo-ps
     :lookup (fn [arg]
               (analyze arg lexicon))
     :grammar grammar
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lexicon lexicon}))

(defn few-rules []
  (let [grammar
        (filter #(or (= (:rule %) "determiner-phrase")
                     (= (:rule %) "noun-phrase2")
                     (= (:rule %) "nbar"))
                grammar)
        lexicon @lexicon]
    {:name "few-rules"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "en"
     :language-keyword :english
     :morph fo
     :morph-ps fo-ps
     :lookup (fn [arg]
               (analyze arg lexicon))
     :grammar grammar
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lexicon lexicon}))

(defn small-lexicon []
  (let [grammar grammar
        lexicon (into {}
                      (for [[k v] @lexicon]
                        (if (or (= k "dog")
                                (= k "Luisa")
                                (= k "second")
                                (= k "s"))
                          [k v])))]
    {:name "small-lexicon"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "en"
     :language-keyword :english
     :morph fo
     :morph-ps fo-ps
     :lookup (fn [arg]
               (analyze arg lexicon))
     :grammar grammar
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lexicon lexicon}))
