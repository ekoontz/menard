(ns babel.english.grammar
  (:refer-clojure :exclude [get-in])
  (:require 
   [babel.english.lexicon :refer [lexicon]]
   [babel.english.morphology :refer (analyze fo)]
   [babel.index :refer [create-indices lookup-spec]]
   [babel.over :refer (over)]
   [babel.parse :as parse]
   [babel.ug :as ug
    :refer [apply-default-if
            comp-modifies-head
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
            unify-check verb-default?
            ]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.core.cache :as cache]
   [dag_unify.core :refer [fail? get-in remove-matching-keys strip-refs unify]]))

(def index-lexicon-on-paths
  [[:synsem :agr :gender]
   [:synsem :agr :number]
   [:synsem :agr :person]
   [:synsem :aux]
   [:synsem :cat]
   [:synsem :pronoun]
   [:synsem :sem :pred]
   [:synsem :sem :human]])

(defn default-fn [tree]
  (log/debug (str "English: do-defaults (pre) on tree: " (parse/fo-ps tree fo)))
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
                       :infl :conditional}})

            ;; past progressive
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:aspect :progressive
                             :tense :past}
                       :infl :imperfect}})

            ;; simple past
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:aspect :perfect
                             :tense :present}
                       :infl :past}})

            ;; pluperfect
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:aspect :pluperfect
                             :tense :past}
                       :infl :pluperfect}}))]
    (log/debug (str "English: do-defaults (post) on tree: " (fo result)))
    (log/debug (str "aspect (post): " (strip-refs (get-in result
                                                         [:synsem :sem :aspect]
                                                         ::unset))))
    (log/debug (str "infl   (post): " (strip-refs (get-in result
                                                          [:synsem :infl]
                                                          ::unset))))  
    (log/debug (str "tense  (post): " (strip-refs (get-in result
                                                          [:synsem :sem :tense]
                                                          ::unset))))
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
                                 :synsem {:cat :verb}
                                 :head {:synsem {:participle false}}})

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

(defn medium []
  (let [lexicon
        (into {}
              (for [[k v] @lexicon]
                (let [filtered-v v]
                  (if (not (empty? filtered-v))  ;; TODO: this empty-filtering should be done in lexicon.cljc, not here.
                    [k filtered-v]))))
        indices (create-indices lexicon index-lexicon-on-paths)
        ;; this function 'morph' is identical to: babel.english/morph
        morph (fn [expr & {:keys [from-language show-notes]
                           :or {from-language nil
                                show-notes false}}]
                (fo expr
                    :from-language from-language :from-notes show-notes
                    :lexicon lexicon))
        model
        {:name "medium"
         :default-fn default-fn
         :semantic-correspondence {:it [[:obj :null]
                                        [:obj :number]
                                        [:shared-with-obj]
                                        [:subj :null]
                                        [:subj :number]]}
         :index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))
         ;; Will throw a clojure/core-level exception if more than 1 rule has the same :rule value:
         :grammar-map (zipmap
                       (map #(keyword (get-in % [:rule])) grammar)
                       grammar)
         
         :grammar grammar
         
         :language "en"
         :language-keyword :english
         
         :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
         :lexicon lexicon
         :lookup (fn [arg]
                   (analyze arg lexicon))
         :morph morph
         :morph-ps fo-ps
         ;; TODO: remove: not used
         :morph-walk-tree (fn [tree]
                            (merge tree
                                   (morph-walk-tree tree)))}]
    (assoc model
          :generate-fn
          (fn [spec]
            (babel.generate/generate spec model)))))
