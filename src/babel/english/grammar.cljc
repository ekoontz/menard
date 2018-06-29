(ns babel.english.grammar
  (:refer-clojure :exclude [get-in])
  (:require 
   [babel.english.lexicon :refer [deliver-lexicon transform-with-english-lexical-rules
                                  vocab-entry-to-lexeme]]
   [babel.english.morphology :refer (analyze fo)]
   [babel.generate :as generate]
   [babel.html :refer [local-timestamp]]
   [babel.index :refer [create-indices lookup-spec]]
   [babel.lexiconfn :refer [edn2lexicon filtered-lexicon read-lexicon]]
   [babel.parse :as parse]
   [babel.ug :as ug
    :refer [apply-default-if comp-modifies-head
            comp-specs-head head-semantics
            head-principle root-is-comp
            root-is-head root-is-head-root
            subcat-1-principle subcat-1-1-principle
            subcat-1-1-principle-comp-subcat-1
            subcat-2-principle subcat-2-2-principle
            subcat-5-principle unify-check
            verb-default?]]
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

(defn noun-default? [tree]
  (and (= :noun (get-in tree [:synsem :cat]))
       (= :top (get-in tree [:synsem :agr :number] :top))))

(defn default-fn [tree]
  (log/debug (str "English: do-defaults (pre) on tree: " (parse/fo-ps tree fo)))
  (log/trace (str "aspect (pre): " (strip-refs (get-in tree
                                                       [:synsem :sem :aspect]
                                                       ::unset))))
  (log/trace (str "infl   (pre): " (strip-refs (get-in tree
                                                       [:synsem :infl]
                                                       ::unset))))  
  (log/trace (str "tense  (pre): " (strip-refs (get-in tree
                                                       [:synsem :sem :tense]
                                                       ::unset))))
  (let [result
        (-> tree
            (apply-default-if
             noun-default?
             {:synsem {:agr {:number :sing}}})
            
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
    (log/debug (str "English: do-defaults (post) on tree: " (parse/fo-ps result fo)))
    (log/trace (str "aspect (post): " (strip-refs (get-in result
                                                          [:synsem :sem :aspect]
                                                          ::unset))))
    (log/trace (str "infl   (post): " (strip-refs (get-in result
                                                          [:synsem :infl]
                                                          ::unset))))  
    (log/trace (str "tense  (post): " (strip-refs (get-in result
                                                          [:synsem :sem :tense]
                                                          ::unset))))
    [result]))

(declare cache)
(declare model-with-vocab-items)

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
               :b head-english}
     :first :comp}))

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
    :comp {:synsem {:subcat []}}
    :head {:synsem {:subcat []}}}))

(def h00
  (unify-check
   head-first
   {:comment "h00"
    :schema-symbol 'h00 ;; used by over-each-parent to know where to put children.
    :comp {:synsem {:subcat []}}
    :head {:synsem {:subcat []}}}))

(def c11
  (unify-check
   subcat-1-1-principle
   hc-agreement
   head-principle
   comp-modifies-head
   head-last
   {
    :schema-symbol 'c11 ;; used by over-each-parent to know where to put children.
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

(def grammar
  [(unify-check h21
                {:rule "adjective-phrase"
                 :synsem {:cat :adjective}})
   
   (unify-check h21
                (let [head-synsem {:cat :intensifier}]
                  {:rule "intensifier-phrase"
                   :synsem head-synsem}))
   
   (unify-check h10
                (let [semantics (atom :top)
                      comp-type (atom :top)]
                  {:rule "complementizer-phrase-nonslash"
                   :synsem {:cat :comp
                            :comp-type comp-type
                            :sem semantics}
                   :comp {:synsem {:slash false}}
                   :head {:synsem {:comp-type comp-type
                                   :sem semantics}}}))
   (unify-check
    {:rule "complementizer-phrase-slash"}
    head-first
    head-principle
    {:synsem {:slash true
              :cat :comp}
     :head {:phrasal false}
     :comp {:phrasal true
            ;; TODO: the below should be in complementizer's [:subcat :1]:
            :synsem {:slash true}}})

   (unify-check c10
                {:rule "determiner-phrase"
                 :synsem {:cat :det}
                 :head {:synsem {:slash false}}})

   ;;         nbar   |sem  [1] |
   ;;       /      \ |mod <[2]>|
   ;;      /        \
   ;; adj |sem [2]|  n |sem [1]|
   ;; 
   (unify-check c11-comp-subcat-1
                (let [head-constraint (atom :top)
                      adj-sem (atom {:prop head-constraint})
                      head-mod (atom :top)]
                  {:rule "nbar"
                   :synsem {:mod {:first adj-sem
                                  :rest head-mod}}
                   :comp {:synsem {:cat :adjective
                                   :sem adj-sem}}
                   :head {:phrasal false
                          :synsem {:cat :noun
                                   :sem {:prop head-constraint}
                                   :mod head-mod}}}))
   ;; noun-phrase -> det nbar
   (unify-check c10
                comp-specs-head
                (let [number-agreement (atom :top)
                      mod (atom :top)]
                  {:rule "noun-phrase"
                   :aliases (list "np")
                   :synsem {:agr {:number number-agreement}
                            :reflexive false
                            :cat :noun
                            :sem {:mod mod
                                  :number number-agreement}}
                   :head {:phrasal :top
                          :synsem {:mod mod}}}))

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
   (unify-check
    {:rule "s/obj"}
    (let [object-synsem (atom {:top :top})
          subject-synsem (atom :top)]
      (unify-check head-last
                   head-principle
                   {:comp {:synsem subject-synsem}
                    :head {:synsem {:sem {:reflexive false}
                                    :slash false
                                    :subcat {:1 subject-synsem
                                             :2 object-synsem
                                             :3 []}}}
                    :synsem {:cat :verb
                             :slash true
                             :subcat {:1 object-synsem
                                      :2 []}}})))
   (unify-check
    {:rule "nbar-s-obj"
     :example "(the) [nbar dog [s-obj you see]]"}
    head-first
    head-principle
    (let [mod-subj (atom :top)
          mod-pred (atom :top)
          mod-prop (atom :top)
          head-mod (atom :top)
          mod-aspect (atom :top)
          mod-tense (atom :top)
          head-subcat (atom {:1 {:cat :det}
                             :2 []})]
      {:slash false
       :synsem {:sem {:prop mod-prop}
                :mod {:first {:subj mod-subj
                              :tense mod-tense
                              :aspect mod-aspect
                              :obj :modified
                              :pred mod-pred}
                      :rest head-mod}
                :subcat head-subcat}
       :comp {:synsem {:slash true
                       :cat :verb
                       :subcat {:1 {:cat :noun}
                                :2 []}
                       :sem {:subj mod-subj
                             :aspect mod-aspect
                             :obj {:prop mod-prop}
                             :pred mod-pred
                             :tense mod-tense}}}
       :head {:rule "nbar"
              :synsem {:cat :noun
                       :subcat head-subcat
                       :mod head-mod}}}))
   (unify-check c10
                unmodified
                root-is-head
                {:rule "sentence-nonphrasal-head"
                 :synsem {:cat :verb
                          :slash false}
                 :head {:phrasal false
                        :slash false
                        :synsem {:participle false}}})
   (unify-check c10
                unmodified
                root-is-head-root
                {:head {:phrasal true
                        :synsem {:slash false}}
                 :rule "sentence-phrasal-head"
                 :synsem {:cat :verb
                          :slash false}})
   
   ;; "sees a book" :complement is semantic object
   (unify-check h21
                root-is-head
                {:rule "transitive-vp-nonphrasal-head"
                 :synsem {:aux false
                          :slash false
                          :cat :verb}})

   ;; "gives a book" [to X] :complement is the semantic object
   (unify-check h32
                root-is-head
                {:rule "ditransitive-vp-nonphrasal-head-1"
                 :head {:applied {:ditrans true}
                        :phrasal false
                        :synsem {:subcat {:2 {:top :top}
                                          :3 {:top :top}}}}
                 :synsem {:cat :verb
                          :slash false
                          :subcat {:2 {:top :top}
                                   :3 []}}})

   ;; "has seen a dog" : complement is semantic object.
   (let [obj-mod (atom :top)
         obj (atom {:mod obj-mod})]
     (unify-check h21
                  root-is-head-root
                  {:rule "transitive-vp-phrasal-head"
                   :comp {:synsem {:mod obj-mod
                                   :sem obj}}
                   :head {:phrasal true}
                   :synsem {:aux false
                            :sem {:obj obj}
                            :slash false
                            :cat :verb}}))

   ;; "[h turn ] [c off]"
   (let [iobj-mod (atom :top)]
     (unify-check h32
                  root-is-head
                  {:rule "vp-phrasal-verb"
                   :head {:phrasal false
                          :phrasal-verb true}
                   :comp {:synsem {:mod iobj-mod}}
                   :synsem {:aux false
                            :cat :verb
                            :sem {:iobj iobj-mod}
                            :slash false}}))

   ;;   "[h give the book] [c to you]"
   (unify-check h21
                {:rule "vp-to-vp-pp"}
                root-is-head-root
                {:head {:phrasal true
                        :phrasal-verb false}
                 :comp {:synsem {:cat :prep}}
                 :synsem {:aux false
                          :cat :verb
                          :slash false}})])

(defn aux-is-head-feature [phrase]
  (cond (= :verb (get-in phrase [:synsem :cat]))
        (unify-check phrase
                     (let [ref (atom :top)]
                       {:synsem {:aux ref}
                        :head {:synsem {:aux ref}}}))
        true phrase))

(defn modal-is-head-feature [phrase]
  (cond (= :verb (get-in phrase [:synsem :cat]))
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

(defn compile-lexicon []
  (into {}
        (for [[k v] (deliver-lexicon)]
          (let [filtered-v v]
            (if (not (empty? filtered-v))  ;; TODO: this empty-filtering should be done in lexicon.cljc, not here.
              [k filtered-v])))))

(defn model []
  (let [debug (log/info "  loading lexicon..")
        lexicon (read-lexicon "en")
        debug (log/info "  indices..")
        indices (create-indices lexicon index-lexicon-on-paths)
        ;; this function 'morph' is identical to: babel.english/morph
        debug (log/info "  morph..")
        morph (fn [expr & {:keys [from-language show-notes]
                           :or {from-language nil
                                show-notes false}}]
                (fo expr
                    :from-language from-language :show-notes show-notes
                    :lexicon lexicon))
        debug (log/info "  finalizing..")
        model
        {:name (str "English language model created with ❤ by babel.english.grammar/model "
                    "at: " (local-timestamp))
         :default-fn default-fn
         :index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))
         ;; Will throw a clojure/core-level exception if more than 1 rule has the same :rule value:
         :grammar-map (zipmap
                       (map #(keyword (get-in % [:rule])) grammar)
                       grammar)
         
         :grammar grammar
         
         :language "en"
         :language-keyword :english
         
         ;; TODO: unused; remove.
         :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
         :lexicon lexicon
         :lookup (fn [arg]
                   (analyze arg lexicon))
         :morph morph
         :morph-ps fo-ps}]
    (merge model
           {:vocab2model (fn [vocab-items filter-lexicon-fn]
                           (model-with-vocab-items vocab-items filter-lexicon-fn model))
            :generate-fn
            (fn [spec]
              (generate/generate spec model))})))

;;(def source-model @@(get babel.directory/models :en))
;;(def filter-lexicon-fn #(= :det (get-in % [:synsem :cat])))
;;(def new-model ((:vocab2model source-model) source-vocab-items filter-lexicon-fn))
;;(clojure.pprint/pprint (get (:lexicon new-model) "wine"))

(defn model-with-vocab-items [vocab-items filter-lexicon-fn model]
  (let [input-lexicon (transform-with-english-lexical-rules
                       (reduce merge (map vocab-entry-to-lexeme vocab-items)))
        lexicon (merge-with concat
                            input-lexicon
                            (filtered-lexicon
                             (:lexicon model)
                             filter-lexicon-fn))
        indices (create-indices lexicon index-lexicon-on-paths)
        model (merge model
                     {:lexicon lexicon})]
    (merge model
           {:index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))})))
