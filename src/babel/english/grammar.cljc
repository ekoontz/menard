(ns babel.english.grammar
  (:refer-clojure :exclude [get-in])
  (:require 
   [babel.cache :refer (build-lex-sch-cache create-index spec-to-phrases)]
   [babel.english.lexicon :refer [lexicon]]
   [babel.english.morphology :refer (analyze fo)]
   [babel.enrich :refer [enrich]]
   [babel.over :refer (over)]
   [babel.parse :as parse]
   [babel.ug :refer [comp-modifies-head
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
                     ]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer (get-in remove-matching-keys unifyc)]))

(declare cache)

(def hc-agreement
  (let [agr (atom :top)]
    {:synsem {:agr agr}
     :head {:synsem {:agr agr}}
     :comp {:english {:agr agr}
            :synsem {:agr agr}}}))

(def head-first
  (let [head-english (atom :top)
        comp-english (atom :top)]
    {:head {:english head-english}
     :comp {:english comp-english}
     :english {:a head-english
               :b comp-english}}))

(def head-last
  (let [head-english (atom :top)
        comp-english (atom :top)]
    {:head {:english head-english}
     :comp {:english comp-english}
     :english {:a comp-english
               :b head-english}}))

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

(def c11
  (unifyc
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
;; sound like construction droid names or something..
(def c11-comp-subcat-1
  (let [subcat (atom :top)]
    (unifyc
     {:head {:synsem {:subcat {:1 subcat}}}
      :comp {:synsem {:subcat {:1 subcat}}}}
     subcat-1-1-principle-comp-subcat-1
     hc-agreement
     head-principle
     comp-modifies-head
     head-last
     {:schema-symbol 'c11-comp-subcat-1
      :first :comp
      :comment "c11-comp-subcat-1"})))

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
    :schema-symbol 'h21 ;; used by over-each-parent to know where to put children.
    :first :head}))

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

                   (unifyc c11-comp-subcat-1
                           (let [propernoun (atom :top)
                                 outerspace-ness (atom :top)
                                 head-synsem {:cat :noun
                                              :sem {:it-came-from-outerspace outerspace-ness}
                                              :propernoun propernoun
                                              :modified true}]
                             {:comp {:phrasal false ;; rathole prevention ;; TODO: see if this can be removed.
                                     :synsem {:cat :adjective
                                              :mod head-synsem}}
                              :head {:phrasal false
                                     :synsem {:propernoun propernoun
                                              :modified false}} ;; TODO: document what purpose :modified serves (if any: if none, remove).
                              :rule "nbar"
                              :synsem head-synsem}))

                   (unifyc c10
                           comp-specs-head
                           (let [number-agreement (atom :top)
                                 propernoun (atom :top)]
                             {:rule "noun-phrase1"
                              :aliases (list "np1")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :propernoun propernoun
                                       :sem {:number number-agreement
                                             :mod '()}}
                              :head {:phrasal false
                                     :synsem {:propernoun propernoun}}
                              :comp {:phrasal false}})) ;; rathole prevention ;; TODO: see if this can be removed.

                   (unifyc c10
                           comp-specs-head
                           (let [number-agreement (atom :top)
                                 propernoun (atom :top)]
                             {:rule "noun-phrase2"
                              :aliases (list "np2")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :propernoun propernoun
                                       :sem {:number number-agreement}}
                              :head {:phrasal true
                                     :synsem {:propernoun propernoun}}
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
                           root-is-head
                           {:head {:phrasal false ;; non-auxiliary past: e.g. "he slept"
                                   :synsem {:aux false}}
                            :rule "s-past-nonphrasal-head"
                            :synsem {:infl :past
                                     :cat :verb
                                     :sem {:aspect :perfect
                                           :tense :past}}})

                   (unifyc c10
                           root-is-head
                           {:head {:phrasal true ;; reflexive past: e.g. "he washed himself"
                                   :synsem {:aux false}}
                            :rule "s-past-phrasal-head"
                            :synsem {:infl :past
                                     :cat :verb
                                     :sem {:aspect :perfect
                                           :tense :past}}})
                   
                   (unifyc c10
                           root-is-head-root
                           {:rule "s-conditional-phrasal-head"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :conditional
                                     :cat :verb
                                     :sem {:tense :conditional}}})
                   (unifyc c10
                           root-is-head
                           {:rule "s-conditional-nonphrasal-head"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :conditional
                                     :cat :verb
                                     :sem {:tense :conditional}}})
                   (unifyc c10
                           root-is-head-root
                           {:rule "s-future-phrasal-head"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :future
                                     :cat :verb
                                     :sem {:tense :future}}})
                   (unifyc c10
                           root-is-head
                           {:rule "s-future-nonphrasal-head"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :future
                                     :cat :verb
                                     :sem {:tense :future}}})
                   (unifyc c10
                           root-is-head-root
                           {:rule "s-imperfect-phrasal-head"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :imperfect
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :past}}})
                   (unifyc c10
                           root-is-head
                           {:rule "s-imperfect-nonphrasal-head"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :imperfect
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :past}}})

                   (unifyc c10
                           root-is-head-root
                           {:rule "s-present-phrasal-head"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :present
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :present}}})
                   (unifyc c10
                           root-is-head
                           {:rule "s-present-nonphrasal-head"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :present
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :present}}})
                   (unifyc h21
                           {:rule "vp-infinitive"
                            :synsem {:aux false
                                     :infl :infinitive
                                     :cat :verb}})

                   (unifyc h21
                           root-is-comp
                           {:rule "vp-aux"
                            :head {:phrasal false}
                            :synsem {:aux true
                                     :infl :present
                                     :sem {:aspect :perfect
                                           :tense :past}
                                     :cat :verb}})

                   ;; this rule is kind of complicated and made more so by
                   ;; dependence on auxilary sense of "avere" which supplies the
                   ;; obj-agr agreement between the object and the main (non-auxilary) verb.
                   (unifyc h22
                           root-is-comp
                           (let [obj-agr (atom :top)]
                             {:head {:phrasal false}
                              :rule "vp-aux-22"
                              :synsem {:aux true
                                       :cat :verb
                                       :infl :present
                                       :sem {:tense :past}
                                       :subcat {:2 {:agr obj-agr}}}}))
                   (unifyc h21
                           {:rule "vp-future"
                            :synsem {:aux false
                                     :infl :future
                                     :cat :verb}})
                   (unifyc h21
                          {:rule "vp-imperfect"
                           :synsem {:aux false
                                    :infl :imperfect
                                    :cat :verb}})
                   (unifyc h21
                           root-is-head
                           {:rule "vp-present"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :present
                                     :sem {:tense :present}
                                     :cat :verb}})
                   (unifyc h21
                           root-is-head
                           {:comp {:phrasal false
                                   :synsem {:cat :noun
                                            :pronoun true}}
                            :rule "vp-pronoun"
                            :synsem {:aux false
                                     :cat :verb}})

                   (unifyc h10
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

(def small
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
              (for [[k v] lexicon]
                (let [filtered-v
                      (filter #(or (= (get-in % [:synsem :cat]) :verb)
                                   (= (get-in % [:synsem :propernoun]) true)
                                   (= (get-in % [:synsem :pronoun]) true))
                              v)]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        ]
    {:name "small"
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "en"
     :morph fo
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
                       [k filtered-v]))))}
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))

(def small-plus-vp-pronoun
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
              (for [[k v] lexicon]
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
                       [k filtered-v]))))}
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))

(def small-plus-plus-np
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
              (for [[k v] lexicon]
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
     :grammar grammar
     :lookup (fn [arg]
               (analyze arg lexicon))

     ;; Will throw exception if more than 1 rule has the same :rule value:
     :grammar-map (zipmap
                   (map #(keyword (get-in % [:rule])) grammar)
                   grammar)

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
                       [k filtered-v]))))}
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))

(def medium
  (let [lexicon
        (into {}
              (for [[k v] lexicon]
                (let [filtered-v v]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))]
    {:name "medium"

     ;; Will throw a clojure/core-level exception if more than 1 rule has the same :rule value:
     :grammar-map (zipmap
                   (map #(keyword (get-in % [:rule])) grammar)
                   grammar)

     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :lookup (fn [arg]
               (analyze arg lexicon))
     :grammar grammar
     :morph fo
     :lexicon lexicon
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))

(def np-grammar
  (let [grammar
        (filter #(or (= (:rule %) "noun-phrase2")
                     (= (:rule %) "nbar"))
                grammar)
        lexicon
        (into {}
              (for [[k v] lexicon]
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
     :lookup (fn [arg]
               (analyze arg lexicon))
     :enrich enrich
     :grammar grammar
     :lexicon lexicon
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))

(log/info "English grammar defined (small,small-plus-vp-pronoun,medium).")
