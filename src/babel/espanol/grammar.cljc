(ns babel.espanol.grammar
  (:refer-clojure :exclude [get-in resolve])
  (:require 
   [babel.espanol.lexicon :refer [deliver-lexicon]]
   [babel.espanol.morphology :as morph
    :refer [analyze fo morph-walk-tree]]
   [babel.index :refer [create-indices lookup-spec]]
   [babel.lexiconfn :refer [lexicon-for-generation]]
   [babel.parse :as parse]
   [babel.stringutils :refer [show-as-tree]]
   [babel.ug :refer [comp-modifies-head comp-specs-head head-principle
                     root-is-head
                     root-is-head-root
                     subcat-1-principle subcat-1-1-principle
                     subcat-1-1-principle-comp-subcat-1
                     subcat-2-principle subcat-2-2-principle
                     subcat-5-principle
                     ]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [clojure.core.cache :as cache]
   [dag_unify.core :refer (get-in unifyc)]))

(def index-lexicon-on-paths
  [[:synsem :cat]
   [:synsem :aux]
   [:synsem :sem :pred]])

(def tenses
  {"present" {:synsem {:sem {:tense :present}}}
   "conditional" {:synsem {:sem {:tense :conditional}}}
   "future" {:synsem {:sem {:tense :future}}}
   "imperfect" {:synsem {:sem {:aspect :progressive
                                :tense :past}}}
   "preterito" {:synsem {:sem {:aspect :perfect
                               :tense :present}}}})

(defn fo-ps [expr]
  (parse/fo-ps expr fo))

(def head-first
  (let [head-espanol (atom :top)
        comp-espanol (atom :top)
        rule (atom :top)]
    (unifyc
     {:comp {:espanol {:initial false}}
      :head {:espanol {:initial true}}}
     {:head {:espanol head-espanol}
      :comp {:espanol comp-espanol}
      :rule rule
      :espanol {:a head-espanol
                :rule rule
                :b comp-espanol}})))
(def head-last
  (let [head-espanol (atom :top)
        comp-espanol (atom :top)
        rule (atom :top)]
    (unifyc
     {:comp {:espanol {:initial true}}
      :head {:espanol {:initial false}}}
     {:head {:espanol head-espanol}
      :rule rule
      :comp {:espanol comp-espanol}
      :espanol {:a comp-espanol
                :rule rule
                :b head-espanol}})))

;; -- BEGIN SCHEMA DEFINITIONS
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
                    :pronoun true}}
    :schema-symbol 'c21 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comment "c21"}))

(def hc-agreement
  (let [agr (atom :top)]
    {:synsem {:agr agr}
     :head {:synsem {:agr agr}}
     :comp {:espanol {:agr agr}
            :synsem {:agr agr}}}))
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

;; -- END SCHEMA DEFINITIONS

(def grammar (list (unifyc h21
                           {:rule "adjective-phrase"
                            :synsem {:cat :adjective}})

                   (unifyc h21
                           (let [head-synsem {:cat :intensifier
                                              ;; TODO: document what purpose :modified
                                              ;; serves (if any: if none, remove).
                                              :modified true}] 
                             {:rule "intensifier-phrase"
                              :synsem head-synsem}))

                   (unifyc h11-comp-subcat-1
                           (let [head-synsem {:cat :noun
                                              :modified true}]
                             ;; rathole prevention ;; TODO: see if this can be removed.
                             {:comp {:phrasal false 
                                     :synsem {:cat :adjective
                                              :mod head-synsem}}
                              :head {:phrasal false
                                     ;; TODO: document what purpose :modified serves
                                     ;; (if any: if none, remove).
                                     :synsem {:modified false}}
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
                              ;; rathole prevention ;; TODO: see if this can be removed.
                              :comp {:phrasal false}}))

                   (unifyc c10
                           comp-specs-head
                           (let [number-agreement (atom :top)]
                             {:rule "noun-phrase2"
                              :aliases (list "np2")
                              :synsem {:agr {:number number-agreement}
                                       :cat :noun
                                       :sem {:number number-agreement}}
                              :head {:phrasal true}
                              ;; rathole prevention ;; TODO: see if this can be removed.
                              :comp {:phrasal false}}))

                   (unifyc h10
                           {:rule "prepositional-phrase"
                            :synsem {:cat :prep}})

                   (unifyc c10
                           root-is-head-root
                           ;; only a vp-aux may be the head child,
                           ;; not simply a lexical auxiliary verb.
                           {:head {:phrasal true
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
                           {:rule "s-preterito-phrasal"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :preterito
                                     :cat :verb
                                     :sem {:aspect :perfect
                                           :tense :present}}})
                   (unifyc c10
                           root-is-head
                           {:rule "s-preterito-nonphrasal"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl :preterito
                                     :cat :verb
                                     :sem {:aspect :perfect
                                           :tense :present}}})
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
                           root-is-head-root
                           {:rule "s-present-phrasal"
                            :head {:phrasal true}
                            :synsem {:aux false
                                     :infl :present
                                     :cat :verb
                                     :sem {:aspect :progressive
                                           :tense :present}}})
                   (unifyc c10
                           root-is-head
                           {:rule "s-present-nonphrasal"
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
                           {:rule "vp-aux"
                            :head {:phrasal false}
                            :synsem {:aux true
                                     :infl :present
                                     :sem {:tense :past}
                                     :cat :verb}})

                   ;; this rule is kind of complicated and made more so by
                   ;; dependence on auxilary sense of "avere" which supplies the
                   ;; obj-agr agreement between the object and the main (non-auxilary) verb.
                   (unifyc h22
                           (let [obj-agr (atom :top)]
                             {:head {:phrasal false}
                              :rule "vp-aux-22"
                              :synsem {:aux true
                                       :cat :verb
                                       :infl :present
                                       :sem {:tense :past}
                                       :subcat {:2 {:agr obj-agr}}}
                              :espanol {:b {:obj-agr obj-agr}}}))
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
                           {:rule "vp-past"
                            :synsem {:aux false
                                     :infl :past
                                     :cat :verb}})
                   (unifyc h21
                           {:rule "vp-present"
                            :synsem {:aux false
                                     :infl :present
                                     :sem {:tense :present}
                                     :cat :verb}})

                   ;; e.g. used as: "tú te llamas Juan" -
                   ;; [s-present-phrasal 'tú' [vp-pronoun-phrasal 'te' [vp32 'llamarse' 'Juan']]]
                   (unifyc h32
                           root-is-head
                           {:rule "vp-32"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl {:not :past}
                                     :cat :verb}})
                   (unifyc c21
                           root-is-head
                           {:head {:phrasal false}
                            :comp {:phrasal false
                                   :synsem {:cat :noun
                                            :pronoun true}}
                            :rule "vp-pronoun-nonphrasal"
                            :synsem {:cat :verb
                                     :infl {:not :past}}})
                   (unifyc c21
                           root-is-head-root
                           {:head {:phrasal true}
                            :comp {:phrasal false
                                   :synsem {:cat :noun
                                            :pronoun true}}
                            :rule "vp-pronoun-phrasal"
                            :synsem {:cat :verb
                                     :infl {:not :past}}})
                   (unifyc h10
                           {:head {:phrasal false
                                   :synsem {:cat :sent-modifier}}
                            :rule "s-modifier"})
))

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

(def grammar
  (map (fn [phrase]
         (modal-is-head-feature
          (aux-is-head-feature phrase)))
       grammar))

(defn small []
  (log/info (str "Español model: small"))
  (let [grammar
        (filter #(or (= (get-in % [:rule]) "s-conditional-nonphrasal")
                     (= (get-in % [:rule]) "s-conditional-phrasal")
                     (= (get-in % [:rule]) "s-present-nonphrasal")
                     (= (get-in % [:rule]) "s-present-phrasal")
                     (= (get-in % [:rule]) "s-future-nonphrasal")
                     (= (get-in % [:rule]) "s-future-phrasal")
                     (= (get-in % [:rule]) "s-imperfect-nonphrasal")
                     (= (get-in % [:rule]) "s-imperfect-phrasal")
                     (= (get-in % [:rule]) "s-preterito-nonphrasal")
                     (= (get-in % [:rule]) "s-preterito-phrasal")
                     (= (get-in % [:rule]) "s-aux")
                     (= (get-in % [:rule]) "vp-pronoun-nonphrasal")
                     (= (get-in % [:rule]) "vp-pronoun-phrasal")
                     (= (get-in % [:rule]) "vp-32")
                     (= (get-in % [:rule]) "vp-aux"))
                grammar)
        lexicon (deliver-lexicon)
        lexicon-for-analysis lexicon
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
        lexicon-for-generation (lexicon-for-generation lexicon)
        indices (create-indices lexicon-for-generation index-lexicon-on-paths)]
    {:name "small"
     :index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))
     :language "es"
     :language-keyword :espanol
     :lookup (fn [arg]
               (morph/analyze arg lexicon-for-analysis))
     
     ;; Will throw exception if more than 1 rule has the same :rule value:
     :grammar-map (zipmap
                   (map #(keyword (get-in % [:rule]))
                        grammar)
                   grammar)
     :generate {:lexicon lexicon-for-generation} ;; filter out null subjects.
     :grammar grammar
     :lexicon lexicon
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :morph fo
     :morph-ps fo-ps
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))}))
(defn medium []
  (log/info (str "Español model: medium"))
  (let [lexicon (deliver-lexicon)
        lexicon-for-generation
        (into {}
              (for [[k v] lexicon]
                (let [filtered-v v]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))
        lexicon-for-analysis lexicon
        indices (create-indices lexicon index-lexicon-on-paths)]
    {:name "medium"
     :generate {:lexicon lexicon-for-generation} ;; filter out null subjects.
     :index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))
     :lookup (fn [arg]
               (morph/analyze arg lexicon-for-analysis))
     :morph fo
     :morph-ps fo-ps
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :grammar grammar
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lexicon lexicon
     }))

(defn parse [surface]
  (parse/parse surface
               (:lexicon (small))
               (:lookup (small))
               (:grammar (small))))

