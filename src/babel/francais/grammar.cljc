(ns babel.francais.grammar
  (:refer-clojure :exclude [get-in resolve])
  (:require 
   [clojure.set :refer [union]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [babel.francais.lexicon :refer [lexicon]]
   [babel.francais.morphology :as morph :refer [fo]]
   [babel.index :refer (build-lex-sch-index create-index spec-to-phrases)]
   [babel.parse :as parse]
   [babel.ug :refer [comp-modifies-head comp-specs-head head-principle
                     root-is-comp root-is-head-root root-is-head
                     subcat-1-principle subcat-1-1-principle
                     subcat-1-1-principle-comp-subcat-1 subcat-2-principle
                     subcat-2-2-principle subcat-5-principle]]
   [clojure.core.cache :as cache]
   [dag_unify.core :refer (fail? get-in strip-refs unify unifyc)]))
(declare against-pred)
(declare matching-head-lexemes)

(defn enrich [spec lexicon]
  (against-pred spec lexicon))

(defn against-pred [spec lexicon]
  (let [pred (get-in spec [:synsem :sem :pred] :top)]
    (if (= :top pred)
      spec
      (mapcat
       (fn [lexeme]
         (let [result
               (unify spec
                      {:synsem {:sem (strip-refs (get-in lexeme [:synsem :sem] :top))}})]
           (if (not (fail? result))
             (do
               (log/debug (str "matched head lexeme: " (strip-refs lexeme)))
               (list result)))))
       (matching-head-lexemes spec lexicon)))))

(defn matching-head-lexemes [spec lexicon]
  (let [pred-of-head (get-in spec [:synsem :sem :pred] :top)]
    (if (= pred-of-head :top)
      spec
      (mapcat (fn [lexemes]
                (mapcat (fn [lexeme]
                          (if (= pred-of-head
                                 (get-in lexeme [:synsem :sem :pred] :top))
                            (list lexeme)))
                        lexemes))
              (vals lexicon)))))

(def head-first
  (let [head-french (atom :top)
        comp-french (atom :top)]
    (unifyc
     {:comp {:français {:initial false}}
      :head {:français {:initial true}}}
     {:head {:français head-french}
      :comp {:français comp-french}
      :français {:a head-french
                 :b comp-french}})))
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

(def hc-agreement
  (let [agr (atom :top)]
    {:synsem {:agr agr}
     :head {:synsem {:agr agr}}
     :comp {:français {:agr agr}
            :synsem {:agr agr}}}))

(def head-first
  (let [head-français (atom :top)
        comp-français (atom :top)]
    (unifyc
     {:comp {:français {:initial false}}
      :head {:français {:initial true}}}
     {:head {:français head-français}
      :comp {:français comp-français}
      :français {:a head-français
                :b comp-français}})))

(def head-last
  (let [head-français (atom :top)
        comp-français (atom :top)]
    (unifyc
     {:comp {:français {:initial true}}
      :head {:français {:initial false}}}
     {:head {:français head-français}
      :comp {:français comp-français}
      :français {:a comp-français
                :b head-français}})))

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
    ;; TODO: using :schema-symbol below - cannot use
    ;; :schema for some reason; need to figure out why.
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
                                              :modified true}]
                             ;; TODO: document what purpose
                             ;; :modified serves (if any: if none, remove).
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
                                     ;; TODO: document what purpose :modified serves,
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
                   (unifyc c21
                           root-is-head-root
                           {:head {:phrasal true
                                   :infl {:not :past-p}}
                            :comp {:synsem {:cat :noun
                                            :pronoun true}}
                            :rule "vp-pronoun-phrasal"
                            :synsem {:cat :verb}})

                   (unifyc h21
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

                   ;; [nous [être + naître]] => nous somme nées
                   (unifyc h22
                           root-is-comp
                           (let [obj-agr (atom :top)] ;; TODO: Remove: this obj-agr is not used.
                             {:head {:phrasal false}
                              :rule "vp-aux-22"
                              :synsem {:aux true
                                       :cat :verb
                                       :infl :present
                                       :sem {:tense :past}}}))
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
                                     :infl :past-p
                                     :cat :verb}})
                   (unifyc h21
                           {:rule "vp-present"
                            :synsem {:aux false
                                     :infl :present
                                     :sem {:tense :present}
                                     :cat :verb}})


                   ;; e.g. used as: "je m'appelle Jean" -
                   ;; [s-present-phrasal 'je' [vp-pronoun-phrasal 'm'' [vp32 'se appeler' 'Jean']]]
                   (unifyc h32
                           root-is-head
                           {:rule "vp-32"
                            :head {:phrasal false}
                            :synsem {:aux false
                                     :infl {:not :past}
                                     :cat :verb}})

                   ;; [s-present "je" [vp-pronoun "m'" "amuse"]]
                   (unifyc c21
                           root-is-head
                           {:head {:phrasal false
                                   :synsem {:infl {:not :past-p}}}
                            :comp {:phrasal false
                                   :synsem {:cat :noun
                                            :pronoun true}}
                            :rule "vp-pronoun-nonphrasal"
                            :synsem {:aux false
                                     :cat :verb}})
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

(defn morph-walk-tree [tree]
  (log/debug (str "morph-walk-tree: " (fo tree)))
  (merge
   {:surface (fo (get-in tree [:français]))}
   (if (get-in tree [:comp])
     {:comp (morph-walk-tree (get-in tree [:comp]))}
     {})
   (if (get-in tree [:head])
     {:head (morph-walk-tree (get-in tree [:head]))})))

(def small
  (let [grammar
        (filter #(or (= (:rule %) "s-conditional-nonphrasal")
                     (= (:rule %) "s-present-nonphrasal")
                     (= (:rule %) "s-future-nonphrasal")
                     (= (:rule %) "s-imperfect-nonphrasal")
                     (= (:rule %) "s-aux")
                     (= (:rule %) "vp-aux"))
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
     :language "fr"
     :language-keyword :français
     :lookup (fn [arg]
               (morph/analyze arg lexicon))
     :enrich enrich
     :grammar grammar
     ;; Will throw exception if more than 1 rule has the same :rule value:
     :grammar-map (zipmap
                   (map #(keyword (get-in % [:rule]))
                        grammar)
                   grammar)
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lexicon lexicon
     :morph fo
     :index (create-index grammar (flatten (vals lexicon)) head-principle)}))

(defn analyze [arg]
  (morph/analyze arg lexicon))

(def medium
  (let [lexicon
        (into {}
              (for [[k v] lexicon]
                (let [filtered-v v]
                  (if (not (empty? filtered-v))
                    [k filtered-v]))))

        grammar ;; small grammar + a few other things:
        (seq (union (set (:grammar small))
                    (set (filter #(or (= (:rule %) "vp-pronoun-nonphrasal")
                                      (= (:rule %) "vp-pronoun-phrasal")
                                      (= (:rule %) "s-conditional-phrasal")
                                      (= (:rule %) "s-present-phrasal")
                                      (= (:rule %) "s-future-phrasal")
                                      (= (:rule %) "vp-aux-22")
                                      (= (:rule %) "vp-32"))
                                 grammar))))]
    {:name "medium"
     :enrich enrich
     :grammar grammar
     ;; Will throw exception if more than 1 rule has the same :rule value:
     :grammar-map (zipmap
                   (map #(keyword (get-in % [:rule]))
                        grammar)
                   grammar)
     :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
     :lexicon lexicon
     :index (create-index grammar (flatten (vals lexicon)) head-principle)
     :morph-walk-tree (fn [tree]
                        (do
                          (merge tree
                                 (morph-walk-tree tree))))
     :language "fr"
     :language-keyword :français
     :lookup (fn [arg]
               (morph/analyze arg lexicon))
     :morph fo
     }))

(defn parse [surface]
  (parse/parse surface
               (:lexicon medium)
               (:lookup medium)
               (:grammar medium)))

(log/info "Français grammar defined (small, medium).")
