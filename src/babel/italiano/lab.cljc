(ns babel.italiano.lab
  (:require
   [babel.directory] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [frontier generate]]
   [babel.index :refer [create-indices lookup-spec]]
   [babel.italiano :as italiano :refer [model morph morph-ps]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [dag_unify.core :as u :refer [pprint strip-refs unify]]))

;; [H C]
;;
;;   H
;;  / \
;; H   C
;;
(def tree-1
  {:phrasal true
   :head {:phrasal false}
   :comp {:phrasal false}})

;; [[H C] C]
;;
;;     H
;;    / \
;;   H   C
;;  / \
;; H   C
;;
(def tree-2
  {:phrasal true
   :head tree-1
   :comp {:phrasal false}})

;; [H [H C]]
;;
;;    H
;;   / \
;;  H   C
;;     / \
;;    H   C
;;
(def tree-3
  {:phrasal true
   :head {:phrasal false}
   :comp tree-1})

;; [[H C] [H C]]
;;
;;      H
;;    /   \
;;   H     C
;;  / \   / \
;; H   C H   C
;;
(def tree-4
  {:phrasal true
   :head tree-1
   :comp tree-1})

;; [[H C] C]
;;
;;     H
;;    / \
;;   H   C
;;  / \
;; H   C
(def tree-5
  {:phrasal true
   :head tree-1
   :comp {:phrasal false}})

;; [[H C] [H [H C]]]
;;
;;      H
;;    /    \
;;   H      C
;;  / \    / \
;; H   C  H   C
;;       / \
;;      H   C
;;
(def tree-6
  {:phrasal true
   :head tree-1
   :comp tree-5})


;;      ___H__
;;     /      \
;;    C        H
;;   / \  
;;  C   H 
;;        
;;        
;;
(def tree-7
  {:phrasal true
   :comp tree-1
   :head {:phrasal false}})


(def tree-8
  ;;      ___.__
  ;;     /      \
  ;;    C        H
  ;;   / \      / \
  ;;  C   H    H   C
  ;;          / \
  ;;         H   C
  ;;
  {:phrasal true
   :comp tree-1
   :head {:phrasal true
          :head {:phrasal true
                 :comp {:phrasal false}
                 :head {:phrasal false}}}})

(def tree-9
  ;;      ___.__
  ;;     /      \
  ;;    C        H
  ;;   / \      / \
  ;;  C   H    H   C
  ;;              / \  
  ;;             H   C
  ;;
  {:phrasal true
   :comp tree-1
   :head {:phrasal true
          :head {:phrasal false}
          :comp {:phrasal true
                 :head {:phrasal false}
                 :comp {:phrasal false}}}})

(def object-is-pronoun {:head {:comp {:synsem {:pronoun true}}}})

(def basic
  {:modified false
   :synsem {:cat :verb
            :subcat []}})

(def specs 
  [
   (unify tree-1 basic)
   (unify tree-2 basic)
   (unify tree-3 basic)
   (unify tree-4 basic)
   (unify tree-5 basic)
   (unify tree-6 basic {:synsem {:sem {:tense :present
                                       :aspect :perfect}}})
   (unify tree-7 basic)
   (unify tree-8 basic)
   (unify tree-9 basic)
   ])

(def vedere-specs
  (map #(unify % {:synsem {:essere false
                           :subcat []
                           :cat :verb}
                  :root {:italiano {:italiano "vedere"}}})
       specs))

(defn park []
  (let [vedere-specs
        [{:synsem {:subcat (), :cat :verb, :essere false},
          :modified false,
          :phrasal true,
          :head {:phrasal false},
          :comp {:phrasal false},
          :root {:italiano {:italiano "vedere"}}}]]
    (repeatedly
     #(println (morph (time
                       (generate
                        (unify 
                         (nth vedere-specs (rand-int (count vedere-specs)))
                         {:head {:phrasal false}
                          :comp {:phrasal false}})
                        model)))))))

(defn downtown []
  (let [spec
        {:synsem {:cat :verb
                  :subcat []}
         :modified false}]
    (repeatedly #(println (morph (time (generate spec model)))))))

(defn basecamp []
  (let [semantic-spec
        {:modified false,
         :synsem {:cat :verb, :subcat []
                  :sem {:aspect :simple
                        :pred :be-called
                        :tense :present}}}
        root-spec
        {:modified false,
         :root {:italiano {:italiano "chiamarsi"}}
         :synsem {:cat :verb, :subcat []
                  :sem {:aspect :simple
                        :tense :present}}}

        all-of-the-specs (concat specs [root-spec semantic-spec]
                                 vedere-specs)]
    
    (repeatedly #(println
                  (morph-ps (time (generate
                                   (nth all-of-the-specs
                                        (rand-int (count all-of-the-specs)))
                                   model)))))))
(defn nextcamp []
  (basecamp))


(defn refresh [& [refresh-lexicon?]]
  (babel.test.test/init-db)
  (if refresh-lexicon? (println (str "wrote: "
                                     (babel.lexiconfn/write-lexicon "it" (babel.italiano.lexicon/compile-lexicon))
                                     " items.")))
  (babel.directory/refresh-models)
  (load "../italiano/grammar")
  (load "../italiano"))

;;(map #(println (morph %)) (grow (sprouts spec model) model))
(defn get-rule [rule]
  (-> model :grammar-map (get rule)))

(defn parse-at [expression path]
  (map #(u/get-in % path ::none)
       (mapcat :parses (parse expression))))

(defn lexeme-at [lexeme path]
  (map #(u/get-in % path ::none)
       (-> model :lexicon (get lexeme))))

(defn generate-at [spec path]
  (let [generated (generate spec model)]
    (u/get-in generated path ::none)))

(def basic-grammar
  #{"sentence-nonphrasal-head"})

(def present-grammar
  #{"sentence-phrasal-head"
    "vp-pronoun-phrasal"
    "vp-pronoun-nonphrasal"})

(def chiamarsi-grammar
  #{"s-aux"
    "sentence-phrasal-head"
    "vp-aux-phrasal-complement"
    "vp-pronoun-phrasal"
    "vp-32"})

(def future-grammar
  #{"sentence-phrasal-head"
    "vp-pronoun-phrasal"
    "vp-pronoun-nonphrasal"})

(def present-perfect-grammar
  #{"s-aux"
    "vp-32"
    "vp-aux-22-nonphrasal-comp"
    "vp-aux-22-phrasal-comp"
    "vp-aux-nonphrasal-complement"
    "vp-aux-phrasal-complement"
    "vp-pronoun-phrasal"
    "vp-pronoun-nonphrasal"})

(defn create-index-fn [verb-set grammar]
  (let [verbcoach-pronouns
        (fn [v]
          (or
           (and (= :noun (u/get-in v [:synsem :cat]))
                (not (= :acc (u/get-in v [:synsem :case]))))
           (and (= :noun (u/get-in v [:synsem :cat]))
                (= true (u/get-in v [:synsem :propernoun])))
           (and (= :noun (u/get-in v [:synsem :cat]))
                (= :acc (u/get-in v [:synsem :case]))
                (= true (u/get-in v [:synsem :reflexive])))))

        verbcoach-index-paths
        [[:italiano :italiano]
         [:synsem :aux]
         [:synsem :cat]
         [:synsem :essere]
         [:synsem :infl]
         [:synsem :sem :pred]]

        lexicon
        (into {}
              (for [[k vals] (:lexicon model)]
                (let [filtered-vals
                      (filter
                       (fn [v]
                         (or 
                          (and (= :verb (u/get-in v [:synsem :cat]))
                               (or (= true (u/get-in v [:synsem :aux]))
                                   (contains? verb-set
                                              (u/get-in v [:italiano :italiano]))))
                          (verbcoach-pronouns v)))
                       vals)]
                  (if (not (empty? filtered-vals))
                    [k filtered-vals]))))
        indices (create-indices lexicon verbcoach-index-paths)]
    (fn [spec]
      (lookup-spec spec indices verbcoach-index-paths))))

;; this is a lot like a lexical compilation default map.
(def rule-matcher
  {:top
   basic-grammar
   
   {:root {:italiano {:italiano "chiamarsi"}}}
   chiamarsi-grammar

   {:synsem {:sem {:tense :present
                   :aspect :perfect}}}
   present-perfect-grammar

   {:synsem {:sem {:tense :future}}}
   future-grammar

   {:synsem {:sem {:tense :conditional}}}
   future-grammar

   {:synsem {:sem {:tense :past
                   :aspect :progressive}}}
   future-grammar
   
   {:synsem {:sem {:tense :present
                   :aspect :simple}}}
   present-grammar})


(defn rule-matcher-reducer [input-spec]
  (reduce
   clojure.set/union
   (map (fn [key-in-rule-matcher]
          (let [result (unify key-in-rule-matcher input-spec)]
            (if (= :fail result)
              nil
              (get rule-matcher key-in-rule-matcher))))
        (keys rule-matcher))))

;; (take 10 (generate-for-verbcoach))
(defn generate-for-verbcoach
  "generate sentences efficiently given specific constraints."
  []
  (let [verb-set #{"arrabbiarsi" "chiamarsi" "fermarsi" "parlare" "sedersi"}
        specs
        (map (fn [root]
               {:root {:italiano {:italiano root}}})
             verb-set)
        tense-specs [
                     {:synsem {:cat :verb
                               :sem {:tense :present
                                     :aspect :perfect}
                               :subcat []}}
                     {:synsem {:cat :verb
                               :sem {:tense :future}
                               :subcat []}}
                     {:synsem {:cat :verb
                               :sem {:tense :conditional}
                               :subcat []}}
                     {:synsem {:cat :verb
                               :sem {:tense :past
                                     :aspect :progressive}
                               :subcat []}}
                     {:synsem {:cat :verb
                               :sem {:tense :present
                                     :aspect :simple}}}
                     ]
        ]
    
    (repeatedly
     #(do
        (println
         (let [chosen-spec
               (unify (first (shuffle specs))
                      (first (shuffle tense-specs)))]
           (let [generate-with-grammar-set
                 (rule-matcher-reducer chosen-spec)
                 grammar
                 (filter (fn [rule-structure]
                           (contains? generate-with-grammar-set
                                      (u/get-in rule-structure [:rule])))
                         (:grammar model))]
             (morph (binding [babel.generate/println? false
                              babel.generate/truncate? false
                              babel.generate/index-fn (create-index-fn verb-set grammar)
                              babel.generate/lexical-filter
                              (fn [lexeme] (= false (u/get-in lexeme [:italiano :exception] false)))
                              babel.generate/grammar grammar]
                      (time (generate chosen-spec model)))))))))))
