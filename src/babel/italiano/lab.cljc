(ns babel.italiano.lab
  (:require
   [babel.directory] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [frontier generate]]
   [babel.index :refer [create-indices lookup-spec]]
   [babel.italiano :as italiano :refer [model morph morph-ps parse]]
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

(defn with-shrunken-model []
  (binding [babel.generate/truncate? true
            babel.generate/println? false
            babel.generate/index-fn (:index-fn model)
            babel.generate/morph-ps (:morph-ps model)
            babel.generate/grammar
            (filter
             (fn [rule]
               (or
                (= (:rule rule) "s-aux")
                (= (:rule rule) "vp-pronoun-phrasal")
                (= (:rule rule) "vp-aux-22-nonphrasal-comp")))
             (:grammar model))]
    (generate {:modified false
               :root {:italiano {:italiano "radersi"}}
               :synsem {:cat :verb
                        :sem {:aspect :perfect
                              :tense :present}
                        :subcat []}})))

(defn svegliarsi []
  (repeatedly 
   #(println (pprint
              (let [expr 
                    (time (generate {:modified false, 
                                     :synsem {:cat :verb, :subcat []
                                              :sem {:tense :present, :aspect :progressive}}, 
                                     :root {:italiano {:italiano "svegliarsi"}}
                                     :comp {:phrasal false}}
                                    model))]
                {:m (morph expr)
                 :ps (morph-ps expr)})))))

(defn chiamarsi
  "generate chiamarsi sentences with a grammar subset."
  []
  (let [grammar
        (filter
         #(or
           (= "s-present-phrasal" (u/get-in % [:rule]))
           (= "vp-32" (u/get-in % [:rule]))
           (= "vp-pronoun-phrasal" (u/get-in % [:rule])))
         
         (:grammar model))
        spec {:root {:italiano {:italiano "chiamarsi"}}
              :synsem {:cat :verb
                       :sem {:tense :present
                             :aspect :simple}
                       :subcat []}}]
    (repeatedly
     #(println
       (morph (binding [babel.generate/println? false
                        babel.generate/truncate? false
                        babel.generate/grammar grammar]
                (time (generate spec model))))))))



(def verbcoach-grammar
  #{
    ;;    "adjective-phrase"
    ;;    "intensifier-phrase"
    ;;    "nbar1"
    ;;    "nbar2"
    ;;    "noun-phrase1"
    ;;    "noun-phrase2"
    ;;    "np-to-n-pp"
    ;;    "prepositional-phrase"
    ;;    "s-aux"
    ;;    "s-modified-modifier-first"
    ;;    "s-modified-modifier-last"
    ;;    "s-modifier"
    ;;    "sentence-nonphrasal-head"
    "sentence-phrasal-head"
    ;;    "vp"
    ;;    "vp-32"
    "vp-aux-22-nonphrasal-comp"
    "vp-aux-22-phrasal-comp"
    "vp-aux-nonphrasal-complement"
    "vp-aux-phrasal-complement"
    "vp-aux-phrasal-complement-essere-false"
    "vp-infinitive"
    ;;    "vp-past"
    "vp-present"
    "vp-pronoun-nonphrasal"})
;;    "vp-pronoun-phrasal"



(def verbcoach-pronouns
  (fn [v]
    (or 
     (and (= :noun (u/get-in v [:synsem :cat]))
          (not (= :acc (u/get-in v [:synsem :case]))))
     (and (= :noun (u/get-in v [:synsem :cat]))
          (= :acc (u/get-in v [:synsem :case]))
          (= true (u/get-in v [:synsem :reflexive]))))))

(def verbcoach-index-fn
  (let [index-lexicon-on-paths
        [[:italiano :italiano]
         [:synsem :aux]
         [:synsem :cat]
         [:synsem :essere]
         [:synsem :infl]
         [:synsem :sem :pred]]]

    (fn [spec]
      (let [lexicon 
            (into {}
                  (for [[k vals] (:lexicon model)]
                    (let [filtered-vals
                          (filter verbcoach-pronouns
                                  vals)]
                      (if (not (empty? filtered-vals))
                        [k filtered-vals]))))]
        (lookup-spec spec (create-indices lexicon index-lexicon-on-paths) index-lexicon-on-paths)))))

(def
  verbcoach-index-paths
  [[:italiano :italiano]
   [:synsem :aux]
   [:synsem :cat]
   [:synsem :essere]
   [:synsem :infl]
   [:synsem :sem :pred]])

(defn create-index-fn [verb-set grammar]
  (let [lexicon
        (into {}
              (for [[k vals] (:lexicon model)]
                (let [filtered-vals
                      (filter
                       (fn [v]
                         (or 
                          (and (= :verb (u/get-in v [:synsem :cat]))
                               (contains? verb-set
                                          (u/get-in v [:italiano :italiano])))
                          (verbcoach-pronouns v)))
                       vals)]
                  (if (not (empty? filtered-vals))
                    [k filtered-vals]))))
        indices (create-indices lexicon verbcoach-index-paths)]
    (fn [spec]
      (lookup-spec spec indices verbcoach-index-paths))))

(defn arrabbiarsi
  "generate arrabiarsi sentences with a grammar subset."
  []
  (let [verb-set #{"arrabbiarsi" "essere" "fermarsi" "parlare" "sedersi"}
        grammar
        (filter
         #(contains? verbcoach-grammar (u/get-in % [:rule]))
         (:grammar model))
        spec {:root {:italiano {:italiano "arrabbiarsi"}}
              :synsem {:cat :verb
                       :modified false
                       :sem {:tense :past
                             :aspect :progressive}
                       :subcat []}}
        index-fn (create-index-fn verb-set grammar)]
    (repeatedly
     #(println
       (morph (binding [babel.generate/println? false
                        babel.generate/truncate? true
                        babel.generate/grammar grammar
                        babel.generate/index-fn index-fn]
                (time (generate spec model))))))))

(defn arrabbiarsi-or-sedersi
  "generate arrabiarsi sentences with a grammar subset."
  []
  (let [verb-set #{"arrabbiarsi" "essere" "fermarsi" "parlare" "sedersi"}
        grammar
        (filter
         #(contains? verbcoach-grammar (u/get-in % [:rule]))
         (:grammar model))
        specs [{:root {:italiano {:italiano "arrabbiarsi"}}
                :synsem {:cat :verb
                         :modified false
                         :sem {:tense :past
                               :aspect :progressive}
                         :subcat []}}

               {:root {:italiano {:italiano "sedersi"}}
                :synsem {:cat :verb
                         :modified false
                         :sem {:tense :future}
                         :subcat []}}]
        index-fn (create-index-fn verb-set grammar)]
    (repeatedly
     #(println
       (morph (binding [babel.generate/println? false
                        babel.generate/truncate? true
                        babel.generate/grammar grammar
                        babel.generate/index-fn index-fn]
                (time (generate (first (shuffle specs)) model))))))))

(defn stampare
  "generate arrabiarsi sentences with a grammar subset."
  []
  (let [grammar
        (filter
         #(contains? verbcoach-grammar (u/get-in % [:rule]))
         (:grammar model))
        spec {:root {:italiano {:italiano "stampare"}}
              :synsem {:cat :verb
                       :aux false
                       :modified false
                       :sem {:tense :past
                             :aspect :progressive}
                       :subcat []}}]

    (repeatedly
     #(println
       (morph (binding [babel.generate/println? true
                        babel.generate/truncate? false
                        babel.generate/index-fn verbcoach-index-fn
                        babel.generate/grammar grammar]
                (time (generate spec model))))))))

(defn chiamarsi-ps
  "generate chiamarsi sentences with parse trees"
  []
  (repeatedly 
   #(println (pprint
              (let [expr 
                    (time (generate {:modified false, 
                                     :synsem {:cat :verb, :subcat []
                                              :sem {:tense :present,
                                                    :aspect :simple}}, 
                                     :root {:italiano {:italiano "chiamarsi"}}}
                                    model))]
                {:m (morph expr)
                 :ps (morph-ps expr)})))))
