(ns babel.italiano.lab
  (:require
   [babel.directory] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [frontier generate]]
   [babel.index :refer [create-indices lookup-spec]]
   [babel.italiano :as italiano :refer [model morph-ps]]
   [babel.italiano.grammar :as grammar]
   [babel.italiano.morphology :as m]
   #?(:cljs [babel.logjs :as log])
   [clojure.string :as string]
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
        [{:synsem {:subcat []
                   :cat :verb
                   :essere false}
          :modified false
          :phrasal true
          :root {:italiano {:italiano "vedere"}}}]
        tense-specs
        [{:synsem {:sem {:tense :present
                         :aspect :simple}}}
         {:synsem {:sem {:tense :past
                         :aspect :progressive}}}
         {:synsem {:sem {:tense :future}}}
         {:synsem {:sem {:tense :conditional}}}]]
    (repeatedly
     #(println (m/morph (time
                         (generate
                          (unify 
                           (nth vedere-specs (rand-int (count vedere-specs)))
                           (nth tense-specs (rand-int (count tense-specs)))
                           {:head {:phrasal false}
                            :comp {:phrasal false}})
                          model)))))))

(defn downtown []
  (let [spec
        {:synsem {:cat :verb
                  :subcat []}
         :modified false}]
    (repeatedly #(println (m/morph (time (generate spec model)))))))

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
       (mapcat :parses (italiano/parse expression))))

(defn lexeme-at [lexeme path]
  (map #(u/get-in % path ::none)
       (-> model :lexicon (get lexeme))))

(defn generate-at [spec path]
  (let [generated (generate spec model)]
    (u/get-in generated path ::none)))

(defn target-generation [spec index-fn model]
  (let [grammar ((:rule-matcher-reducer model) spec)]
    (binding [babel.generate/println? false
              babel.generate/truncate? false
              babel.generate/index-fn index-fn
              babel.generate/lexical-filter
              (fn [lexeme] (= false (u/get-in lexeme [:italiano :exception] false)))
              babel.generate/grammar grammar]
      (let [structure (time (generate spec model))]
        (if (= :fail structure)
          (throw (Exception. (str "target-generation failed with spec: " spec))))
        structure))))

(def lexicon-indices
  (let [filtered-lexicon
        (into {}
              (for [[k vals] (:lexicon model)]
                (let [filtered-vals
                      (filter
                       #(or (= :verb (u/get-in % [:synsem :cat]))
                            (and (= :noun (u/get-in % [:synsem :cat]))
                                 (not (= :acc (u/get-in % [:synsem :case]))))
                            (and (= :noun (u/get-in % [:synsem :cat]))
                                 (= :acc (u/get-in % [:synsem :case]))
                                 (= true (u/get-in % [:synsem :reflexive]))))
                       vals)]
                  (if (not (empty? filtered-vals))
                    [k filtered-vals]))))]
    (create-indices filtered-lexicon grammar/index-paths)))

(defn verbcoach-lexical-lookup [spec]
  (lookup-spec spec lexicon-indices grammar/index-paths))

;; (take 10 (generate-for-verbcoach))
(defn generate-for-verbcoach 
  "generate sentences efficiently given specific constraints."
  [& [spec]]
  (let [example-verbs #{"arrabbiarsi" "chiamarsi" "dormire" "fermarsi" "parlare" "sedersi"}]
;;        example-verbs #{"arrabbiarsi"}]
;;        example-verbs #{"parlare"}
        
    (repeatedly
     #(do
        (let [spec (or spec :top)
              tense-spec (first (shuffle grammar/tense-specs))
              tense-spec
              (let [result (unify spec tense-spec)]
                (if (not (= :fail result))
                  result
                  spec))
              root-spec
              (let [unif
                    (unify spec
                           {:root {:italiano {:italiano
                                              (first (shuffle example-verbs))}}})]
                (if (not (= :fail unif))
                  unif
                  spec))
              chosen-spec (unify root-spec tense-spec)]
          (let [result
                (target-generation chosen-spec verbcoach-lexical-lookup model)]
            (if (= result :fail)
              (throw (Exception. (str "failed to generate with spec: " (u/strip-refs chosen-spec)))))
            (println (m/morph result))))))))

(defn simple-sentence []
  (generate {:rule "sentence-nonphrasal-head"
             :synsem {:subcat []
                      :cat :verb
                      :sem {:tense :present
                            :aspect :simple
                            :subj {:pred :I}}}
             :root {:italiano {:italiano "dormire"}}}
            model))

(defn furniture-sentence []
  (let [expr (generate
              {:synsem {:cat :verb
                        :sem {:obj {:pred :table
                                    :mod []
                                    :number :sing
                                    :spec {:def :def}}
                              :pred :in-front-of
                              :aspect :simple
                              :tense :present
                              :subj {:pred :chair
                                     :mod []
                                     :number :sing
                                     :spec {:def :def}}}
                        :subcat []}
               :comp {:synsem {:agr {:person :3rd}}}
               :modified false})]
    expr))

(def test-input-1
  {:italiano "dormire"
   :infl :present
   :agr {:person :1st}
   :cat :verb})

(def test-input-2
  {:italiano "dormire"
   :infl :present
   :agr {:person :2nd}
   :cat :verb})

(def test-input-3
  {:a {:initial true, :cat :noun, :italiano "Giovanni e io", :case :nom},
   :b {:italiano "vedere",
       :cat :verb,
       :initial false,
       :infl :conditional,
       :essere false,
       :agr {:number :plur, :person :1st, :gender :masc},
       :passato "visto",
       :future-stem "vedr"}})

(def test-input-4
  {:a {:initial true, :cat :noun, :italiano "Giovanni e io", :case :nom},
   :b {:italiano "vedere",
       :cat :verb,
       :initial false,
       :infl :future
       :essere false,
       :agr {:number :plur, :person :1st, :gender :masc},
       :passato "visto",
       :future-stem "vedr"}})

(def result (m/morph test-input-2))

