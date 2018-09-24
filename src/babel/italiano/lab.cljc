(ns babel.italiano.lab
  (:require
   [babel.directory] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [frontier generate]]
   [babel.index :refer [create-indices lookup-spec]]
   [babel.italiano :as italiano :refer [model morph morph-ps]]
   [babel.italiano.grammar :as grammar]
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
       (mapcat :parses (italiano/parse expression))))

(defn lexeme-at [lexeme path]
  (map #(u/get-in % path ::none)
       (-> model :lexicon (get lexeme))))

(defn generate-at [spec path]
  (let [generated (generate spec model)]
    (u/get-in generated path ::none)))

;; to be used within babel.reader/generate-question-and-correct-set:
(defn target-generation [spec index-fn model]
  (let [grammar
        (let [generate-with-grammar-set (grammar/rule-matcher-reducer spec)]                   
          (filter (fn [rule-structure]
                    (contains? generate-with-grammar-set
                               (u/get-in rule-structure [:rule])))
                  (:grammar model)))]
    (morph (binding [babel.generate/println? false
                     babel.generate/truncate? false
                     babel.generate/index-fn index-fn
                     babel.generate/lexical-filter
                     (fn [lexeme] (= false (u/get-in lexeme [:italiano :exception] false)))
                     babel.generate/grammar grammar]
             (time (generate spec model))))))

;; (take 10 (generate-for-verbcoach))
(defn generate-for-verbcoach 
  "generate sentences efficiently given specific constraints."
  [& [spec]]
  (let [example-verbs #{"arrabbiarsi" "chiamarsi" "dormire" "fermarsi" "parlare" "sedersi"} 
        create-index-fn
        (fn []
          (let [lexicon
                (into {}
                      (for [[k vals] (:lexicon model)]
                        (let [verbcoach-pronouns
                              (fn [v]
                                (or
                                 (and (= :noun (u/get-in v [:synsem :cat]))
                                      (not (= :acc (u/get-in v [:synsem :case]))))
                                 (and (= :noun (u/get-in v [:synsem :cat]))
                                      (= :acc (u/get-in v [:synsem :case]))
                                      (= true (u/get-in v [:synsem :reflexive])))))
                              filtered-vals
                              (filter
                               #(or (= :verb (u/get-in % [:synsem :cat]))
                                    (verbcoach-pronouns %))
                               vals)]
                          (if (not (empty? filtered-vals))
                            [k filtered-vals]))))
                indices (create-indices lexicon grammar/index-paths)]
            (fn [spec]
              (lookup-spec spec indices grammar/index-paths))))]
    (repeatedly
     #(do
        (println
         (let [tense-spec (first (shuffle grammar/tense-specs))
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
           (target-generation chosen-spec (create-index-fn) model)))))))



