(ns babel.italiano.lab
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.directory :refer []]
   [babel.italiano :as italiano :refer [analyze generate model morph morph-ps parse]]
   [babel.italiano.grammar :refer [model-plus-lexicon]]
   [babel.test.test :as btest]
   #?(:cljs [babel.logjs :as log])
   [clojure.pprint :refer [pprint]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [clojure.set :as set]
   [dag_unify.core :refer [get-in strip-refs]]))

(defn generate-speed-test [spec & [times]]
  (btest/generate-speed-test spec model times))

(defn run-passato-prossimo-test []
  (generate-speed-test {:synsem {:cat :verb :subcat []
                                 :sem {:reflexive true
                                       :tense :present
                                       :aspect :perfect}}}))
;; roundtrip parser testing
(defn roundtrip-parsing [n]
  (take n
        (repeatedly #(let [generated
                           (morph (generate {:synsem {:cat :verb
                                                      :subcat []}}))
                           parsed (reduce concat (map :parses (parse generated)))]
                       (log/info (str "generated: " generated))
                       (log/info (str "semantics: "
                                      (or
                                       (strip-refs
                                        (get-in (first parsed)
                                                [:synsem :sem]))
                                       (str "NO PARSE FOUND FOR: " generated))))
                       {:generated generated
                        :pred (get-in (first parsed) [:synsem :sem :pred])
                        :subj (get-in (first parsed) [:synsem :sem :subj :pred])}))))

(defn tiny-model []
  (model-plus-lexicon
   ;; filtered lexicon:
   (into {}
         (for [[k lexemes] (:lexicon model)]
           (let [filtered-lexemes
                 (filter (fn [lexeme]
                           (or (= k "vedere")
                               (and (= (get-in lexeme [:synsem :cat])
                                       :noun)
                                    (= (get-in lexeme [:synsem :pronoun])
                                       true))))
                         lexemes)]
             (if (not (empty? filtered-lexemes))
               [k filtered-lexemes]))))

   ;; filtered grammar:
   (fn [rule]
     (or (= (:rule rule)
            "s-present-phrasal")
         (= (:rule rule)
            "vp-pronoun-nonphrasal")))))

(defn sentences-with-pronoun-objects []
  (let [spec
        {:modified false
         :synsem {:cat :verb
                  :subcat []
                  :sem {:pred :top
                        :tense :present
                        :aspect :simple
                        :subj {:pred :top}
                        :obj {:pred :top}}}
         :head {:comp {:synsem {:pronoun true}}}}]
    (repeatedly #(-> spec generate morph time println))))

(defn sentences-with-pronoun-objects-small []
  (let [lexicon (:lexicon model)
        transitive-verbs
        (filter (fn [k]
                  (let [lexemes (->> (get lexicon k)
                                     (filter (fn [lexeme]
                                               (and (= (get-in lexeme [:synsem :cat])
                                                       :verb)
                                                    (= (get-in lexeme [:synsem :aux] false)
                                                       false)
                                                    (not (empty? (get-in lexeme [:synsem :subcat] [])))
                                                    (map? (get-in lexeme [:synsem :subcat :2]))
                                                    (empty? (get-in lexeme [:synsem :subcat :3] []))))))]
                    (not (empty? lexemes))))
                (keys lexicon))

        chosen-subset (set (take 3 (shuffle transitive-verbs)))

        model
        (model-plus-lexicon
         ;; filtered lexicon:
         (into {}
               (for [[k lexemes] lexicon]
                 (let [filtered-lexemes
                       (filter (fn [lexeme]
                                 (or (and

                                      ;; must have the same surface form as the
                                      ;; three randomly-chosen words above.
                                      (contains? chosen-subset k)

                                      ;; must be a transitive verb: exactly 2 arguments (subject and object).
                                      (= (get-in lexeme [:synsem :cat]) :verb)
                                      (not (empty? (get-in lexeme [:synsem :subcat] [])))
                                      (map? (get-in lexeme [:synsem :subcat :2]))
                                      (empty? (get-in lexeme [:synsem :subcat :3] [])))

                                     (and (= (get-in lexeme [:synsem :cat])
                                             :noun)
                                          (= (get-in lexeme [:synsem :pronoun])
                                             true))))
                               lexemes)]
                   (if (not (empty? filtered-lexemes))
                     [k filtered-lexemes]))))
         
         ;; filtered grammar:
         (fn [rule]
           (or (= (:rule rule)
                  "s-present-phrasal")
               (= (:rule rule)
                  "vp-pronoun-nonphrasal"))))
        spec
        {:modified false
         :phrasal true
         :synsem {:cat :verb
                  :subcat []
                  :sem {:pred :top
                        :tense :present
                        :aspect :simple
                        :subj {:pred :top}
                        :obj {:pred :top}}}
         :comp {:phrasal false}
         :head {:comp {:phrasal false
                       :synsem {:pronoun true}}}}]
    (repeatedly #(-> spec (generate model) morph time println))))
  
(defn sentences-with-pronoun-objects []
  (let [spec
        {:modified false
         :synsem {:cat :verb
                  :subcat []
                  :sem {:pred :top
                        :tense :present
                        :aspect :simple
                        :subj {:pred :top}
                        :obj {:pred :top}}}
         :head {:comp {:synsem {:pronoun true}}}}]
    (repeatedly #(-> spec generate morph time println))))

(defn sentences-with-pronoun-objects-hints
  "supply a spec enhanced with syntactic info to speed-up generation."  
  []
  (let [spec
        {:modified false
         :synsem {:cat :verb
                  :subcat []
                  :sem {:pred :top
                        :tense :present
                        :aspect :simple
                        :subj {:pred :top}
                        :obj {:pred :top}}}
         :head {:comp {:synsem {:pronoun true}}}}]
    (repeatedly #(-> spec generate morph time println))))

(defn sentences-with-pronoun-objects-small []
  (let [lexicon (:lexicon model)
        transitive-verbs
        (filter (fn [k]
                  (let [lexemes (->> (get lexicon k)
                                     (filter (fn [lexeme]
                                               (and (= (get-in lexeme [:synsem :cat])
                                                       :verb)
                                                    (= (get-in lexeme [:synsem :aux] false)
                                                       false)
                                                    (not (empty? (get-in lexeme [:synsem :subcat] [])))
                                                    (map? (get-in lexeme [:synsem :subcat :2]))
                                                    (empty? (get-in lexeme [:synsem :subcat :3] []))))))]
                    (not (empty? lexemes))))
                (keys lexicon))

        chosen-subset (set (take 3 (shuffle transitive-verbs)))

        model
        (model-plus-lexicon
         ;; filtered lexicon:
         (into {}
               (for [[k lexemes] lexicon]
                 (let [filtered-lexemes
                       (filter (fn [lexeme]
                                 (or (and

                                      ;; must have the same surface form as the
                                      ;; three randomly-chosen words above.
                                      (contains? chosen-subset k)

                                      ;; must be a transitive verb: exactly 2 arguments (subject and object).
                                      (= (get-in lexeme [:synsem :cat]) :verb)
                                      (not (empty? (get-in lexeme [:synsem :subcat] [])))
                                      (map? (get-in lexeme [:synsem :subcat :2]))
                                      (empty? (get-in lexeme [:synsem :subcat :3] [])))

                                     (and (= (get-in lexeme [:synsem :cat])
                                             :noun)
                                          (= (get-in lexeme [:synsem :pronoun])
                                             true))))
                               lexemes)]
                   (if (not (empty? filtered-lexemes))
                     [k filtered-lexemes]))))
         
         ;; filtered grammar:
         (fn [rule]
           (or (= (:rule rule)
                  "s-present-phrasal")
               (= (:rule rule)
                  "vp-pronoun-nonphrasal"))))
        spec
        {:modified false
         :phrasal true
         :synsem {:cat :verb
                  :subcat []
                  :sem {:pred :top
                        :tense :present
                        :aspect :simple
                        :subj {:pred :top}
                        :obj {:pred :top}}}
         :comp {:phrasal false}
         :head {:comp {:phrasal false
                       :synsem {:pronoun true}}}}]
    (repeatedly #(-> spec (generate model) morph time println))))
