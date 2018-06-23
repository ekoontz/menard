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
         :phrasal true
         :synsem {:aux false
                  :cat :verb
                  :subcat []
                  :sem {:pred :top
                        :tense :present
                        :aspect :simple
                        :subj {:pred :top}
                        :obj {:pred :top}}}
         :comp {:phrasal false}
         :head {:phrasal true
                :head {:phrasal false}
                :comp {:phrasal false
                       :synsem {:pronoun true}}}}]
    (repeatedly #(-> spec generate morph time println))))

(defn sentences-with-pronoun-objects-small []
  (let [lexicon (:lexicon model)
        transitive? (fn [lexeme]
                      (and (= (get-in lexeme [:synsem :cat])
                              :verb)
                           (= (get-in lexeme [:synsem :aux] false)
                              false)
                           (= (get-in lexeme [:synsem :infl] :top)
                              :top)
                           (not (empty? (get-in lexeme [:synsem :subcat] [])))
                           (map? (get-in lexeme [:synsem :subcat :2]))
                           (empty? (get-in lexeme [:synsem :subcat :3] []))))
        
        transitive-verbs
        (filter (fn [k]
                  (let [lexemes (->> (get lexicon k)
                                     (filter transitive?))]
                    (not (empty? lexemes))))
                (keys lexicon))

        pronoun? (fn [lexeme]
                   (and (= (get-in lexeme [:synsem :cat])
                           :noun)
                        (= (get-in lexeme [:synsem :pronoun])
                           true)))
        spec
        {:modified false
         :phrasal true
         :synsem {:aux false
                  :cat :verb
                  :subcat []
                  :sem {:pred :top
                        :tense :present
                        :aspect :simple
                        :subj {:pred :top}
                        :obj {:pred :top}}}
         :comp {:phrasal false}
         :head {:comp {:phrasal false
                       :synsem {:pronoun true}}}}]
    (repeatedly (fn []
                  (let [chosen-subset (set (take 10 (shuffle transitive-verbs)))
                        debug (println (str "chosen:" (first chosen-subset)))
                        model
                        (model-plus-lexicon
                         ;; filtered lexicon: all pronouns and a small subset of transitive verbs.
                         (into {}
                               (for [[k lexemes] lexicon]
                                 (let [filtered-lexemes
                                       (filter (fn [lexeme]
                                                 (or (and
                                                      (contains? chosen-subset k)
                                                      (transitive? lexeme))
                                                     (pronoun? lexeme)))
                                               lexemes)]
                                   (if (not (empty? filtered-lexemes))
                                     [k filtered-lexemes])))))]
                    (-> spec (generate model) morph time println))))))
