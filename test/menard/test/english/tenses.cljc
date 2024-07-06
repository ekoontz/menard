(ns menard.test.english.tenses
  (:require [menard.english :as en :refer [analyze expressions generate morph parse syntax-tree get-grammar get-lexicon]]
            [dag_unify.core :as u :refer [unify]]
            [menard.lexiconfn :as l]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest tense-parsing
  (let [parses (parse "she saw him")]
    (is (seq parses))
    (is (not (contains? (set (->> parses
                                  (map #(unify %
                                               {:sem {:pred :see
                                                      :aspect :simple
                                                      :tense :past
                                                      :subj {:pred :she}
                                                      :obj {:pred :he}}}))))
                        :fail))))
  (let [parses (parse "he will see her")]
    (is (seq parses))
    (is (not (contains? (set (->> parses
                                  (map #(unify %
                                               {:sem {:pred :see
                                                      :tense :future
                                                      :subj {:pred :he}
                                                      :obj {:pred :she}}}))))
                        :fail))))
  (let [parses (parse "she would see them")]
    (is (seq parses))
    (is (not (contains? (set (->> parses
                                  (map #(unify %
                                               {:sem {:pred :see
                                                      :tense :conditional
                                                      :subj {:pred :she}
                                                      :obj {:pred :they}}}))))
                        :fail))))
  (let [parses (parse "they see her")]
    (is (seq parses))
    (is (not (contains? (set (->> parses
                                  (map #(unify %
                                               {:sem {:pred :see
                                                      :tense :present
                                                      :aspect :simple
                                                      :subj {:pred :they}
                                                      :obj {:pred :she}}}))))
                        :fail)))))
