(ns menard.test.english.emojis
  (:require [menard.english :as en :refer [analyze expressions generate morph parse syntax-tree get-grammar get-lexicon]]
            [dag_unify.core :as u :refer [unify]]
            [menard.lexiconfn :as l]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(load "../../../../src/menard/english/complete")

(deftest emoji-parsing
  (is (= '("[s(:present-simple) .[pronoun+emoji +we .👴🏿👵🏼] +[vp +get .up]]")
         (->> "we 👴🏿👵🏼 get up" en/parse (map en/syntax-tree))))
  (is (= '("[s(:present-simple) .[pronoun+emoji +you(2) .👦🧒🏼] +sleep]")
         (->> "you 👦🧒🏼 sleep" en/parse (map en/syntax-tree))))
  (is (= 
       '({:obj :none,
          :subj
          {:prop {:animate true},
           :existential? false,
           :mod [],
           :locative? false,
           :ref {:context :informal, :number :plur},
           :pred :you},
          :mod [],
          :pred :sleep,
          :aspect :simple,
          :tense :present})
       (->> "you 👦🧒🏼 sleep" en/parse (map #(u/get-in % [:sem])) (map l/pprint)))))



