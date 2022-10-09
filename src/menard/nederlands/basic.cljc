(ns menard.nederlands.basic
  (:require [dag_unify.core :as u]
            [clojure.tools.logging :as log]

            ;; This :require of menard.generate is needed
            ;; for some reason for the
            ;; menard.model/install-the-usual-suspects macro
            ;; to work: otherwise we get a:
            ;; 'Syntax error (ClassNotFoundException) compiling at (menard/nederlands/complete.cljc:12:1).'
            [menard.generate :as generate] 


            [menard.nederlands.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]
            [menard.parse :as parse]))

(defn basic-filter
  "create a 'basic' lexicon that only contains all closed-class words, but
   only :basic open-class words"
  [lexicon]
  (->>
   (keys lexicon)
   (map (fn [k]
          (let [vals (get lexicon k)
                filtered-vals (->> vals
                                   (filter (fn [lexeme]
                                             (let [cat (u/get-in lexeme [:cat])
                                                   curriculum (u/get-in lexeme [:curriculum] ::none)]
                                               (or
                                                true
                                                (and (= cat :adjective)
                                                     (= :basic curriculum))
                                                (and (= cat :adverb)
                                                     (= :basic curriculum))
                                                (and (= cat :conjunction))
                                                (and (= cat :det))
                                                (and (= cat :exclamation))
                                                (and (= cat :intensifier))
                                                (and (= cat :misc))
                                                (or (and (= cat :noun)
                                                         (true? (u/get-in lexeme [:pronoun?]))))
                                                (or (and (= cat :noun)
                                                         (true? (u/get-in lexeme [:propernoun?]))))
                                                (or (and (= cat :noun)
                                                         (= :basic curriculum)))
                                                (and (= cat :numbers))
                                                (and (= cat :preposition))
                                                (and (= cat :verb)
                                                     (= :basic curriculum)))))))]
            (if (seq filtered-vals)
              {k filtered-vals}))))
   (into {})))

(def model
  (ref (create "nederlands/models/basic"
               "basic"
               compile-lexicon)))

(menard.model/install-the-usual-suspects model)
