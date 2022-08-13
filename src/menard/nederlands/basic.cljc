(ns menard.nederlands.basic
  (:require [dag_unify.core :as u]
            [menard.nederlands :as nl]
            [menard.model :refer [create]]))

(def create-basic-model? true)

(defn basic-filter
  "create a 'basic' lexicon that only contains closed-class words and :basic open-class words"
  [lexicon]
  (->>
   (map (fn [k]
          (let [vals (get lexicon k)
                filtered-vals (->> vals
                                   (filter (fn [lexeme]
                                             (let [cat (u/get-in lexeme [:cat])
                                                   curriculum (u/get-in lexeme [:curriculum] ::none)]
                                               (or
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
              {k filtered-vals})))
        (keys lexicon))
     (into {})))
