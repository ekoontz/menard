(ns babel.italiano.pos
  (:require
   [babel.lexiconfn :as lexiconfn]
   [babel.pos :as pos]
   [dag_unify.core :refer (unifyc)]))

(def countable-noun pos/countable-noun)
(def determiner pos/determiner)
(def drinkable-noun pos/drinkable-noun)
(def noun pos/noun)
(def pronoun-acc pos/pronoun-acc)
