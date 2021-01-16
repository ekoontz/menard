(ns menard.test.translate
  (:require [menard.english :as en]
            [menard.nederlands :as nl]
            [menard.translate :refer [nl-to-en-spec]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest nodig
  (is (=
       (->> "ik heb het geld nodig" nl/parse (take 1) first nl-to-en-spec en/generate en/morph)
       "I need the money"))
  (is (=
       (->> "ze hebben het geld nodig" nl/parse (take 1) first nl-to-en-spec en/generate en/morph)
       "they need the money")))
