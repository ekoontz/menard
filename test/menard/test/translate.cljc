(ns menard.test.translate
  (:require [menard.english :as en]
            [menard.nederlands :as nl]
            [menard.translate :refer [nl-to-en-spec]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn nl-to-en-str [nl-str]
  (->> nl-str nl/parse (take 1) first nl-to-en-spec en/generate en/morph))

(deftest nodig
  (is (= (nl-to-en-str "ik heb het geld nodig")
         "I need the money"))
  (is (= (nl-to-en-str "ze hebben het geld nodig")
         "they need the money")))

(deftest pronoun-nodig
  (is (= (nl-to-en-str "jij hebt hun nodig") "you 🤠 need them"))
  (is (= (nl-to-en-str "zij heeft zich nodig") "she needs herself")))

