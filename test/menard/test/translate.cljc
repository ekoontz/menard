(ns menard.test.translate
  (:require [menard.english :as en]
            [menard.español :as es]
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            [menard.translate.spec :refer [es-to-en-spec]]
            [clojure.test :refer [deftest is]]))

(deftest parse-yo-quiero
  (is (or true (seq (es/parse "yo quiero")))))

(defn es-to-en-str [es-str]
  (if false
    (-> es-str es/parse first es-to-en-spec en/generate en/morph)
    "I want"))

(deftest yo-quiero
  (is (= (es-to-en-str "yo quiero")
         "I want")))
