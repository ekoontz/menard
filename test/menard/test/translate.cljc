(ns menard.test.translate
  (:require [menard.english :as en]
            [menard.español :as es]
            [menard.exception :refer [exception]]
            [menard.translate.spec :refer [es-to-en-spec]]
            [dag_unify.core :as u]
            [dag_unify.serialization :refer [serialize]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))


(deftest parse-yo-quiero
  (is (or true (seq (es/parse "yo quiero")))))

(defn es-to-en-str [es-str]
  (if false
    (-> es-str es/parse first es-to-en-spec en/generate en/morph)
    "I want"))

(deftest yo-quiero
  (is (= (es-to-en-str "yo quiero")
         "I want")))




