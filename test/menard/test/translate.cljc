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

(defn es-to-en-str [es-str]
  (-> es-str es/parse first es-to-en-spec en/generate en/morph))

(deftest yo-quiero
  (is (= (nl-to-en-str "yo quiero")
         "I want")))
