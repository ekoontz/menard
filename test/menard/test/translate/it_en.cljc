(ns menard.test.translate.it-en
    (:require [menard.italiano :as it
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.translate.it-en :refer [string-to-en-structure string-to-string]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest your-cats-plural
  (is (= "your (👥) cats"
         (->> "i vostri gatti" string-to-string))))

(deftest your-cats-singular
  (is (= "your (👤) cats"
         (->> "i tuoi gatti" string-to-string))))

(deftest generate-verb-sentence
  (let [spec {:root "trovare",
              :sem {:tense :present,
                    :aspect :simple
                    :subj {:pred :i}}
              :cat :verb
              :phrasal? true
              :subcat []}]
    (is (= "io trovo" (-> spec it/generate it/morph)))))
