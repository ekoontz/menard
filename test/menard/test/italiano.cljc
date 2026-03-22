(ns menard.test.italiano
  (:require [menard.italiano :as it
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [menard.translate.it-en :refer [string-to-string]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; These are for convenience so you don't
;; have to reload these language model files every time you make a change:
;; you can just reload this file (menard.test.italiano.clj)
;; file and it will reload the changed language-model files.
;; If you're not changing the language model files, you can comment these out
;; to speed up loading time for this file (menard.test.italiano)
;;(load "../../../src/menard/subcat")
(load "../../../src/menard/italiano")

(def spec3 {:cat :verb
            :rule "s"
            :subcat []
            :root "abbandonare"
            :sem {:pred :abandon
                  :tense :present
                  :subj {:pred :i}}})

(deftest subject-agreement
  (count
   (take 10
         (repeatedly #(is (= "io abbandono"
                             (-> spec3
                                 generate
                                 ((fn [x]
                                    (log/info (str "syntax-tree: " (syntax-tree x)))
                                    x))
                                 morph
                                 ((fn [x]
                                    (log/info (str "morph: " x))
                                    x)))))))))

(def np-spec {:root "gatto", :sem {:quant :she}, :subcat [], :phrasal? true, :cat :noun})
(def np-spec-sing {:root "gatto",
                   :sem {:quant :she
                         :number :sing}
                   :subcat [],
                   :phrasal? true,
                   :cat :noun})

(def np-spec-plur {:root "gatto",
                   :sem {:quant :she
                         :number :plur}
                   :subcat [],
                   :phrasal? true,
                   :cat :noun})

;; (->> #(-> np-spec-plur generate morph println) repeatedly (take 100) count)

(deftest your-cats
  (is (= "your (👥) cats"
         (->> "i vostri gatti" string-to-string)))
  (is (= "your (👤) cats"
         (->> "i tuoi gatti" string-to-string))))



