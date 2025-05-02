(ns menard.test.español.direct-objects
  (:require [menard.español.curated-verbs :as es
             :refer [analyze generate morph parse syntax-tree]]
            [menard.lexiconfn :as l]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u :refer [unify]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

;; if you made changes to these, you can uncomment them to reload them
;; so that in turn the below model will be reloaded with these changes:
(load "../../../../src/menard/subcat")
(load "../../../../src/menard/español/tenses")

;; reload the model every time to help with debugging the model:
(load "../../../../src/menard/español/curated_verbs")

(def model es/model)

(deftest generate-yo-lo-vedo
  (doall
   (take 20 (repeatedly #(is (= "yo los veo"
                                (->
                                 {:root "ver"
                                  :subcat []
                                 :reflexive? false
                                  :head {:comp {:reflexive? false}}
                                  :sem {:subj {:pred :i}
                                        :tense :present
                                        :aspect :simple
                                        :obj {:pred :they
                                              :gender :masc}}
                                 :rule "s"}
                                 es/generate
                                 es/morph)))))))
