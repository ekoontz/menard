(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze generate grammar index-fn lexicon
                                   morph parse syntax-tree]]
   [babylon.generate :as g :refer [add lazy-map]]
   [dag_unify.core :as u]
   [clojure.tools.logging :as log]))

;; "X put Y on Z".
(def put-spec
  {:rule "s"
   :head {:rule "vp" :phrasal true
          :head {:rule "vp-below-vp"
                 :comp {:phrasal false}}}
   :subcat []})
