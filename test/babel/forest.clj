(ns italianverbs.test.forest
  (:refer-clojure :exclude [get-in resolve merge])
  (:require
   [clojure.core :as core]
   ;; have to exclude partition-by because of some namespace clash, not sure how else to fix
   [clojure.core.async :as async :exclude [partition-by]]
   [clojure.set :refer :all]
   [clojure.string :as string]
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]
   [italianverbs.cache :refer (build-lex-sch-cache get-comp-phrases-of get-head-phrases-of get-lex
                                                   get-parent-phrases-for-spec)]
   [italianverbs.forest :refer :all]
   [italianverbs.italiano :as it]
   [italianverbs.over :as over]
   [dag-unify.core :as unify]
   [dag-unify.core :refer (dissoc-paths get-in fail? lazy-shuffle remove-top-values-log show-spec unifyc)]))

;; TODO: this test brings in extra stuff (namely italiano) that is built upon forest, not forest itself.
;; make more illustrative tests of core forest functionality (e.g. lighting-bolt)
(deftest generate-test
  (let [generated (first (take 1 (it/generate
                                  {:synsem {:aux false 
                                            :cat :verb :subcat '() 
                                            :sem {:subj {:animate true}}}})))]
    (is (not (fail? generated)))
    (is (not (nil? generated)))))
