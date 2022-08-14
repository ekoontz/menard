(ns menard.english.compile
  (:require #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]
            [menard.lexiconfn :as l]))

(defn compile-lexicon [lexicon morphology-rules filter-fn]
  lexicon)
