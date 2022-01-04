(ns menard.reflexives
  (:require #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [menard.lexiconfn :refer [read-and-eval]]
            [menard.model :refer [use-path]]))

(def reflexive-options
  (let [retval
        (->> (read-and-eval (use-path "reflexives.edn"))
             (remove nil?))]
    (log/info (str (count retval) " reflexive options loaded."))
    retval))
