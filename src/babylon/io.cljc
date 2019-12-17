(ns babylon.io
  (:require #?(:clj [clojure.java.io :as io])))

;; https://clojureverse.org/t/best-practices-for-importing-raw-text-files-into-clojurescript-projects/2569/2
#?(:clj
   (defmacro inline-resource [resource-path]
      (io/resource resource-path))

   (defmacro slurp [x]
     (clojure.core/slurp x)))

