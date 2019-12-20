(ns babylon.io
  (:require [clojure.java.io :as io]))

;; https://clojureverse.org/t/best-practices-for-importing-raw-text-files-into-clojurescript-projects/2569/2
(defmacro some-function [resource-path]
  (io/resource resource-path))

(defmacro some-other-function [x]
  (clojure.core/slurp x))

(defmacro load-lexicon-file [filename]
  (-> ,filename
      io/resource
      slurp
      read-string))
