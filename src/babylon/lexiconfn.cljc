(ns babylon.lexiconfn
  (:require-macros [babylon.io :refer [inline-resource]]))

(defn read-rules [rules-filename]
  (-> rules-filename
      #?(:cljs inline-resource)))
