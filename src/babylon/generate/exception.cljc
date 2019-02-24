(ns babylon.generate.exception
  (:gen-class :extends java.lang.Exception
   :init init
   :constructors {[clojure.lang.PersistentArrayMap] []}))

(defn -init
  [diagnostic-map]
  [[] diagnostic-map])
