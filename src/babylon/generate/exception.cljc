(ns babylon.generate.exception
  (:gen-class :extends java.lang.Exception
   :init init
   :constructors {[clojure.lang.PersistentArrayMap] []}))

;; thanks to https://kotka.de/blog/2010/02/gen-class_how_it_works_and_how_to_use_it.html
;; for ongoing work on this.
(defn -init
  [diagnostic-map]
  [[] diagnostic-map])
