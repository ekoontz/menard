(ns babel.italiano.morphology.misc)

(defn compile-morphology []
  (-> "babel/italiano/morphology/elisions.edn"
      clojure.java.io/resource
      slurp
      read-string))
