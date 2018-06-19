(ns babel.italiano.morphology.misc)

(defn compile-morphology []
  (-> "babel/italiano/morphology/misc.edn"
      clojure.java.io/resource
      slurp
      read-string))
