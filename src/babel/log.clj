;; thanks to http://brownsofa.org/blog/2015/06/14/clojure-in-production-logging/
(ns babel.log
  (:import (org.apache.log4j Level
                             Logger
                             PropertyConfigurator)))
(defn log4j!
  "Reconfigures log4j from a log4j.properties file on the classpath"
  []
  (PropertyConfigurator/configure "/Users/ekoontz/babel/resources/log4j.properties"))

