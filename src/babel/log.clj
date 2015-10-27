;; thanks to http://brownsofa.org/blog/2015/06/14/clojure-in-production-logging/
(ns babel.log
  (:require [clojure.java.io :as io]
            [clojure.set :as set])
  (:import (org.apache.log4j Level
                             Logger
                             PropertyConfigurator)))
(defn reload-config!
  "Reconfigures log4j from a log4j.properties file on the classpath"
  []
  (PropertyConfigurator/configure "/Users/ekoontz/babel/resources/log4j.properties"))
