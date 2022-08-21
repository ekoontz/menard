(ns menard.log
  (:refer-clojure :exclude [time])
  (:require [goog.log :as glog])
  (:import goog.debug.Console))

(defn fmt [msgs]
  (apply str (interpose " " (map pr-str msgs))))

(defn debug [& s]
  (let [msg (fmt s)]
    (glog/fine logger msg)))

(defn error [& s]
  (let [msg (fmt s)]
    (glog/error logger msg)))

(defn info [& s]
  (let [msg (fmt s)]
    (glog/info logger msg)))
