(ns menard.log
  (:require [clojure.pprint :as pprint]
            [clojure.tools.logging :as log]))

(defn log-large-map [m & [log-fn label]]
  (let [log-fn (or log-fn (fn [x] (log/info x)))
        label (or label "untitled")
        writer (java.io.StringWriter.)]
    ;; TODO: add support to menard.lexiconfn/pprint to take a _writer_ like clojure.pprint/pprint does.
    (log-fn (str "<" label ">"))
    (pprint/pprint m writer)
    (doseq [line (clojure.string/split (str writer) #"\n")]
      (log-fn line))
    (log-fn (str "</" label ">"))))

