(ns menard.log)

(defn fmt [msgs]
  (apply str (interpose " " (map pr-str msgs))))

(defn debug [& s]
  (let [msg (fmt s)]
    (js/console.debug msg)))

(defn error [& s]
  (let [msg (fmt s)]
    (js/console.error msg)))

(defn info [& s]
  (let [msg (fmt s)]
    (js/console.info msg)))

(defn warn [& s]
  (let [msg (fmt s)]
    (js/console.warn msg)))
