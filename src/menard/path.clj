(ns menard.path)

(defn use-path [path]
  (if (System/getenv "MODEL_URL")
    (str (System/getenv "MODEL_URL") path)
    (str "" path)))

