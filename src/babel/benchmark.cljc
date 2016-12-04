(ns babel.benchmark
  (:require [babel.generate :refer [generate]]
            [clojure.string :as string]
            [clojure.tools.logging :as log]))

(defn benchmark [function-to-run do-this-many]
  (let [do-this-many (Integer. do-this-many)
        timings
        (->>
         (take do-this-many
               (repeatedly 
                #(with-out-str (time (log/info
                                      (str (function-to-run)))))))
         (map #(string/replace % #".*time:\s*([0-9.]+).*" "$1"))
         (map string/trim)
         (map #(Double. %))
         ;; remove first run time - it's an outlier because of startup
         ;; and language model-loading.
         (#(if (> (count %) 1) (rest %) %))
         (sort))]
    (let [result
          {:mean (/ (reduce + timings) (count timings))
           :median (nth timings (int (Math/floor (/ (count timings) 2))))
           :.75 (nth timings (int (Math/floor (* (count timings) 0.75))))
           :.95 (nth timings (int (Math/floor (* (count timings) 0.95))))
           :times timings
           }]
      (println result)
      result)))


