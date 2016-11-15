(ns babel.english.benchmark
  (:refer-clojure :exclude [get-in])
  (:require [babel.english :as english :refer [analyze generate morph parse]]
            [babel.english.grammar :as grammar]
            [babel.english.morphology :as morph :refer [fo]]
            [babel.parse :as parse]
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log])
            [clojure.repl :refer [doc]]
            [clojure.string :as string]
            [dag_unify.core :refer [get-in strip-refs]]))

;; Creating language models is expensive so we'll create them before running any benchmarks..
(def small (grammar/small))
(def medium (grammar/medium))

(defn parse-mark [times expr]
  (count (take (Integer. times)
               (repeatedly
                #(println (with-out-str
                            (time (mapcat :parses (parse expr)))))))))

(defn benchmark [spec model do-this-many]
  (let [do-this-many (Integer. do-this-many)
        timings
        (->>
         (take do-this-many
               (repeatedly 
                #(with-out-str (time (fo
                                      (let [generated
                                            (generate spec :model model)
                                            output (log/info (fo generated))]
                                        generated))))))
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

;; lein run -m babel.english.benchmark/gen-mark3 10
(defn gen-mark3 [do-this-many]
  (let [do-this-many (Integer. do-this-many)]
    (benchmark {:modified false
                :synsem {:cat :verb
                         :sem {:pred :be-called
                               :tense :present
                               :aspect :progressive}}}
               medium
               do-this-many)))

