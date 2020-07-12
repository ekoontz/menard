(ns menard.test.english
  (:require [menard.english :as en :refer [analyze expressions generate load-model morph parse syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(load-model)
(deftest all-expressions-work
  "generate every expression in _expressions_ specification list, and then try to parse that expression."
  (let [expressions
        (->>
         (range 0 (count expressions))
         (pmap (fn [index]
                 (first
                  (->> (repeatedly #(generate (nth expressions index)))
                       (take 3) ;; if generation fails the first time, retry once.
                       (filter #(not (nil? %))))))))]
    (is (empty? (filter empty? expressions)))
    (is (empty? (filter empty? (map (fn [expression]
                                      (log/info (str "parsing generated expression: '" (morph expression) "'"))
                                      (-> expression
                                          morph
                                          parse))
                                    expressions))))))
