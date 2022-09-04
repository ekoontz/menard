(ns menard.test.english
  (:require [menard.english :as en :refer [analyze expressions generate load-model morph parse syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(load-model)
(deftest all-specifications-work
  "generate an expression for every specification in _specifications_,
   and then try to parse that expression."
  (let [specifications expressions ;; in menard.english and elsewhere
        ;; the specifications are called expressions, but that's a bit misleading:
        ;; the specifications instead specify how a generated expression should
        ;; look like. So we'll use that meaning of the terms here.
        expressions
        (->>
         (range 0 (count specifications))
         (pmap (fn [index]
                 (first
                  (->> (repeatedly #(generate (nth specifications index)))
                       (take 3) ;; if generation fails the first time, retry once.
                       (filter #(not (nil? %))))))))]
    (is (empty? (filter empty? expressions)))
    (is (empty? (filter empty? (map (fn [expression]
                                      (log/info (str "parsing generated expression: '" (morph expression) "'"))
                                      (-> expression
                                          morph
                                          parse))
                                    expressions))))))
