(ns babel.italiano.benchmark
  (:refer-clojure :exclude [get-in])
  (:require [babel.italiano.grammar :refer [small medium np-grammar]]
            [babel.italiano.lexicon :refer [lexicon]]
            [babel.italiano.morphology :as morph :refer [analyze-regular fo replace-patterns]]
            [babel.italiano.morphology.nouns :as nouns]
            [babel.italiano.morphology.verbs :as verbs]
            [babel.italiano.workbook :refer [analyze generate generate-all parse]]
            [babel.parse :as parse]
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log])
            [clojure.string :as string]
            [dag_unify.core :refer [get-in strip-refs]]))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. error-string)))
  #?(:cljs
     (throw (js/Error. error-string))))

(defn run-benchmark [times]
  (count (take (Integer/parseInt times)
               (repeatedly #(let [debug (println "starting generation..")
                                  expr (time (generate :top))]
                              (println (str "generated expression: " (fo expr)))
                              (println (str "starting parsing.."))
                              (let [parsed (time (take 1 (parse (fo expr))))]
                                (if (empty? parsed)
                                  (throw (exception (str "could not parse: " (fo expr)))))
                                (println (str "parsed: " (fo (first parsed))))
                                (println "")))))))
(defn -main [times]
  (run-benchmark times))



