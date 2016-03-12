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

(defn run-benchmark []
  (repeatedly #(let [debug (println "starting generation")
                     expr (time (generate :top))]
                 (println (str "generated expression: " (fo expr)))
                 (let [parsed (time (first (take 1 (parse (fo expr)))))]
                   (println (str "parsed: " (fo parsed)))
                   (println "")))))
(defn -main []
  (take 5 (run-benchmark)))

