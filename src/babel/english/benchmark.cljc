(ns babel.english.benchmark
  (:refer-clojure :exclude [get-in])
  (:require [babel.benchmark :refer [benchmark]]
            [babel.english :as english :refer [model morph parse]]
            [babel.english.grammar :as grammar]
            [babel.english.morphology :as morph :refer [fo analyze]]
            [babel.parse :as parse]
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log])
            [clojure.repl :refer [doc]]
            [clojure.string :as string]
            [dag_unify.core :refer [get-in strip-refs]]))

(defn parse-mark [times expr]
  (count (take (Integer. times)
               (repeatedly
                #(println (with-out-str
                            (time (mapcat :parses (parse expr)))))))))

;; lein run -m babel.english.benchmark/gen-mark3 10
(defn gen-mark3 [do-this-many]
  (let [do-this-many (Integer. do-this-many)
        spec {:modified false
              :synsem {:cat :verb
                       :sem {:pred :be-called
                             :tense :present
                             :aspect :simple}}}]
    (benchmark
     #((:morph model) (babel.generate/generate spec model))
     do-this-many)))
