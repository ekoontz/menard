(ns menard.test.english
  (:require [menard.english :as en :refer [analyze expressions generate morph parse syntax-tree get-grammar get-lexicon]]
            [dag_unify.core :as u]
            [menard :as m]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [menard.lexiconfn :refer [display-derivation]]
            [menard.english.woordenlijst :as woordenlijst]))

(deftest all-specifications-work
  "generate an expression for every specification in _specifications_,
   and then try to parse that expression."
  (let [do-this-many-per-spec 3
        specifications expressions ;; in menard.english and elsewhere
        ;; the specifications are called expressions, but that's a bit misleading:
        ;; the specifications instead specify how a generated expression should
        ;; look like. So we'll use that meaning of the terms here.
        ]
    (->>
     (range 0 (count specifications))
     (mapv (fn [index]
             (let [spec (nth specifications index)
                   generated (->> (repeatedly #(generate spec))
                                  (filter #(not (nil? %)))
                                  (take do-this-many-per-spec))
                   truncate? true]
               (log/debug (str "generated from spec: " index ":"
                              (clojure.string/join ","
                                                   (mapv syntax-tree generated))))
               (is (= do-this-many-per-spec (count generated)))
               (->> generated
                    (map morph)
                    (mapcat parse)
                    (mapv (fn [each-parse]
                            (log/debug (str "each-parse: " (syntax-tree each-parse)))
                            (is (not (empty? each-parse))))))))))))

(deftest woordenlijst-generate
  (let [spec {:rule "np:2"
              :cat :noun
              :agr {:number :sing}
              :max-depth 1
              :sem {:pred "trousers"}
              :phrasal? true
              :subcat []}]
    (is (not (empty?
              (->
               spec
               ((fn [spec]
                  (let [generated-expression
                        (generate spec woordenlijst/model)]
                    (log/info (str "generated: " (morph generated-expression)))
                    generated-expression)))
               ((fn [x] (str (morph x) ":" (dag_unify.diagnostics/strip-refs (u/get-in x [:sem :pred])))))))))))

