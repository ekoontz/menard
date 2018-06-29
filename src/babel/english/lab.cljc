(ns babel.english.lab
  (:require
   [babel.directory :refer [models]] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [frontier generate get-lexemes]]
   [babel.english :as english :refer [model morph morph-ps]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [dag_unify.core :as u :refer [pprint strip-refs unify]]))

(defn parse [surface-string]
  (english/parse surface-string model false))

(defn downtown []
  (let [specs
        [{:synsem {:cat :verb
                   :subcat []
                   :sem {:aspect :simple
                         :tense :present}}}]]
    (repeatedly #(println
                  (let [spec (first (shuffle specs))]
                    (morph (generate
                            spec
                            model)
                           :show-notes false))))))
(defn basecamp []
  (let [specs
        [

         {:synsem {:cat :verb
                   :sem {:pred :give-x-to-y
                         :reflexive false
                         :obj {:pred :cat
                               :mod {:first {:pred :black}}}}
                   :subcat []}}

         {:head {:comp {:synsem {:cat :prep}}}
          :synsem {:cat :verb
                   :sem {:pred :give-x-to-y
                         :reflexive false
                         :obj {:pred :book
                               :mod {:first {:pred :red}}}}
                   :subcat []}}
         
         {:synsem {:cat :verb
                   :sem {:pred :give-x-to-y
                         :reflexive true}
                   :subcat []}}]]
    (repeatedly #(println
                  (let [spec (first (shuffle specs))]
                    (morph (time (generate spec model))
                           :show-notes false))))))

(defn nextcamp []
  (let [parse (-> "the small dogs you see" parse first)]
    (pprint (u/get-in parse [:synsem :sem]))))

(defn refresh [& refresh-lexicon?]
  (let [refresh-lexicon? (or refresh-lexicon? false)]
    (babel.test.test/init-db)
    (if refresh-lexicon? (babel.lexiconfn/write-lexicon "en" (babel.english.grammar/compile-lexicon)))
    (babel.directory/refresh-models)
    (load "../english")))

(defn get-rule [rule]
  (-> model :grammar-map (get rule)))

(defn parse-at [expression path]
  (map #(u/get-in % path ::none)
       (parse expression)))

(defn lexeme-at [lexeme path]
  (map #(u/get-in % path ::none)
       (-> model :lexicon (get lexeme))))

(defn generate-at [spec path]
  (let [generated (generate spec model)]
    (u/get-in generated path ::none)))

